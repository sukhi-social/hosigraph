defmodule Ecto.Adapters.Hosi do
  @moduledoc """
  An Ecto adapter for [hosigraph](https://github.com/sukhi-social/hosigraph): an append-only event
  log with a Datalog database (Datalevin) folded from it.

  ## Setup

      defmodule MyApp.Repo do
        use Ecto.Repo, otp_app: :my_app, adapter: Ecto.Adapters.Hosi
      end

      config :my_app, MyApp.Repo,
        dir: "/var/lib/my_app/hosi",
        schemas: [MyApp.User, MyApp.Post]     # the schemas kept in hosigraph

  A schema is an ordinary Ecto schema with an integer `:id`, which is the hosigraph entity id.
  Rows are facts: a user's name is the fact `[user-id :"user/name" "Ada"]`.

  ## What works as in Ecto

  `insert`, `update` (with `optimistic_lock`), `delete`, `insert_all`, `get`, `get_by`, `all`,
  `one`, `exists?`, `aggregate(:count)`, `preload`, and `unique_constraint` in changesets.
  Queries may use one schema, `where` (`== != < <= > >= in is_nil not and or`), `order_by`,
  `limit`, `offset` and `select` of fields. Everything else raises with a pointer to
  `c:datalog/2`.

  ## What is different

    * **`not` is "there is no such fact".** `where: not (u.age > 30)` also finds rows that have no
      age at all; SQL would skip them because `NULL > 30` is unknown. `!=`, `<` and the rest need
      the fact to exist, as in SQL.
    * **No transactions.** hosigraph writes one batch atomically; Ecto's step-by-step
      transactions do not fit it. Use `c:write_batch/2` for all-or-nothing writes of several rows.
    * `Repo.insert` of a changeset with associations writes parent and children one after
      the other (not atomically).
    * `stream` is not supported.
    * No migrations: the hosigraph schema is derived from your Ecto schemas when the Repo starts.
    * **Only the Repo's `:schemas` write**, so that their types and rules apply.
    * **A delete removes every fact of a record**, so it is refused when the record also holds facts
      that are not the schema's.
    * **A write waits `timeout:`** (or the Repo's `:timeout`, 15 s). When that runs out the result is
      unknown: hosigraph goes on with the write.

  ## Functions added to your Repo

    * `datalog(query, opts \\\\ [])` – a Datalog query (`Ecto.Hosi.Query.datalog/1`), with rules;
      misspelt attributes and strings for keywords raise (`check: false` to skip that)
    * `history(struct_or_id)` – every event of one entity
    * `highest_in(schema, {key, order}, keys, limit, opts \\\\ [])` – the newest rows of some keys, by a
      `hosi_tuple key, order` index of the schema (one seek per key)
    * `highest(schema, field, limit, opts \\\\ [])` – newest-first pages, e.g. a timeline
    * `erase(erasures, opts \\\\ [])` – really remove (right to be forgotten)
    * `clean(opts \\\\ [])` – let what was erased leave the disk
    * `write_batch(steps, opts \\\\ [])` – several inserts/updates/deletes, all or nothing

  ## Pointing hosigraph at Datalevin

  hosigraph keeps "now" in Datalevin: the native `dtlv`, or a resident JVM with the standalone jar.
  Say which in `:open`:

      config :my_app, MyApp.Repo,
        dir: "/var/lib/my_app/hosi",
        schemas: [...],
        open: %{config: %{khana: [:dtlv, "/opt/datalevin/dtlv"]}}
        # or: open: %{config: %{khana: [:jvm, "java", "/opt/datalevin/datalevin-1.2.10-standalone.jar"]}}

  The adapter also sets the VM's `SIGCHLD` back to its default when it starts (a setting of the
  whole VM): the BEAM starts with it ignored, and the Haskell runtime inside it then cannot wait for
  the `dtlv`/`java` process it started ("waitForProcess: does not exist (No child processes)").

  ## Options

    * `:dir` – the hosigraph directory (also used by `mix ecto.create` / `ecto.drop`)
    * `:schemas` – Ecto schema modules kept here
    * `:open` – more options for opening hosigraph (merged with `dir` and `schema`)
    * `:hosi` – the module that talks to hosigraph (default `Hosi`; a test double works too)
    * `:timeout` – how long a write waits, in ms (default 15000)
    * `:precheck_unique` – default `true`: before writing a `hosi_unique` field, ask whether the value
      is taken, so the changeset error is exact. Set `false` to rely on hosigraph's own `unique`
      alone (one query less per write; the error is then worked out from hosigraph's message)
  """

  @behaviour Ecto.Adapter
  @behaviour Ecto.Adapter.Schema
  @behaviour Ecto.Adapter.Queryable
  @behaviour Ecto.Adapter.Storage
  @behaviour Ecto.Adapter.Transaction

  alias Ecto.Hosi.{Attr, Backend, Bulk, Conflict, Derived, Reader, Schema, Tx, Types}

  require Logger

  ## ------------------------------------------------------------ Ecto.Adapter

  @impl Ecto.Adapter
  defmacro __before_compile__(_env) do
    quote do
      use Ecto.Hosi.StaticMulti

      @doc """
      Run a Datalog query (see `Ecto.Hosi.Query.datalog/1`). Returns the rows, a set: in no
      particular order. A misspelt attribute, or a string where a keyword is kept, raises;
      `check: false` sends the query as it is.
      """
      def datalog(query, opts \\ []), do: Ecto.Hosi.RepoApi.datalog(__MODULE__, query, opts)

      @doc "Every event of one entity (a struct, or its integer id), oldest first."
      def history(struct_or_id), do: Ecto.Hosi.RepoApi.history(__MODULE__, struct_or_id)

      @doc """
      The facts of `field` from the high end: `{structs, last_place}`.
      Pass `after: last_place` for the next page.
      """
      def highest_in(schema, tuple, keys, limit, opts \\ []),
        do: Ecto.Hosi.RepoApi.highest_in(__MODULE__, schema, tuple, keys, limit, opts)

      def highest(schema, field, limit, opts \\ []),
        do: Ecto.Hosi.RepoApi.highest(__MODULE__, schema, field, limit, opts)

      @doc "Really remove entities, as hosigraph's `erase` does: `erase([[:ids, [id]]])`."
      def erase(erasures, opts \\ []), do: Ecto.Hosi.RepoApi.erase(__MODULE__, erasures, opts)

      @doc """
      Let what `erase` took out leave the disk, as hosigraph's `clean` does. Returns its report;
      while `:"records-left"` is above 0, call it again. Waits as long as it takes unless
      `timeout:` says otherwise; `max_chunks:` limits one call.
      """
      def clean(opts \\ []), do: Ecto.Hosi.RepoApi.clean(__MODULE__, opts)

      @doc """
      Several writes in one atomic batch:

          Repo.write_batch(
            insert: {:post, post_changeset},
            insert: {:comment, comment_changeset, refs: [post_id: :post]},
            update: {:author, author_changeset}
          )
          #=> {:ok, %{post: %Post{}, comment: %Comment{}, author: %Author{}}}
      """
      def write_batch(steps, opts \\ []), do: Ecto.Hosi.Batch.run(__MODULE__, steps, opts)
    end
  end

  @shared_meta [:backend, :conn, :attrs, :declared, :derived, :keywords, :refs, :append_only, :names, :schemas, :listed, :precheck_unique]

  @impl Ecto.Adapter
  def ensure_all_started(_config, _type), do: {:ok, []}

  @impl Ecto.Adapter
  def init(config) do
    case Keyword.get(config, :join) do
      nil ->
        init_own(config)

      other ->
        {:ok, child, meta} = join(other)
        {:ok, child, Map.put(meta, :timeout, Keyword.get(config, :timeout, 15_000))}
    end
  end

  # `join: OtherRepo` (or `{OtherRepo, dynamic_name}`): a second repo over the SAME hosigraph, in
  # the same BEAM. It starts nothing and shares the other repo's connection and rules (attributes,
  # derived keys, foreign keys), so its writes keep them in step. The other repo has to be started.
  defp join({_repo, name}) do
    child = %{id: {:joined, name}, start: {Task, :start_link, [fn -> :ok end]}, restart: :temporary}
    {:ok, child, name |> Ecto.Adapter.lookup_meta() |> Map.take(@shared_meta)}
  end

  defp join(repo) when is_atom(repo), do: join({repo, repo.get_dynamic_repo()})

  defp init_own(config) do
    repo = Keyword.fetch!(config, :repo)
    backend = Keyword.get(config, :hosi, Hosi)
    if backend == Hosi, do: wait_for_children!()
    schemas = Keyword.get(config, :schemas, [])
    attrs = Schema.build(schemas)
    derived = Ecto.Hosi.Derived.build(schemas)
    warn_undeclared(schemas)
    name = Keyword.get(config, :conn_name, Module.concat(repo, Conn))

    open =
      config
      |> Keyword.get(:open, %{})
      |> Map.new()
      |> then(fn open ->
        if config[:dir], do: Map.put_new(open, :dir, config[:dir]), else: open
      end)
      |> Map.update(:schema, attrs, &Map.merge(attrs, &1))

    child_spec = %{
      id: name,
      start: {Ecto.Hosi.Start, :start_link, [backend, [open: open, name: name], name]},
      type: :worker
    }

    meta = %{
      backend: backend,
      conn: name,
      attrs: attrs,
      derived: derived,
      declared: for({attr, spec} <- open.schema, is_atom(attr), into: %{}, do: {attr, spec}),
      keywords: Ecto.Hosi.Kw.build(schemas),
      refs: Ecto.Hosi.Cascade.build(schemas),
      append_only: Ecto.Hosi.Cascade.append_only(schemas),
      names: Ecto.Hosi.Derived.names(derived),
      schemas: Map.new(schemas, &{&1.__schema__(:source), &1}),
      listed: MapSet.new(schemas, &{&1.__schema__(:source), &1}),
      precheck_unique: Keyword.get(config, :precheck_unique, true),
      timeout: Keyword.get(config, :timeout, 15_000)
    }

    {:ok, child_spec, meta}
  end

  defp warn_undeclared(schemas) do
    for {mod, key, parent} <- Ecto.Hosi.Cascade.undeclared(schemas) do
      Logger.warning(
        "ecto_hosi: #{inspect(mod)}.#{key} belongs_to #{inspect(parent)} and has no delete rule, " <>
          "so deleting a #{inspect(parent)} leaves it pointing at nothing. In #{inspect(mod)} " <>
          "(use Ecto.Hosi.Schema) say what should happen: hosi_ref #{inspect(key)}, " <>
          "to: #{inspect(parent)}, on_delete: :cascade, :nilify, :restrict or :keep"
      )
    end
  end

  @impl Ecto.Adapter
  def checkout(_meta, _config, fun), do: fun.()

  @impl Ecto.Adapter
  def checked_out?(_meta), do: false

  @impl Ecto.Adapter
  def loaders(primitive, type), do: Types.loaders(primitive, type)

  @impl Ecto.Adapter
  def dumpers(primitive, type), do: Types.dumpers(primitive, type)

  ## ------------------------------------------------------------ Ecto.Adapter.Queryable

  @impl Ecto.Adapter.Queryable
  def prepare(op, query) when op in [:all, :update_all, :delete_all], do: {:nocache, {op, query}}

  @impl Ecto.Adapter.Queryable
  def execute(meta, _query_meta, {:nocache, {:all, query}}, params, _opts) do
    Reader.no_subquery!(query)
    Reader.all(meta, query, params)
  end

  def execute(meta, _query_meta, {:nocache, {:update_all, query}}, params, opts),
    do: meta |> writing(query, opts) |> Bulk.update_all(query, params)

  def execute(meta, _query_meta, {:nocache, {:delete_all, query}}, params, opts),
    do: meta |> writing(query, opts) |> Bulk.delete_all(query, params)

  # the meta for a query that writes, once the query may
  defp writing(meta, query, opts) do
    Reader.no_subquery!(query)
    with {source, schema} <- query.from.source, do: Schema.listed!(meta, source, schema)
    Backend.with_timeout(meta, opts)
  end

  @impl Ecto.Adapter.Queryable
  def stream(_meta, _query_meta, _query, _params, _opts) do
    raise ArgumentError,
          "Ecto.Adapters.Hosi does not support stream/2 (ecto_hosi's limit, not hosigraph's)"
  end

  ## ------------------------------------------------------------ Ecto.Adapter.Schema

  @impl Ecto.Adapter.Schema
  def autogenerate(:id), do: nil
  def autogenerate(:embed_id), do: Ecto.UUID.generate()
  def autogenerate(:binary_id), do: Ecto.UUID.bingenerate()

  @impl Ecto.Adapter.Schema
  def insert(meta, %{source: source} = into, fields, on_conflict, returning, opts) do
    Schema.listed!(meta, source, into[:schema])
    meta = Backend.with_timeout(meta, opts)
    Conflict.check!(on_conflict)
    attrs = field_attrs(source, fields)

    case Conflict.find(meta, source, attrs, on_conflict) do
      {:conflict, id} -> upsert_existing(meta, source, id, attrs, on_conflict, returning)
      :none ->
        Derived.check_new!(meta, source, attrs)
        insert_new(meta, source, Derived.extend(meta, source, nil, attrs), on_conflict, returning)
    end
  end

  defp upsert_existing(meta, source, id, attrs, on_conflict, returning) do
    if Conflict.nothing?(on_conflict) do
      {:ok, []}
    else
      entity = read_entity(meta, id)
      {ops, conds} = Conflict.ops(meta, source, id, entity, attrs, on_conflict)

      write_guarded(
        meta,
        ops,
        conds,
        fn -> returning_values(meta, source, id, returning) end,
        attrs
      )
    end
  end

  # `on_conflict: :nothing` treats a unique violation as "nothing to do".
  defp insert_new(meta, source, attrs, on_conflict, returning) do
    nothing? = Conflict.nothing?(on_conflict)

    case check_unique(meta, attrs, nil) do
      [] ->
        ops =
          [[:create, [:tmp, 1], Attr.marker(source), true]] ++
            for({attr, v} <- attrs, do: [:create, [:tmp, 1], attr, Backend.wrap(meta, attr, v)])

        case Backend.write(meta, ops) do
          {:ok, report} ->
            id = report |> Backend.temp_ids() |> Backend.temp_id(1)
            {:ok, returning_values(meta, source, id, returning)}

          {:error, {_, :"write-rejected", msg}} ->
            if nothing?, do: {:ok, []}, else: {:invalid, rejected(attrs, meta, msg)}

          {:error, reason} ->
            raise Ecto.Hosi.Error, reason: reason
        end

      violations ->
        if nothing?, do: {:ok, []}, else: {:invalid, violations}
    end
  end

  @impl Ecto.Adapter.Schema
  def insert_all(
        meta,
        %{source: source, schema: schema},
        _header,
        rows,
        on_conflict,
        returning,
        placeholders,
        opts
      ) do
    Schema.listed!(meta, source, schema)
    meta = Backend.with_timeout(meta, opts)
    Conflict.check!(on_conflict)

    unless placeholders in [nil, []] do
      raise ArgumentError,
            "Ecto.Adapters.Hosi does not support insert_all placeholders (ecto_hosi's limit)"
    end

    # `insert_all("table", rows)` has no schema, so Ecto dumped nothing: dump by the type of the
    # schema registered for that table (a DateTime, a map, a binary ...).
    rows = if is_nil(schema), do: Enum.map(rows, &dump_row(meta, source, &1)), else: rows
    rows_attrs = Enum.map(rows, &field_attrs(source, &1))

    for attrs <- rows_attrs, v <- Keyword.values(attrs), is_tuple(v) do
      raise ArgumentError,
            "Ecto.Adapters.Hosi insert_all takes plain values only (ecto_hosi's limit)"
    end

    if Conflict.raise?(on_conflict) do
      insert_all_plain(meta, source, rows_attrs, returning)
    else
      insert_all_conflict(meta, source, rows_attrs, on_conflict, returning)
    end
  end

  defp insert_all_plain(meta, source, rows_attrs, returning) do
    ops =
      rows_attrs
      |> Enum.with_index(1)
      |> Enum.flat_map(fn {attrs, n} -> create_ops(meta, source, attrs, n) end)

    if ops == [] do
      {0, nil}
    else
      case Backend.write(meta, ops) do
        {:ok, report} ->
          temps = Backend.temp_ids(report)
          ids = for n <- 1..length(rows_attrs), do: Backend.temp_id(temps, n)
          {length(ids), insert_all_returning(meta, source, ids, returning)}

        {:error, {_, :"write-rejected", msg}} ->
          raise Ecto.Hosi.Error, message: "insert_all was rejected by hosigraph: " <> msg

        {:error, reason} ->
          raise Ecto.Hosi.Error, reason: reason
      end
    end
  end

  defp dump_row(meta, source, row) do
    case Map.get(meta.schemas, source) do
      nil ->
        row

      schema ->
        fields = schema.__schema__(:fields)

        dumped =
          for {field, value} <- row do
            if field in fields and not is_nil(value), do: {field, dump(schema, field, value)}, else: {field, value}
          end

        dumped ++ missing_defaults(schema, dumped)
    end
  end

  defp dump(schema, field, value) do
    case Ecto.Type.adapter_dump(__MODULE__, schema.__schema__(:type, field), value) do
      {:ok, dumped} -> dumped
      :error -> Types.raw_dump(value)
    end
  rescue
    # not what the type wants (a time without microseconds for a _usec field): keep its meaning
    ArgumentError -> Types.raw_dump(value)
  end

  # The columns of a table written by name had database defaults (a counter at 0, a timestamp
  # of now): a field the row leaves out gets the schema's default, or its autogenerated value.
  defp missing_defaults(schema, row) do
    given = Keyword.keys(row)
    base = struct(schema)

    generated =
      for {fields, {m, f, a}} <- schema.__schema__(:autogenerate),
          field <- fields,
          field not in given,
          do: {field, dump(schema, field, apply(m, f, a))}

    defaults =
      for field <- schema.__schema__(:fields),
          field != :id,
          field not in given,
          field not in Keyword.keys(generated),
          not is_nil(v = Map.get(base, field)),
          do: {field, dump(schema, field, v)}

    generated ++ defaults
  end

  defp create_ops(meta, source, attrs, n) do
    Derived.check_new!(meta, source, attrs)
    attrs = Derived.extend(meta, source, nil, attrs)

    [[:create, [:tmp, n], Attr.marker(source), true]] ++
      for({attr, v} <- attrs, do: [:create, [:tmp, n], attr, Backend.wrap(meta, attr, v)])
  end

  # insert_all with on_conflict: each row is new, or collides with an existing row (the action
  # applies to it), or is skipped. One write for all of it.
  defp insert_all_conflict(meta, source, rows_attrs, on_conflict, returning) do
    nothing? = Conflict.nothing?(on_conflict)

    {actions, _seen} =
      Enum.map_reduce(rows_attrs, %{}, fn attrs, seen ->
        key = Conflict.key(source, attrs, on_conflict)

        if key && Map.has_key?(seen, key) do
          if nothing? do
            {:skip, seen}
          else
            raise ArgumentError,
                  "Ecto.Adapters.Hosi: on_conflict cannot affect a row a second time " <>
                    "(two rows of the insert_all have the same conflict_target; ecto_hosi's limit)"
          end
        else
          seen = if key, do: Map.put(seen, key, true), else: seen

          case Conflict.find(meta, source, attrs, on_conflict) do
            {:conflict, id} when nothing? -> {{:skip, id}, seen}
            {:conflict, id} -> {{:old, id, attrs}, seen}
            :none -> {planned_new(meta, source, attrs, on_conflict), seen}
          end
        end
      end)

    olds = for {:old, id, _} <- actions, do: id

    entities =
      if olds == [],
        do: %{},
        else: meta |> Backend.get_entities(olds) |> Backend.ok!() |> then(&Map.new(Enum.zip(olds, &1)))

    # `{ops, conds, id}` of each row that writes; joined once, as appending row by row would copy
    # the whole write for each of them
    {parts, _n} =
      Enum.flat_map_reduce(actions, 0, fn
        {:skip, _}, n ->
          {[], n}

        :skip, n ->
          {[], n}

        {:new, attrs}, n ->
          n = n + 1
          {[{create_ops(meta, source, attrs, n), [], {:new, n}}], n}

        {:old, id, attrs}, n ->
          {o, c} = Conflict.ops(meta, source, id, Map.fetch!(entities, id), attrs, on_conflict)
          {[{o, c, {:old, id}}], n}
      end)

    ops = Enum.flat_map(parts, &elem(&1, 0))
    conds = Enum.flat_map(parts, &elem(&1, 1))
    ids = Enum.map(parts, &elem(&1, 2))

    if ids == [] do
      {0, nil}
    else
      opts = if conds == [], do: [], else: [when: conds]

      case Backend.write(meta, ops, opts) do
        {:ok, report} ->
          temps = Backend.temp_ids(report)

          real =
            Enum.map(ids, fn
              {:new, n} -> Backend.temp_id(temps, n)
              {:old, id} -> id
            end)

          {length(real), insert_all_returning(meta, source, real, returning)}

        {:error, {_, :conflict, _}} ->
          raise Ecto.Hosi.Error, message: "a row changed while insert_all ran; try again"

        {:error, {_, :"write-rejected", msg}} ->
          raise Ecto.Hosi.Error, message: "insert_all was rejected by hosigraph: " <> msg

        {:error, reason} ->
          raise Ecto.Hosi.Error, reason: reason
      end
    end
  end

  # A row with no target under `:nothing` is skipped when it would break a unique value.
  defp planned_new(meta, source, attrs, {:nothing, _, []}) do
    if check_unique(meta, Derived.extend(meta, source, nil, attrs), nil) == [],
      do: {:new, attrs},
      else: :skip
  end

  defp planned_new(_meta, _source, attrs, _on_conflict), do: {:new, attrs}

  @impl Ecto.Adapter.Schema
  def update(meta, %{source: source} = row, fields, filters, returning, opts) do
    Schema.listed!(meta, source, row[:schema])
    meta = Backend.with_timeout(meta, opts)
    Ecto.Hosi.Cascade.append_only!(meta, source)
    {id, locks} = split_filters(filters)
    marker = Attr.marker(source)
    attrs = field_attrs(source, fields, keep_nil: true)
    attrs = Derived.extend(meta, source, id, attrs, fn -> read_entity(meta, id) end)
    {nils, sets} = Enum.split_with(attrs, fn {_, v} -> is_nil(v) end)

    # Setting a field to nil retracts its current value, which has to be read first.
    old = if nils == [], do: %{}, else: read_entity(meta, id)

    deletes =
      for {attr, _} <- nils,
          v <- List.wrap(Map.get(old, attr)),
          do: {[:delete, id, attr, Backend.wrap(meta, attr, v)], [:holds, id, attr, v]}

    ops =
      for({attr, v} <- sets, do: [:create, id, attr, Backend.wrap(meta, attr, v)]) ++
        Enum.map(deletes, &elem(&1, 0))

    conds =
      [[:holds, id, marker, true]] ++
        Enum.map(deletes, &elem(&1, 1)) ++ lock_conditions(source, id, locks)

    case check_unique(meta, sets, id) do
      [] ->
        write_guarded(
          meta,
          ops,
          conds,
          fn -> returning_values(meta, source, id, returning) end,
          sets
        )

      violations ->
        {:invalid, violations}
    end
  end

  @impl Ecto.Adapter.Schema
  def delete(meta, %{source: source} = row, filters, _returning, opts) do
    Schema.listed!(meta, source, row[:schema])
    meta = Backend.with_timeout(meta, opts)
    {id, locks} = split_filters(filters)
    marker = Attr.marker(source)
    entity = read_entity(meta, id)

    Ecto.Hosi.Cascade.append_only!(meta, source)

    if Map.has_key?(entity, marker) do
      case Ecto.Hosi.Cascade.plan(meta, source, [{id, entity}]) do
        {:ok, ops, conds} ->
          write_guarded(meta, ops, conds ++ lock_conditions(source, id, locks), fn -> [] end, [])

        {:restrict, name} ->
          {:invalid, [foreign_key: name]}

        {:other_facts, found} ->
          raise Ecto.Hosi.Error,
            reason: {:other_facts, found},
            message: Ecto.Hosi.Cascade.other_facts_message(found)
      end
    else
      {:error, :stale}
    end
  end

  ## ------------------------------------------------------------ Ecto.Adapter.Transaction
  # Not SQL transactions; see Ecto.Hosi.Tx for what they are and are not.

  @impl Ecto.Adapter.Transaction
  def transaction(meta, opts, fun), do: meta |> Backend.with_timeout(opts) |> Tx.run(fun)

  @impl Ecto.Adapter.Transaction
  def in_transaction?(meta), do: Tx.active?(meta)

  @impl Ecto.Adapter.Transaction
  def rollback(meta, value), do: Tx.rollback(meta, value)

  ## ------------------------------------------------------------ Ecto.Adapter.Storage

  @impl Ecto.Adapter.Storage
  def storage_up(opts) do
    dir = Keyword.fetch!(opts, :dir)

    if File.dir?(dir) do
      {:error, :already_up}
    else
      case File.mkdir_p(dir) do
        :ok -> :ok
        {:error, reason} -> {:error, :file.format_error(reason) |> to_string()}
      end
    end
  end

  @impl Ecto.Adapter.Storage
  def storage_down(opts) do
    dir = Keyword.fetch!(opts, :dir)

    if File.dir?(dir) do
      case File.rm_rf(dir) do
        {:ok, _} -> :ok
        {:error, reason, _} -> {:error, :file.format_error(reason) |> to_string()}
      end
    else
      {:error, :already_down}
    end
  end

  @impl Ecto.Adapter.Storage
  def storage_status(opts), do: if(File.dir?(Keyword.fetch!(opts, :dir)), do: :up, else: :down)

  ## ------------------------------------------------------------ helpers

  # The BEAM starts with SIGCHLD ignored, so the kernel reaps every child the moment it exits.
  # hosigraph's Haskell runtime lives inside the BEAM and starts `dtlv` (or java) itself; when
  # it then waits for that child it gets ECHILD ("waitForProcess: does not exist (No child
  # processes)"). Restoring the default makes the wait work. It is a setting of the whole VM.
  defp wait_for_children! do
    :os.set_signal(:sigchld, :default)
  end

  # [{field, value}] -> [{attr, value}] (nil values are not facts, except when retracting)
  defp field_attrs(source, fields, opts \\ []) do
    keep_nil? = Keyword.get(opts, :keep_nil, false)

    for {field, value} <- fields, keep_nil? or not is_nil(value) do
      if field == :id do
        raise ArgumentError,
              "Ecto.Adapters.Hosi cannot insert or change an id: hosigraph makes them " <>
                "(ecto_hosi's limit)"
      end

      {Attr.attr(source, field), value}
    end
  end

  defp split_filters(filters) do
    {id_pair, locks} = Enum.split_with(filters, fn {f, _} -> f == :id end)

    case id_pair do
      [{:id, id}] when is_integer(id) ->
        {id, locks}

      _ ->
        raise ArgumentError,
              "Ecto.Adapters.Hosi needs the integer :id to update or delete a row " <>
                "(hosigraph's ids are integers)"
    end
  end

  defp lock_conditions(source, id, locks) do
    for {field, v} <- locks do
      attr = Attr.attr(source, field)
      if is_nil(v), do: [:unset, id, attr], else: [:holds, id, attr, v]
    end
  end

  defp write_guarded(meta, ops, conds, on_ok, attrs) do
    if ops == [] do
      # nothing to change: just see that the row still exists
      case Backend.get_entities(meta, [conds |> hd() |> Enum.at(1)]) |> Backend.ok!() do
        [entity] when map_size(entity) > 0 -> {:ok, on_ok.()}
        _ -> {:error, :stale}
      end
    else
      case Backend.write(meta, ops, when: conds) do
        {:ok, _} -> {:ok, on_ok.()}
        {:error, {_, :conflict, _}} -> {:error, :stale}
        {:error, {_, :"write-rejected", msg}} -> {:invalid, rejected(attrs, meta, msg)}
        {:error, reason} -> raise Ecto.Hosi.Error, reason: reason
      end
    end
  end

  defp read_entity(meta, id) do
    case Backend.get_entities(meta, [id]) |> Backend.ok!() do
      [entity] -> entity
      _ -> %{}
    end
  end

  # The values Ecto asked back (`:id`, or fields it wants read after the write).
  defp returning_values(_meta, _source, id, [:id]), do: [id: id]
  defp returning_values(_meta, _source, _id, []), do: []

  defp returning_values(meta, source, id, returning) do
    entity = Backend.read_back(fn -> read_entity(meta, id) end)

    for field <- returning do
      if field == :id,
        do: {:id, id},
        else: {field, Backend.first(entity, Attr.attr(source, field))}
    end
  end

  defp insert_all_returning(_meta, _source, _ids, []), do: nil
  defp insert_all_returning(_meta, _source, ids, [:id]), do: Enum.map(ids, &[&1])

  defp insert_all_returning(meta, source, ids, returning) do
    entities = Backend.read_back(fn -> Backend.get_entities(meta, ids) end) |> Backend.ok!()

    for {id, entity} <- Enum.zip(ids, entities) do
      for field <- returning do
        if field == :id, do: id, else: Backend.first(entity, Attr.attr(source, field))
      end
    end
  end

  ## ---- unique constraints
  #
  # Before writing, ask whether a unique value is already taken: that answers precisely, with the
  # constraint name `unique_constraint/3` expects. hosigraph's own `unique` stays the guard against
  # races; if it still refuses, `rejected/3` works out which attribute from the message.

  defp check_unique(%{precheck_unique: false}, _attrs, _self_id), do: []

  defp check_unique(meta, attrs, self_id) do
    plain =
      for {attr, v} <- attrs,
          match?(%{^attr => %{unique: _}}, meta.attrs),
          taken?(meta, attr, v, self_id) do
        {:unique, cname(meta, attr)}
      end

    plain ++ tuple_violations(meta, attrs, self_id)
  end

  # A `hosi_unique [...], tuple: true` rule is hosigraph's own tuple: ask for the pair as one value.
  # The parts a write does not touch are read from the row (an update); a part that is not known
  # (a ref to a row of the same batch) leaves the question to hosigraph's refusal.
  defp tuple_violations(meta, attrs, self_id) do
    for spec <- native_rules(meta, attrs),
        vals = tuple_values(meta, spec, attrs, self_id),
        vals != nil,
        taken?(meta, spec.attr, vals, self_id) do
      {:unique, cname(meta, spec.attr)}
    end
  end

  # the `tuple: true` rules of the row these attributes belong to, that the write touches
  defp native_rules(meta, attrs) do
    sources = attrs |> Enum.map(fn {a, _} -> a |> Attr.split() |> elem(0) end) |> Enum.uniq()

    for source <- sources,
        spec <- Map.get(meta.derived, source, []),
        spec.native,
        Enum.any?(spec.attrs, &List.keymember?(attrs, &1, 0)),
        do: spec
  end

  defp tuple_values(meta, spec, attrs, self_id) do
    given = Map.new(attrs)

    entity =
      if self_id && Enum.any?(spec.attrs, &(not Map.has_key?(given, &1))),
        do: read_entity(meta, self_id),
        else: %{}

    vals = Enum.map(spec.attrs, fn a -> if Map.has_key?(given, a), do: given[a], else: Backend.first(entity, a) end)
    if Enum.any?(vals, &is_nil/1), do: nil, else: vals
  end

  defp cname(meta, attr), do: Map.get(meta.names, attr) || Attr.constraint_name(attr)

  defp taken?(meta, attr, value, self_id) do
    value = Ecto.Hosi.Kw.to_store(Backend.keywords(meta), attr, value)
    req = %{q: "[:find ?e :in $ ?v :where [?e #{Attr.kw(attr)} ?v]]", inputs: [[:scalar, value]]}
    rows = meta |> Backend.query(req) |> Backend.ok!()
    Enum.any?(rows, fn [e] -> e != self_id end)
  end

  defp rejected(attrs, meta, message) do
    uniques = for {attr, _} <- attrs, match?(%{^attr => %{unique: _}}, meta.attrs), do: attr
    # a tuple the khana keeps is never in the write, but it is what hosigraph names when it refuses
    uniques = uniques ++ for(spec <- native_rules(meta, attrs), do: spec.attr)
    named = Enum.filter(uniques, &mentions?(message, &1))

    case {named, uniques} do
      {[_ | _], _} -> Enum.map(named, &{:unique, cname(meta, &1)})
      {[], [one]} -> [unique: cname(meta, one)]
      _ -> raise Ecto.Hosi.Error, message: "hosigraph refused the write: " <> message
    end
  end

  # does the message name this attribute, as `:user/email`, `:"user/email"` or plain `user/email`?
  defp mentions?(message, attr) do
    Regex.match?(~r/(?<![\w\/\-])#{Regex.escape(Atom.to_string(attr))}(?![\w\/\-])/, message)
  end

  @doc false
  def __check_unique__(meta, attrs, self_id), do: check_unique(meta, attrs, self_id)
end
