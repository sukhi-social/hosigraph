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
        schemas: [MyApp.User, MyApp.Note],     # the views
        khana: [jvm: "/opt/datalevin/datalevin-1.2.10-standalone.jar"]

  ## Facts, not tables

  hosigraph keeps facts, `[record attribute value]`: a user's name is `[17 :name "Ada"]`. It has
  no tables, and neither does the adapter. A schema is a **view** of facts (`Ecto.Hosi.View`): it
  says which facts make a record show up, and which attributes its fields read and write. One
  record can show up in several views. Attributes that views share are declared once in a
  vocabulary (`Ecto.Hosi.Vocabulary`), and rules belong to attributes.

  The integer `:id` of a view is hosigraph's id of the record.

  ## What works as in Ecto

  `insert`, `update` (with `optimistic_lock`), `delete`, `insert_all`, `update_all`,
  `delete_all`, `get`, `get_by`, `all`, `one`, `exists?`, `aggregate`, `preload`, joins,
  `group_by`, and `unique_constraint` in changesets. What Ecto queries cannot say, Datalog can:
  `c:datalog/2`.

  ## What is different

    * **A record shows up in a view because of its facts.** An update through any view adds,
      changes or removes facts; a view of a record type changes only records of that type.
    * **`delete` says what it removes.** A record with a record type is deleted through a view of
      that type, and leaves the type's tombstone (`hosi_tombstone`) if there is one. A record with
      none is removed only if the view shows all its facts. Anything else is refused.
    * **`not` is "there is no such fact".** `where: not (u.age > 30)` also finds records that have
      no age at all; SQL would skip them because `NULL > 30` is unknown. `!=`, `<` and the rest need
      the fact to exist, as in SQL.
    * **No transactions.** hosigraph writes one batch atomically; Ecto's step-by-step
      transactions do not fit it. Use `c:write_batch/2` for all-or-nothing writes of several records.
    * `Repo.insert` of a changeset with associations writes parent and children one after
      the other (not atomically).
    * `from "name"` and `insert_all("name", ..)` are not offered: there is nothing a name would
      name. Use a view, or Datalog.
    * `stream` is not supported.
    * No migrations: the hosigraph schema is derived from your views when the Repo starts.
    * **Only the Repo's `:schemas`** are read and written.
    * **A write waits `timeout:`** (or the Repo's `:timeout`, 15 s). When that runs out the result is
      unknown: hosigraph goes on with the write.

  ## Functions added to your Repo

    * `datalog(query, opts \\\\ [])` – a Datalog query (`Ecto.Hosi.Query.datalog/1` or a request),
      with rules; `load: View` answers structs. Misspelt attributes and strings for keywords or
      UUIDs raise (`check: false` to skip that)
    * `explain(queryable, opts \\\\ [])` – the Datalog an Ecto query becomes, and what runs in Elixir
    * `facts(struct_or_id)` – the facts of one record
    * `history(struct_or_id)` – its past: by field for a struct, every event for an id
    * `highest(attribute, limit, opts \\\\ [])` – newest-first pages over one attribute
    * `highest_in(view, {key, order}, keys, limit, opts \\\\ [])` – the newest records of some keys,
      by a `hosi_tuple key, order` index
    * `write_batch(steps, opts \\\\ [])` – several inserts, updates and deletes, all or nothing
    * `write_facts(ops, opts \\\\ [])` – facts written directly, with the rules of the attributes
    * `erase(erasures, opts \\\\ [])` – really remove (right to be forgotten)
    * `erase_account(struct, opts \\\\ [])` – erase what an account owns, and leave its tombstone
    * `clean(opts \\\\ [])` – let what was erased leave the disk

  ## Pointing hosigraph at Datalevin

  hosigraph keeps "now" in Datalevin: a resident JVM with the standalone jar, or the native `dtlv`.

      khana: [jvm: "/opt/datalevin/datalevin-1.2.10-standalone.jar"]   # `java` from the PATH
      khana: [jvm: "/path/datalevin.jar", java: "/usr/lib/jvm/bin/java"]
      khana: [dtlv: "/opt/datalevin/dtlv"]

  The adapter also sets the VM's `SIGCHLD` back to its default when it starts (a setting of the
  whole VM): the BEAM starts with it ignored, and the Haskell runtime inside it then cannot wait for
  the `dtlv`/`java` process it started ("waitForProcess: does not exist (No child processes)").

  `mix hosi.doctor` checks that the pieces are there.

  ## Options

    * `:dir` – the hosigraph directory (also used by `mix ecto.create` / `ecto.drop`)
    * `:schemas` – the views kept here
    * `:khana` – where "now" is kept (above)
    * `:open` – more options for opening hosigraph (merged with the rest)
    * `:hosi` – the module that talks to hosigraph (default `Hosi`; `Ecto.Hosi.Fake` for tests)
    * `:timeout` – how long a write waits, in ms (default 15000)
    * `:precheck_unique` – default `true`: before writing a unique value, ask whether it is taken,
      so the changeset error is exact. Set `false` to rely on hosigraph's own `unique` alone (one
      query less per write; the error is then worked out from hosigraph's message)
  """

  @behaviour Ecto.Adapter
  @behaviour Ecto.Adapter.Schema
  @behaviour Ecto.Adapter.Queryable
  @behaviour Ecto.Adapter.Storage
  @behaviour Ecto.Adapter.Transaction

  alias Ecto.Hosi.{Backend, Bulk, Change, Conflict, Config, Delete, Derived, Model, Reader, Store, Tx, Types}

  require Logger

  ## ------------------------------------------------------------ Ecto.Adapter

  @impl Ecto.Adapter
  defmacro __before_compile__(_env) do
    quote do
      use Ecto.Hosi.StaticMulti

      @doc """
      Run a Datalog query (see `Ecto.Hosi.Query.datalog/1`). Returns the rows, a set: in no
      particular order. `load: View`, for a query that finds one id, answers the view's structs;
      `load: [View, nil, ..]` puts structs in the columns that name a view. A row with a record
      its view does not show is left out. A misspelt attribute, or a string where a keyword or a
      UUID is kept, raises; `check: false` sends the query as it is.
      """
      def datalog(query, opts \\ []), do: Ecto.Hosi.RepoApi.datalog(__MODULE__, query, opts)

      @doc "The Datalog an Ecto query becomes, what runs in Elixir, and what it costs."
      def explain(queryable, opts \\ []), do: Ecto.Hosi.Explain.explain(__MODULE__, queryable, opts)

      @doc "The facts of one record (a struct, or its id), as hosigraph keeps them now."
      def facts(struct_or_id), do: Ecto.Hosi.RepoApi.facts(__MODULE__, struct_or_id)

      @doc """
      The past of one record, oldest first. For a struct, the changes of each field:
      `%{field => [%{seq: .., kind: :create | :delete, value: ..}]}` (an attribute that is not a
      field of the view is under its own name). For an integer id, every event.
      """
      def history(struct_or_id), do: Ecto.Hosi.RepoApi.history(__MODULE__, struct_or_id)

      @doc """
      The records with the largest values of an attribute, the largest first: `{places, last}`,
      each place `%{id: .., value: ..}`. Pass `after: last` for the next page. `load: View` answers
      the view's structs instead, and leaves out the records it does not show.
      """
      def highest(attribute, limit, opts \\ []),
        do: Ecto.Hosi.RepoApi.highest(__MODULE__, attribute, limit, opts)

      @doc """
      The newest records of some keys by the view's `hosi_tuple key, order` index, newest first:
      `[%{id: .., key: .., at: ..}]`. `after:` a place from an earlier call starts after it.
      """
      def highest_in(view, tuple, keys, limit, opts \\ []),
        do: Ecto.Hosi.RepoApi.highest_in(__MODULE__, view, tuple, keys, limit, opts)

      @doc "Really remove entities, as hosigraph's `erase` does: `erase([[:ids, [id]]])`."
      def erase(erasures, opts \\ []), do: Ecto.Hosi.RepoApi.erase(__MODULE__, erasures, opts)

      @doc """
      Erase an account: every record that has it as `hosi/owner`, then leave the account's own
      record as a tombstone. See `Ecto.Hosi.Account`.
      """
      def erase_account(account, opts \\ []), do: Ecto.Hosi.Account.erase(__MODULE__, account, opts)

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
            update: {:pin, Ecto.Changeset.change(%Pinned{}, pinned_at: now), record: :post},
            update: {:author, author_changeset}
          )
          #=> {:ok, %{post: %Post{}, comment: %Comment{}, pin: %Pinned{}, author: %Author{}}}
      """
      def write_batch(steps, opts \\ []), do: Ecto.Hosi.Batch.run(__MODULE__, steps, opts)

      @doc """
      Write facts directly, `[:create | :delete, id or [:tmp, n], attribute, value]`, with the
      rules of the attributes. See `Ecto.Hosi.Facts`.
      """
      def write_facts(ops, opts \\ []), do: Ecto.Hosi.Facts.write(__MODULE__, ops, opts)
    end
  end

  @shared_meta [:backend, :conn, :model, :declared, :precheck_unique]

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
  # the same BEAM. It starts nothing and shares the other repo's connection, views and rules, so
  # its writes keep them in step. The other repo has to be started.
  defp join({_repo, name}) do
    child = %{id: {:joined, name}, start: {Task, :start_link, [fn -> :ok end]}, restart: :temporary}
    {:ok, child, name |> Ecto.Adapter.lookup_meta() |> Map.take(@shared_meta)}
  end

  defp join(repo) when is_atom(repo), do: join({repo, repo.get_dynamic_repo()})

  defp init_own(config) do
    repo = Keyword.fetch!(config, :repo)
    backend = Keyword.get(config, :hosi, Hosi)
    if backend == Hosi, do: wait_for_children!()
    model = config |> Keyword.get(:schemas, []) |> Model.build()
    warn_undeclared(model)
    name = Keyword.get(config, :conn_name, Module.concat(repo, Conn))
    open = Config.open(config, model)

    child_spec = %{
      id: name,
      start: {Ecto.Hosi.Start, :start_link, [backend, [open: open, name: name], name]},
      type: :worker
    }

    meta = %{
      backend: backend,
      conn: name,
      model: model,
      declared: for({attr, spec} <- open.schema, is_atom(attr), into: %{}, do: {attr, spec}),
      precheck_unique: Keyword.get(config, :precheck_unique, true),
      timeout: Keyword.get(config, :timeout, 15_000)
    }

    {:ok, child_spec, meta}
  end

  defp warn_undeclared(model) do
    for {attr, holders} <- Model.undeclared(model) do
      fields = Enum.map(holders, &"#{inspect(&1)}.#{model.views[&1].field_of[attr]}")

      Logger.warning(
        "ecto_hosi: #{Model.kw(attr)} refers to a record and says nothing about a delete of it, so " <>
          "the reference is left pointing at nothing#{held(fields)}. Say what should happen: " <>
          "hosi_ref :field, on_delete: :cascade, :nilify, :restrict or :keep (or on_delete: in " <>
          "the vocabulary)"
      )
    end
  end

  defp held([]), do: ""
  defp held(fields), do: " (#{Enum.join(fields, ", ")})"

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
    Reader.no_subquery!(meta, query)
    Reader.all(meta, query, params)
  end

  def execute(meta, _query_meta, {:nocache, {:update_all, query}}, params, opts),
    do: meta |> writing(query, opts) |> Bulk.update_all(query, params)

  def execute(meta, _query_meta, {:nocache, {:delete_all, query}}, params, opts),
    do: meta |> writing(query, opts) |> Bulk.delete_all(query, params)

  # the meta for a query that writes, once the query may
  defp writing(meta, query, opts) do
    Reader.no_subquery!(meta, query)
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
  def insert(meta, into, fields, on_conflict, returning, opts) do
    view = view!(meta, into)
    meta = Backend.with_timeout(meta, opts)
    Conflict.check!(on_conflict)
    values = field_values(meta, fields)

    case Conflict.find(meta, view, values, on_conflict) do
      {:conflict, id} ->
        upsert_existing(meta, view, id, values, on_conflict, returning)

      {:elsewhere, id} ->
        if Conflict.nothing?(on_conflict),
          do: {:ok, []},
          else: Conflict.elsewhere!(view, id)

      :none ->
        insert_new(meta, view, Change.new(meta, view, values), on_conflict, returning)
    end
  end

  defp upsert_existing(meta, view, id, values, on_conflict, returning) do
    if Conflict.nothing?(on_conflict) do
      {:ok, []}
    else
      entity = Backend.read_one(meta, id)
      {ops, conds} = Conflict.ops(meta, view, id, entity, values, on_conflict)

      write_guarded(
        meta,
        view,
        ops,
        conds,
        fn -> returning_values(meta, view, id, returning) end,
        Map.new(values)
      )
    end
  end

  # `on_conflict: :nothing` treats a unique violation as "nothing to do".
  defp insert_new(meta, view, facts, on_conflict, returning) do
    nothing? = Conflict.nothing?(on_conflict)

    case __check_unique__(meta, view, facts, nil) do
      [] ->
        case Backend.write(meta, Change.create_ops(meta, [:tmp, 1], facts)) do
          {:ok, report} ->
            id = report |> Backend.temp_ids() |> Backend.temp_id(1)
            {:ok, returning_values(meta, view, id, returning)}

          {:error, {_, :"write-rejected", msg}} ->
            if nothing?, do: {:ok, []}, else: {:invalid, rejected(meta, view, facts, msg)}

          {:error, reason} ->
            raise Ecto.Hosi.Error, reason: reason
        end

      violations ->
        if nothing?, do: {:ok, []}, else: {:invalid, violations}
    end
  end

  @impl Ecto.Adapter.Schema
  def insert_all(meta, into, _header, rows, on_conflict, returning, placeholders, opts) do
    view = view!(meta, into)
    meta = Backend.with_timeout(meta, opts)
    Conflict.check!(on_conflict)

    unless placeholders in [nil, []] do
      raise ArgumentError,
            "Ecto.Adapters.Hosi does not support insert_all placeholders (ecto_hosi's limit)"
    end

    for row <- rows, {_, v} <- row, is_tuple(v) do
      raise ArgumentError,
            "Ecto.Adapters.Hosi insert_all takes plain values only (ecto_hosi's limit)"
    end

    rows = Enum.map(rows, &field_values(meta, &1))

    if Conflict.raise?(on_conflict) do
      insert_all_plain(meta, view, rows, returning)
    else
      insert_all_conflict(meta, view, rows, on_conflict, returning)
    end
  end

  defp insert_all_plain(meta, view, rows, returning) do
    ops =
      rows
      |> Enum.with_index(1)
      |> Enum.flat_map(fn {values, n} ->
        Change.create_ops(meta, [:tmp, n], Change.new(meta, view, values))
      end)

    if ops == [] do
      {0, nil}
    else
      case Backend.write(meta, ops) do
        {:ok, report} ->
          temps = Backend.temp_ids(report)
          ids = for n <- 1..length(rows), do: Backend.temp_id(temps, n)
          {length(ids), insert_all_returning(meta, view, ids, returning)}

        {:error, {_, :"write-rejected", msg}} ->
          raise Ecto.Hosi.Error, message: "insert_all was rejected by hosigraph: " <> msg

        {:error, reason} ->
          raise Ecto.Hosi.Error, reason: reason
      end
    end
  end

  # insert_all with on_conflict: each row is new, or collides with an existing record (the action
  # applies to it), or is skipped. One write for all of it.
  defp insert_all_conflict(meta, view, rows, on_conflict, returning) do
    nothing? = Conflict.nothing?(on_conflict)

    {actions, _seen} =
      Enum.map_reduce(rows, %{}, fn values, seen ->
        key = Conflict.key(view, values, on_conflict)

        if key && Map.has_key?(seen, key) do
          if nothing? do
            {:skip, seen}
          else
            raise ArgumentError,
                  "Ecto.Adapters.Hosi: on_conflict cannot affect a record a second time " <>
                    "(two rows of the insert_all have the same conflict_target; ecto_hosi's limit)"
          end
        else
          seen = if key, do: Map.put(seen, key, true), else: seen

          case Conflict.find(meta, view, values, on_conflict) do
            {kind, _} when kind in [:conflict, :elsewhere] and nothing? -> {:skip, seen}
            {:conflict, id} -> {{:old, id, values}, seen}
            {:elsewhere, id} -> Conflict.elsewhere!(view, id)
            :none -> {planned_new(meta, view, values, on_conflict), seen}
          end
        end
      end)

    olds = for {:old, id, _} <- actions, do: id
    entities = Map.new(Backend.read(meta, olds))

    # `{ops, conds, id}` of each row that writes; joined once, as appending row by row would copy
    # the whole write for each of them
    {parts, _n} =
      Enum.flat_map_reduce(actions, 0, fn
        :skip, n ->
          {[], n}

        {:new, facts}, n ->
          n = n + 1
          {[{Change.create_ops(meta, [:tmp, n], facts), [], {:new, n}}], n}

        {:old, id, values}, n ->
          {o, c} = Conflict.ops(meta, view, id, Map.fetch!(entities, id), values, on_conflict)
          {[{o, c, {:old, id}}], n}
      end)

    ops = Enum.flat_map(parts, &elem(&1, 0))
    conds = Enum.flat_map(parts, &elem(&1, 1))
    ids = Enum.map(parts, &elem(&1, 2))

    if ids == [] do
      {0, nil}
    else
      opts = if conds == [], do: [], else: [when: conds]

      # (nothing to write when every row leaves its record as it is)
      result = if ops == [], do: {:ok, %{}}, else: Backend.write(meta, ops, opts)

      case result do
        {:ok, report} ->
          temps = Backend.temp_ids(report)

          real =
            Enum.map(ids, fn
              {:new, n} -> Backend.temp_id(temps, n)
              {:old, id} -> id
            end)

          {length(real), insert_all_returning(meta, view, real, returning)}

        {:error, {_, :conflict, _}} ->
          raise Ecto.Hosi.Error, message: "a record changed while insert_all ran; try again"

        {:error, {_, :"write-rejected", msg}} ->
          raise Ecto.Hosi.Error, message: "insert_all was rejected by hosigraph: " <> msg

        {:error, reason} ->
          raise Ecto.Hosi.Error, reason: reason
      end
    end
  end

  # A row with no target under `:nothing` is skipped when it would break a unique value.
  defp planned_new(meta, view, values, {:nothing, _, []}) do
    facts = Change.new(meta, view, values)
    if __check_unique__(meta, view, facts, nil) == [], do: {:new, facts}, else: :skip
  end

  defp planned_new(meta, view, values, _on_conflict), do: {:new, Change.new(meta, view, values)}

  @impl Ecto.Adapter.Schema
  def update(meta, row, fields, filters, returning, opts) do
    view = view!(meta, row)
    meta = Backend.with_timeout(meta, opts)
    {id, locks} = split_filters(filters)
    changes = field_values(meta, fields, keep_nil: true)
    entity = Backend.read_one(meta, id)

    if Change.reachable?(view, entity) do
      {ops, conds} = Change.update(meta, id, entity, changes)
      sets = Change.sets(meta, changes, entity)

      conds = Change.existence(meta, view, id, entity) ++ conds ++ lock_conditions(id, locks)

      case __check_unique__(meta, view, sets, id, entity) do
        [] ->
          write_guarded(
            meta,
            view,
            ops,
            conds,
            fn -> returning_values(meta, view, id, returning) end,
            sets
          )

        violations ->
          {:invalid, violations}
      end
    else
      {:error, :stale}
    end
  end

  @impl Ecto.Adapter.Schema
  def delete(meta, row, filters, _returning, opts) do
    view = view!(meta, row)
    meta = Backend.with_timeout(meta, opts)
    {id, locks} = split_filters(filters)
    entity = Backend.read_one(meta, id)

    if Model.member?(view, entity) do
      case Delete.plan(meta, view, [{id, entity}]) do
        {:ok, ops, conds} ->
          write_guarded(meta, view, ops, conds ++ lock_conditions(id, locks), fn -> [] end, %{})

        {:restrict, name} ->
          {:invalid, [foreign_key: name]}

        {:refused, info} ->
          raise Ecto.Hosi.Error, reason: {:refused, info}, message: Delete.message(meta, info)
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

  # the view a write goes through
  defp view!(meta, %{schema: schema} = into) when not is_nil(schema) do
    view = Model.view(meta.model, schema)
    Reader.source!(view, Map.get(into, :source, view.source))
    view
  end

  defp view!(_meta, into) do
    raise ArgumentError,
          "Ecto.Adapters.Hosi does not write by name (#{inspect(Map.get(into, :source))}): " <>
            "hosigraph has no tables, so a name says nothing about the facts. Write through a " <>
            "view of the Repo's :schemas, or with Repo.write_facts/2 (ecto_hosi's limit)"
  end

  # [{attribute, value}] as Ecto gives them -> as the adapter holds them (nil values are not
  # facts, except when removing)
  defp field_values(meta, fields, opts \\ []) do
    keep_nil? = Keyword.get(opts, :keep_nil, false)

    for {attr, value} <- fields, keep_nil? or not is_nil(value) do
      if attr == :id do
        raise ArgumentError,
              "Ecto.Adapters.Hosi cannot insert or change an id: hosigraph makes them " <>
                "(ecto_hosi's limit)"
      end

      {attr, Store.normalize(meta.model, attr, value)}
    end
  end

  defp split_filters(filters) do
    {id_pair, locks} = Enum.split_with(filters, fn {f, _} -> f == :id end)

    case id_pair do
      [{:id, id}] when is_integer(id) ->
        {id, locks}

      _ ->
        raise ArgumentError,
              "Ecto.Adapters.Hosi needs the integer :id to update or delete a record " <>
                "(hosigraph's ids are integers)"
    end
  end

  defp lock_conditions(id, locks) do
    for {attr, v} <- locks do
      if is_nil(v), do: [:unset, id, attr], else: [:holds, id, attr, v]
    end
  end

  defp write_guarded(meta, view, ops, conds, on_ok, values) do
    if ops == [] do
      # nothing to change, and the record was just read
      {:ok, on_ok.()}
    else
      case Backend.write(meta, ops, when: conds) do
        {:ok, _} -> {:ok, on_ok.()}
        {:error, {_, :conflict, _}} -> {:error, :stale}
        {:error, {_, :"write-rejected", msg}} -> {:invalid, rejected(meta, view, values, msg)}
        {:error, reason} -> raise Ecto.Hosi.Error, reason: reason
      end
    end
  end

  # The values Ecto asked back (`:id`, or fields it wants read after the write).
  defp returning_values(_meta, _view, id, [:id]), do: [id: id]
  defp returning_values(_meta, _view, _id, []), do: []

  defp returning_values(meta, view, id, returning) do
    entity = Backend.read_back(fn -> Backend.read_one(meta, id) end)
    for attr <- returning, do: {attr, returned(meta, view, id, entity, attr)}
  end

  defp insert_all_returning(_meta, _view, _ids, []), do: nil
  defp insert_all_returning(_meta, _view, ids, [:id]), do: Enum.map(ids, &[&1])

  defp insert_all_returning(meta, view, ids, returning) do
    entities = Backend.read_back(fn -> Backend.get_entities(meta, ids) end) |> Backend.ok!()

    for {id, entity} <- Enum.zip(ids, entities) do
      for attr <- returning, do: returned(meta, view, id, entity, attr)
    end
  end

  defp returned(_meta, _view, id, _entity, :id), do: id

  defp returned(meta, view, _id, entity, attr) do
    case meta.model.attrs[attr] do
      %{card: :many} -> entity |> Map.get(attr, []) |> Enum.sort()
      _ -> Types.read(view.types[attr], Backend.first(entity, attr))
    end
  end

  ## ---- unique constraints
  #
  # Before writing, ask whether a unique value is already taken: that answers precisely, with the
  # constraint name `unique_constraint/3` expects. hosigraph's own `unique` stays the guard against
  # races; if it still refuses, `rejected/4` works out which attribute from the message.

  @doc false
  # the unique rules that the values (`%{attribute => value}`) would break, by the constraint
  # name of a write through `view`
  def __check_unique__(meta, view, values, self_id, entity \\ nil) do
    meta
    |> __taken__(values, self_id, entity)
    |> Enum.map(&{:unique, Model.unique_name(meta.model, view, &1)})
    |> Enum.uniq()
  end

  @doc false
  # the same, as the attributes of those rules
  def __taken__(%{precheck_unique: false}, _values, _self_id, _entity), do: []

  def __taken__(meta, values, self_id, entity) do
    plain =
      for {attr, v} <- Enum.sort(values),
          match?(%{unique: true, tuple: nil}, meta.model.attrs[attr]),
          element <- Store.elements(meta.model, attr, v),
          known?(element),
          taken?(meta, attr, element, self_id),
          uniq: true,
          do: attr

    plain ++ tuple_violations(meta, values, self_id, entity)
  end

  defp known?([:tmp, _]), do: false
  defp known?([:ref, _]), do: false
  defp known?(_), do: true

  # A `tuple: true` rule is hosigraph's own tuple: ask for the pair as one value. The parts a
  # write does not touch are read from the record (an update).
  defp tuple_violations(meta, values, self_id, entity) do
    for rule <- native_rules(meta, values),
        parts = tuple_values(rule, values, entity || %{}),
        parts != nil,
        taken?(meta, rule.attr, parts, self_id) do
      rule.attr
    end
  end

  # the `tuple: true` rules that a write of these values touches
  defp native_rules(meta, values),
    do: meta.model |> Derived.touched(Map.keys(values)) |> Enum.filter(& &1.native)

  defp tuple_values(rule, values, entity) do
    parts =
      Enum.map(rule.attrs, fn a ->
        if Map.has_key?(values, a), do: values[a], else: Backend.first(entity, a)
      end)

    if Enum.any?(parts, &(is_nil(&1) or not known?(&1))), do: nil, else: parts
  end

  defp taken?(meta, attr, value, self_id) do
    value =
      case meta.model.attrs[attr] do
        %{tuple: parts} when parts != nil ->
          Enum.zip_with(parts, value, &Store.out(meta.model, &1, &2))

        _ ->
          Store.out(meta.model, attr, value)
      end

    req = %{q: "[:find ?e :in $ ?v :where [?e #{Model.kw(attr)} ?v]]", inputs: [[:scalar, value]]}
    rows = meta |> Backend.query(req) |> Backend.ok!()
    Enum.any?(rows, fn [e] -> e != self_id end)
  end

  defp rejected(meta, view, values, message) do
    uniques =
      for({attr, _} <- Enum.sort(values), match?(%{unique: true}, meta.model.attrs[attr]), do: attr) ++
        # a tuple the khana keeps is never in the write, but it is what hosigraph names
        for(rule <- native_rules(meta, values), do: rule.attr)

    uniques = Enum.uniq(uniques)
    named = Enum.filter(uniques, &mentions?(message, &1))
    name = &{:unique, Model.unique_name(meta.model, view, &1)}

    case {named, uniques} do
      {[_ | _], _} -> Enum.map(named, name)
      {[], [one]} -> [name.(one)]
      _ -> raise Ecto.Hosi.Error, message: "hosigraph refused the write: " <> message
    end
  end

  # does the message name this attribute, as `:email` or `:"user/email"`?
  defp mentions?(message, attr) do
    Regex.match?(~r/:"?#{Regex.escape(Atom.to_string(attr))}(?![\w\/\-\.+])/, message)
  end
end
