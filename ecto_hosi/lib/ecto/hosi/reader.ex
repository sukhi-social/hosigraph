defmodule Ecto.Hosi.Reader do
  @moduledoc false
  # `%Ecto.Query{}` -> Datalog. Supports what a store of facts can answer honestly:
  # one view, `where` with == != < <= > >= in is_nil not and or, `order_by`, `limit`,
  # `offset`, `select` of fields (or the struct), and `count`. Anything else goes to the hybrid
  # path, or raises and points to `Repo.datalog/1`.
  #
  # The records of a view are the ones whose facts say so (`hosi_where`): `member_clauses/3`.
  #
  # Inside a `not`, values are written into the query's text and not handed in as inputs. Datalevin
  # (1.2.10) puts a single input into a query before it plans it, and then refuses a `not-join`
  # that lists it; and a list handed in would be joined with the `not`, which asks "is there one of
  # these that it is not?" instead of "is it none of these?".

  alias Ecto.Hosi.{Backend, Model, Store, Types}

  @cmp_ops [:==, :!=, :<, :<=, :>, :>=]
  @cmp_text %{==: "=", !=: "not=", <: "<", <=: "<=", >: ">", >=: ">="}
  @flip %{==: :==, !=: :!=, <: :>, <=: :>=, >: :<, >=: :<=}

  # how many values may be written into one `not` (Datalevin cannot compile queries of thousands of
  # clauses), and from how many on `not (x.field in list)` is checked here instead
  @inline 500
  @filtered 200

  @doc """
  The ids the `where` of a query may match, for `update_all`/`delete_all`: `{view, ids}`.
  Only a candidate list: the caller reads the entities and checks the view shows them.
  """
  def candidate_ids(meta, %Ecto.Query{} = query, params) do
    params = Types.normalize_params(params)
    check_many!(meta, query)
    check_supported!(query)
    view = view!(meta, query.from.source)
    {expr, excluded} = split_excluded(meta, where_expr(query), params)

    ids =
      case direct_ids(expr, params) do
        {:ok, ids} -> ids
        :error -> datalog_ids(meta, view, expr, params)
      end

    ids =
      if excluded == [] do
        ids
      else
        for {id, entity} <- Backend.read(meta, ids),
            not excluded?(%{id: id, entity: entity}, excluded),
            do: id
      end

    {view, ids}
  end

  @doc "The view a query source is read through. There is no reading by name."
  def view!(meta, {name, schema}) when not is_nil(schema) do
    view = Model.view(meta.model, schema)
    source!(view, name)
    view
  end

  def view!(_meta, {name, nil}) do
    raise ArgumentError,
          "Ecto.Adapters.Hosi does not query by name (from #{inspect(name)}): hosigraph has no " <>
            "tables, so a name says nothing about the facts. Query a view of the Repo's " <>
            ":schemas (from n in MyApp.Note), or use Repo.datalog/1 (ecto_hosi's limit)"
  end

  def view!(_meta, _source),
    do: unsupported("this query source", "query a view: from n in MyApp.Note")

  @doc "Raise for a source name other than the view's own: a name does not choose facts."
  def source!(%{source: source}, source), do: :ok

  def source!(view, name) do
    raise ArgumentError,
          "Ecto.Adapters.Hosi: #{inspect(view.module)} was asked for under the name " <>
            "#{inspect(name)}. hosigraph has no tables, so another name does not choose other " <>
            "facts: leave the source as #{inspect(view.source)}, or make another view with its own " <>
            "hosi_where (ecto_hosi's limit)"
  end

  @doc "The clauses that say a record `var` shows up in the view."
  def member_clauses(meta, view, var) do
    Enum.map(view.where, fn
      {attr, :present} ->
        "[#{var} #{Model.kw(attr)} _]"

      {attr, {:eq, value}} ->
        value = Store.out(meta.model, attr, value)
        "[#{var} #{Model.kw(attr)} #{literal(value)}]"
    end)
  end

  @doc "Raise for a query with a subquery anywhere in it, naming a Datalog query to start from."
  def no_subquery!(meta, %Ecto.Query{} = query) do
    if subquery?(Map.from_struct(query)) do
      start =
        case start_source(query.from.source) do
          nil ->
            ""

          source ->
            view = view!(meta, source)
            clauses = Enum.join(member_clauses(meta, view, "?e"), " ")

            " Start from the records of #{inspect(view.module)} and add clauses: " <>
              ~s|Repo.datalog(%{q: "[:find ?e :where #{String.replace(clauses, "\"", "\\\"")}]", inputs: []})|
        end

      raise ArgumentError,
            "Ecto.Adapters.Hosi does not support a subquery in Ecto queries (ecto_hosi's limit; " <>
              "Datalog says it in one query)." <> start
    end

    :ok
  end

  defp subquery?(%Ecto.SubQuery{}), do: true
  defp subquery?(%_{} = struct), do: subquery?(Map.from_struct(struct))
  defp subquery?(map) when is_map(map), do: map |> Map.values() |> subquery?()
  defp subquery?(list) when is_list(list), do: Enum.any?(list, &subquery?/1)
  defp subquery?(tuple) when is_tuple(tuple), do: tuple |> Tuple.to_list() |> subquery?()
  defp subquery?(_), do: false

  # the source whose records the (innermost) query is about
  defp start_source({name, schema}) when is_binary(name) and not is_nil(schema), do: {name, schema}
  defp start_source(%Ecto.SubQuery{query: query}), do: start_source(query.from.source)
  defp start_source(_), do: nil

  def all(meta, %Ecto.Query{} = query, params) do
    params = Types.normalize_params(params)
    check_many!(meta, query)

    if one_view?(meta, query, params),
      do: one_view(meta, query, params),
      else: Ecto.Hosi.Hybrid.all(meta, query, params)
  end

  # A field with many values is a set: it is searched by its items (`x in v.field`) or asked
  # whether it is there at all (`is_nil`); a comparison or an order of the whole would mean nothing.
  defp check_many!(meta, query) do
    schemas =
      [query.from.source | Enum.map(query.joins, & &1.source)]
      |> Enum.with_index()
      |> Enum.reduce(%{}, fn
        {{_, schema}, ix}, acc when not is_nil(schema) -> Map.put(acc, ix, schema)
        _, acc -> acc
      end)

    many? = fn
      {{:., _, [{:&, _, [ix]}, attr]}, _, []} ->
        Map.has_key?(schemas, ix) and match?(%{card: :many}, meta.model.attrs[attr])

      _ ->
        false
    end

    exprs =
      Enum.map(query.wheres, & &1.expr) ++
        Enum.map(query.joins, & &1.on.expr) ++ Enum.map(query.havings, & &1.expr)

    Enum.each(exprs, fn expr ->
      Macro.prewalk(expr, fn
        {op, _, [l, r]} = e when op in @cmp_ops ->
          if many?.(l) or many?.(r), do: many!(e)
          e

        {:in, _, [l, _]} = e ->
          if many?.(l), do: many!(e)
          e

        other ->
          other
      end)
    end)

    for clause <- query.order_bys ++ query.group_bys,
        item <- clause.expr do
      e = case item do
        {_dir, e} -> e
        e -> e
      end

      if many?.(e), do: many!(e)
    end
  end

  defp many!(expr) do
    unsupported(
      "#{Macro.to_string(expr)}: a field with many values is a set",
      "search it by its items (\"x\" in v.field), ask is_nil(v.field), or use Repo.datalog/1"
    )
  end

  # The one-view path answers exactly what Datalog can say; anything else (a join, group_by,
  # an aggregate, a fragment, like, ...) goes to the hybrid path.
  defp one_view?(meta, query, params) do
    check_supported!(query)
    view = view!(meta, query.from.source)
    st = state(meta, view, params)
    {expr, _} = split_excluded(meta, where_expr(query), params)
    if expr, do: comp(expr, st)
    Enum.each(query.select.fields, &select_field/1)
    order_keys(query)
    true
  rescue
    _ in [ArgumentError, MatchError, FunctionClauseError] -> false
  end

  @doc false
  # The one-view query as a plan:
  # `{:one_view, view, ids | {:datalog, request}, fields, order keys, excluded fields}`.
  def plan(meta, query, params) do
    params = Types.normalize_params(params)
    check_many!(meta, query)

    if one_view?(meta, query, params) do
      view = view!(meta, query.from.source)
      {expr, excluded} = split_excluded(meta, where_expr(query), params)

      source =
        case direct_ids(expr, params) do
          {:ok, ids} -> {:ids, ids}
          :error -> {:datalog, request(meta, view, expr, params)}
        end

      fields = Enum.map(query.select.fields, &select_field/1)
      {:one_view, view, source, fields, order_keys(query), Enum.map(excluded, &elem(&1, 0))}
    else
      :hybrid
    end
  end

  defp one_view(meta, query, params) do
    check_supported!(query)
    view = view!(meta, query.from.source)
    fields = Enum.map(query.select.fields, &select_field/1)
    {expr, excluded} = split_excluded(meta, where_expr(query), params)
    keys = order_keys(query)

    {ids, verify?} =
      case direct_ids(expr, params) do
        {:ok, ids} -> {ids, true}
        :error -> {datalog_ids(meta, view, expr, params), false}
      end

    cond do
      fields == [:count] and not verify? and excluded == [] ->
        {1, [[length(ids)]]}

      true ->
        needs_entities? =
          verify? or excluded != [] or Enum.any?(fields, &entity_field?/1) or
            Enum.any?(keys, fn {_, f} -> f != :id end)

        rows = rows(meta, view, ids, needs_entities?, verify?)
        rows = Enum.reject(rows, &excluded?(&1, excluded))
        rows = rows |> sort(keys) |> window(query, params)

        if fields == [:count] do
          {1, [[length(rows)]]}
        else
          out = Enum.map(rows, fn row -> Enum.map(fields, &value(&1, row, view, meta)) end)
          {length(out), out}
        end
    end
  end

  ## ---------------------------------------------------------------- unsupported

  defp check_supported!(q) do
    for {present?, what} <- [
          {q.joins != [], "joins"},
          {q.group_bys != [], "group_by"},
          {q.havings != [], "having"},
          {not is_nil(q.distinct), "distinct"},
          {q.combinations != [], "union/except/intersect"},
          {not is_nil(q.with_ctes), "with_cte"},
          {q.windows != [], "windows"},
          {not is_nil(q.lock), "lock"}
        ],
        present? do
      unsupported(what, "write it as a Datalog query with Repo.datalog/1")
    end

    :ok
  end

  defp unsupported(what, hint) do
    raise ArgumentError,
          "Ecto.Adapters.Hosi does not support #{what} in Ecto queries (ecto_hosi's limit); #{hint}"
  end

  ## ---------------------------------------------------------------- where

  defp where_expr(%{wheres: wheres}) do
    Enum.reduce(wheres, nil, fn
      %{expr: e}, nil -> e
      %{op: :or, expr: e}, acc -> {:or, [], [acc, e]}
      %{expr: e}, acc -> {:and, [], [acc, e]}
    end)
  end

  # `Repo.get/2` and friends: `id == ^x` or `id in ^xs` need no Datalog at all.
  defp direct_ids({op, _, [l, r]}, params) when op in [:==, :in] do
    with {:field, :id} <- classify(l, params),
         {kind, v} when kind in [:value, :list] <- classify(r, params) do
      {:ok, v |> List.wrap() |> Enum.filter(&is_integer/1) |> Enum.uniq() |> Enum.sort()}
    else
      _ -> :error
    end
  end

  defp direct_ids(_, _), do: :error

  defp datalog_ids(meta, view, expr, params) do
    meta
    |> Backend.query(request(meta, view, expr, params))
    |> Backend.ok!()
    |> Enum.map(fn [id] -> id end)
    |> Enum.sort()
  end

  # A long `not (x.field in list)` among the conditions that all have to hold is checked on the
  # records instead: `{the other conditions, [{field, values}]}`.
  defp split_excluded(_meta, nil, _params), do: {nil, []}

  defp split_excluded(meta, expr, params) do
    {excluded, rest} =
      expr
      |> flatten_and()
      |> Enum.map(&{excluding(meta, &1, params), &1})
      |> Enum.split_with(fn {found, _} -> found != nil end)

    rest =
      rest
      |> Enum.map(&elem(&1, 1))
      |> Enum.reduce(nil, fn e, acc -> if acc, do: {:and, [], [acc, e]}, else: e end)

    {rest, Enum.map(excluded, &elem(&1, 0))}
  end

  defp excluding(meta, {:not, _, [{:in, _, [{{:., _, [{:&, _, [0]}, f]}, _, []}, list]}]}, params) do
    case classify(list, params) do
      {kind, values} when kind in [:list, :value] ->
        values = values |> Types.unpack_list() |> List.wrap()

        if length(values) > @filtered do
          # (a list kept as one text is compared as the text)
          values =
            case meta.model.attrs[f] do
              %{enc: {:array, _}, card: :one} -> Enum.map(values, &Store.normalize(meta.model, f, &1))
              _ -> values
            end

          {f, MapSet.new(values)}
        end

      _ ->
        nil
    end
  end

  defp excluding(_meta, _expr, _params), do: nil

  defp excluded?(row, excluded) do
    Enum.any?(excluded, fn
      {:id, values} -> MapSet.member?(values, row.id)
      {f, values} -> row.entity |> Map.get(f, []) |> Enum.any?(&MapSet.member?(values, &1))
    end)
  end

  defp flatten_and({:and, _, [a, b]}), do: flatten_and(a) ++ flatten_and(b)
  defp flatten_and(e), do: [e]

  defp state(meta, view, params),
    do: %{n: 0, inputs: [], params: params, model: meta.model, meta: meta, view: view, negated: false}

  # the Datalog request that finds the records of the view that the where matches
  defp request(meta, view, expr, params) do
    st = state(meta, view, params)
    {clauses, st} = if expr, do: comp(expr, st), else: {[], st}
    inputs = Enum.reverse(st.inputs)
    specs = Enum.map(inputs, &elem(&1, 0))

    q =
      "[:find ?e :in #{Enum.join(["$" | specs], " ")} " <>
        ":where #{Enum.join(member_clauses(meta, view, "?e") ++ clauses, " ")}]"

    %{q: q, inputs: Enum.map(inputs, &elem(&1, 1))}
  end

  # --- expressions -> clauses

  defp comp({:and, _, [a, b]}, st) do
    {ca, st} = comp(a, st)
    {cb, st} = comp(b, st)
    {ca ++ cb, st}
  end

  defp comp({:or, _, _} = e, st) do
    {branches, st} =
      e
      |> flatten_or()
      |> Enum.map_reduce(st, fn b, st ->
        {cl, st} = comp(b, st)
        {"(and #{Enum.join(cl, " ")})", st}
      end)

    text = Enum.join(branches, " ")
    {["(or-join [#{Enum.join(join_vars(text), " ")}] #{text})"], st}
  end

  defp comp({:not, _, [{:is_nil, _, [f]}]}, st) do
    {:field, field} = classify(f, st.params)
    {["[?e #{Model.kw(nil_field!(field))} _]"], st}
  end

  defp comp({:not, _, [inner]}, st) do
    {cl, inside} = comp(inner, %{st | negated: true})
    {["(not-join [?e] #{Enum.join(cl, " ")})"], %{inside | negated: st.negated}}
  end

  defp comp({:is_nil, _, [f]}, st) do
    {:field, field} = classify(f, st.params)
    {["(not-join [?e] [?e #{Model.kw(nil_field!(field))} _])"], st}
  end

  defp comp({:in, _, [l, r]}, st) do
    case {classify(l, st.params), classify(r, st.params)} do
      # `x in v.field` of a field with many values: the record has that value
      {{:value, v}, {:field, f}} ->
        unless match?(%{card: :many}, st.model.attrs[f]), do: unsupported_expr({:in, [], [l, r]})
        {var, st} = input(:scalar, Store.out(st.model, f, v), st)
        {["[?e #{Model.kw(f)} #{var}]"], st}

      {{:field, f}, {kind, vs}} when kind in [:list, :value] ->
        case vs |> Types.unpack_list() |> List.wrap() do
          [] ->
            {["[(= 0 1)]"], st}

          vs ->
            one_of(f, store(st, f, vs), st)
        end

      _ ->
        unsupported_expr({:in, [], [l, r]})
    end
  end

  defp comp({op, _, [l, r]} = e, st) when op in @cmp_ops do
    case {classify(l, st.params), classify(r, st.params)} do
      {{:field, f}, {:field, g}} ->
        {lv, c1, st} = field_var(f, st)
        {rv, c2, st} = field_var(g, st)
        {c1 ++ c2 ++ ["[(#{@cmp_text[op]} #{lv} #{rv})]"], st}

      {{:field, f}, {:value, v}} ->
        field_vs_value(op, f, v, st)

      {{:value, v}, {:field, f}} ->
        field_vs_value(@flip[op], f, v, st)

      _ ->
        unsupported_expr(e)
    end
  end

  defp comp(other, _st), do: unsupported_expr(other)

  defp field_vs_value(:==, f, v, st) when f != :id do
    {var, st} = input(:scalar, store(st, f, v), st)
    {["[?e #{Model.kw(f)} #{var}]"], st}
  end

  defp field_vs_value(op, f, v, st) do
    {lv, clauses, st} = field_var(f, st)
    {var, st} = input(:scalar, store(st, f, v), st)
    {clauses ++ ["[(#{@cmp_text[op]} #{lv} #{var})]"], st}
  end

  # the field is one of the values
  defp one_of(f, values, %{negated: false} = st) do
    {var, st} = input(:coll, values, st)

    if f == :id,
      do: {["[(= ?e #{var})]"], st},
      else: {["[?e #{Model.kw(f)} #{var}]"], st}
  end

  defp one_of(f, values, st) do
    if length(values) > @inline do
      unsupported(
        "a list of #{length(values)} values inside a not that is part of an or or of another not",
        "put the not (x.field in list) among the conditions that all have to hold"
      )
    end

    {var, clauses, st} = field_var(f, st)
    {clauses ++ [any_of(var, values)], st}
  end

  @doc "How many values `any_of/2` takes."
  def inline_limit, do: @inline

  @doc "The clause that says a variable is one of these values, written into the query."
  def any_of(var, [value]), do: "[(= #{var} #{literal(value)})]"
  def any_of(var, values), do: "(or #{Enum.map_join(values, " ", &any_of(var, [&1]))})"

  @doc "A value as Datalog text."
  def literal(value), do: value |> Hosi.Edn.encode() |> IO.iodata_to_binary()

  # a value as hosigraph keeps it for the field (a keyword, a UUID, a list kept as one text)
  defp store(_st, :id, v), do: v

  defp store(st, f, vs) when is_list(vs) do
    case st.model.attrs[f] do
      %{enc: {:array, _}, card: :one} ->
        # a list of lists (`in`) or one list (`==`)
        if Enum.all?(vs, &(is_list(&1) or is_binary(&1))) and Enum.any?(vs, &is_list/1),
          do: Enum.map(vs, &Store.normalize(st.model, f, &1)),
          else: Store.normalize(st.model, f, vs)

      _ ->
        Store.out(st.model, f, vs)
    end
  end

  defp store(st, f, v), do: Store.out(st.model, f, v)

  defp field_var(:id, st), do: {"?e", [], st}

  defp field_var(f, st) do
    var = "?v#{st.n}"
    {var, ["[?e #{Model.kw(f)} #{var}]"], %{st | n: st.n + 1}}
  end

  # a value of the query: an input, or inside a `not` its text
  defp input(:scalar, value, %{negated: true} = st), do: {literal(value), st}

  defp input(kind, value, st) do
    var = "?p#{st.n}"
    spec = if kind == :coll, do: "[#{var} ...]", else: var
    {var, %{st | n: st.n + 1, inputs: [{spec, [kind, value]} | st.inputs]}}
  end

  defp flatten_or({:or, _, [a, b]}), do: flatten_or(a) ++ flatten_or(b)
  defp flatten_or(e), do: [e]

  defp join_vars(text), do: ["?e" | Regex.scan(~r/\?p\d+/, text) |> List.flatten() |> Enum.uniq()]

  defp nil_field!(:id), do: raise(ArgumentError, "is_nil on the primary key is not meaningful")
  defp nil_field!(f), do: f

  defp unsupported_expr(expr) do
    unsupported(
      "this expression (#{Macro.to_string(expr)})",
      "use Repo.datalog/1 for queries beyond comparisons, in, is_nil, not, and, or"
    )
  end

  # --- what an expression stands for

  defp classify({{:., _, [{:&, _, [0]}, f]}, _, []}, _params), do: {:field, f}
  defp classify(%Ecto.Query.Tagged{value: v}, params), do: classify(v, params)
  defp classify({:^, _, [ix]}, params), do: {:value, Enum.at(params, ix)}
  defp classify({:^, _, [ix, len]}, params), do: {:list, Enum.slice(params, ix, len)}
  defp classify(list, _params) when is_list(list), do: {:list, list}

  defp classify(v, _params) when is_number(v) or is_binary(v) or is_boolean(v) or is_atom(v),
    do: {:value, v}

  defp classify(other, _params), do: unsupported_expr(other)

  ## ---------------------------------------------------------------- select / order / window

  defp select_field({{:., _, [{:&, _, [0]}, f]}, _, []}), do: {:field, f}
  defp select_field({:count, _, []}), do: :count
  defp select_field({:count, _, [_field]}), do: :count
  defp select_field(v) when is_number(v) or is_binary(v) or is_boolean(v), do: {:lit, v}

  defp select_field(other),
    do:
      unsupported(
        "this select (#{Macro.to_string(other)})",
        "select fields, the struct, or count"
      )

  defp entity_field?({:field, f}), do: f != :id
  defp entity_field?(_), do: false

  defp order_keys(%{order_bys: order_bys}) do
    for %{expr: exprs} <- order_bys, {dir, e} <- exprs do
      {:field, f} = classify(e, [])
      {if(dir in [:desc, :desc_nulls_first, :desc_nulls_last], do: :desc, else: :asc), f}
    end
  end

  defp rows(meta, view, ids, needs_entities?, verify?) do
    if needs_entities? and ids != [] do
      pairs = Backend.read(meta, ids)

      pairs =
        if verify?, do: Enum.filter(pairs, fn {_, e} -> Model.member?(view, e) end), else: pairs

      Enum.map(pairs, fn {id, entity} -> %{id: id, entity: entity} end)
    else
      Enum.map(ids, &%{id: &1, entity: %{}})
    end
  end

  defp sort(rows, keys) do
    keys = if keys == [], do: [{:asc, :id}], else: keys
    Enum.sort(rows, &before?(&1, &2, keys))
  end

  defp before?(_a, _b, []), do: true

  defp before?(a, b, [{dir, f} | rest]) do
    x = field_value(a, f)
    y = field_value(b, f)

    cond do
      x == y -> before?(a, b, rest)
      dir == :asc -> x < y
      true -> x > y
    end
  end

  defp field_value(%{id: id}, :id), do: id
  defp field_value(%{entity: e}, f), do: Backend.first(e, f)

  defp window(rows, query, params) do
    rows = rows |> Enum.drop(int(query.offset, params) || 0)

    case int(query.limit, params) do
      nil -> rows
      n -> Enum.take(rows, n)
    end
  end

  defp int(nil, _params), do: nil
  defp int(%{expr: n}, _params) when is_integer(n), do: n
  defp int(%{expr: {:^, _, [ix]}}, params), do: Enum.at(params, ix)

  defp value({:field, :id}, row, _view, _meta), do: row.id

  defp value({:field, f}, row, view, meta) do
    case meta.model.attrs[f] do
      %{card: :many} -> row.entity |> Map.get(f, []) |> Enum.sort()
      _ -> Types.read(view.types[f], field_value(row, f))
    end
  end

  defp value({:lit, v}, _row, _view, _meta), do: v
end
