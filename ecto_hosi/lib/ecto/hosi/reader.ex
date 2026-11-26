defmodule Ecto.Hosi.Reader do
  @moduledoc false
  # `%Ecto.Query{}` -> Datalog. Supports what a table-less store can answer honestly:
  # one schema, `where` with == != < <= > >= in is_nil not and or, `order_by`, `limit`,
  # `offset`, `select` of fields (or the struct), and `count`. Anything else raises and points
  # to `Repo.datalog/1`.

  alias Ecto.Hosi.{Attr, Backend, Types}

  @cmp_ops [:==, :!=, :<, :<=, :>, :>=]
  @cmp_text %{==: "=", !=: "not=", <: "<", <=: "<=", >: ">", >=: ">="}
  @flip %{==: :==, !=: :!=, <: :>, <=: :>=, >: :<, >=: :<=}

  @doc """
  The ids the `where` of a query may match, for `update_all`/`delete_all`: `{source, schema, ids}`.
  Only a candidate list: the caller reads the entities and checks the marker.
  """
  def candidate_ids(meta, %Ecto.Query{} = query, params) do
    params = Types.normalize_params(params)
    check_supported!(query)
    {source, schema} = source!(query)
    expr = where_expr(query)

    ids =
      case direct_ids(expr, params) do
        {:ok, ids} -> ids
        :error -> datalog_ids(meta, Attr.marker(source), source, expr, params)
      end

    {source, schema, ids}
  end

  defp source!(query) do
    case query.from.source do
      {source, schema} when is_binary(source) -> {source, schema}
      _ -> unsupported("this query source", "name a schema or a table: from u in MyApp.User")
    end
  end

  def all(meta, %Ecto.Query{} = query, params) do
    params = Types.normalize_params(params)

    if legacy_ok?(query, params),
      do: one_schema(meta, query, params),
      else: Ecto.Hosi.Hybrid.all(meta, query, params)
  end

  # The one-schema path answers exactly what Datalog can say; anything else (a join, group_by,
  # an aggregate, a fragment, like, ...) goes to the hybrid path.
  defp legacy_ok?(query, params) do
    check_supported!(query)
    {source, _schema} = source!(query)
    st = %{n: 0, inputs: [], params: params}
    expr = where_expr(query)
    if expr, do: comp(expr, source, st)
    Enum.each(query.select.fields, &select_field/1)
    order_keys(query)
    true
  rescue
    _ in [ArgumentError, MatchError, FunctionClauseError] -> false
  end

  defp one_schema(meta, query, params) do
    check_supported!(query)

    {source, schema} = source!(query)

    fields = Enum.map(query.select.fields, &select_field/1)
    expr = where_expr(query)
    marker = Attr.marker(source)
    keys = order_keys(query)

    {ids, verify?} =
      case direct_ids(expr, params) do
        {:ok, ids} -> {ids, true}
        :error -> {datalog_ids(meta, marker, source, expr, params), false}
      end

    cond do
      fields == [:count] and not verify? ->
        {1, [[length(ids)]]}

      true ->
        needs_entities? =
          verify? or Enum.any?(fields, &entity_field?/1) or
            Enum.any?(keys, fn {_, f} -> f != :id end)

        rows = rows(meta, source, marker, ids, needs_entities?, verify?)
        rows = rows |> sort(keys, source) |> window(query, params)

        if fields == [:count] do
          {1, [[length(rows)]]}
        else
          out = Enum.map(rows, fn row -> Enum.map(fields, &value(&1, row, source, schema)) end)
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
    raise ArgumentError, "Ecto.Adapters.Hosi does not support #{what} in Ecto queries; #{hint}"
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

  defp datalog_ids(meta, marker, source, expr, params) do
    st = %{n: 0, inputs: [], params: params}
    {clauses, st} = if expr, do: comp(expr, source, st), else: {[], st}
    inputs = Enum.reverse(st.inputs)
    specs = Enum.map(inputs, &elem(&1, 0))

    q =
      "[:find ?e :in #{Enum.join(["$" | specs], " ")} " <>
        ":where #{Enum.join(["[?e #{Attr.kw(marker)} _]" | clauses], " ")}]"

    req = %{q: q, inputs: Enum.map(inputs, &elem(&1, 1))}

    req
    |> then(&Backend.query(meta, &1))
    |> Backend.ok!()
    |> Enum.map(fn [id] -> id end)
    |> Enum.sort()
  end

  # --- expressions -> clauses

  defp comp({:and, _, [a, b]}, source, st) do
    {ca, st} = comp(a, source, st)
    {cb, st} = comp(b, source, st)
    {ca ++ cb, st}
  end

  defp comp({:or, _, _} = e, source, st) do
    {branches, st} =
      e
      |> flatten_or()
      |> Enum.map_reduce(st, fn b, st ->
        {cl, st} = comp(b, source, st)
        {"(and #{Enum.join(cl, " ")})", st}
      end)

    text = Enum.join(branches, " ")
    {["(or-join [#{Enum.join(join_vars(text), " ")}] #{text})"], st}
  end

  defp comp({:not, _, [{:is_nil, _, [f]}]}, source, st) do
    {:field, field} = classify(f, st.params)
    {["[?e #{Attr.kw(Attr.attr(source, nil_field!(field)))} _]"], st}
  end

  defp comp({:not, _, [inner]}, source, st) do
    {cl, st} = comp(inner, source, st)
    {["(not-join [#{Enum.join(join_vars(Enum.join(cl, " ")), " ")}] #{Enum.join(cl, " ")})"], st}
  end

  defp comp({:is_nil, _, [f]}, source, st) do
    {:field, field} = classify(f, st.params)
    {["(not-join [?e] [?e #{Attr.kw(Attr.attr(source, nil_field!(field)))} _])"], st}
  end

  defp comp({:in, _, [l, r]}, source, st) do
    with {:field, f} <- classify(l, st.params),
         {kind, vs} when kind in [:list, :value] <- classify(r, st.params) do
      case vs |> Types.unpack_list() |> List.wrap() do
        [] ->
          {["[(= 0 1)]"], st}

        vs ->
          {var, st} = input(:coll, vs, st)

          if f == :id,
            do: {["[(= ?e #{var})]"], st},
            else: {["[?e #{Attr.kw(Attr.attr(source, f))} #{var}]"], st}
      end
    else
      _ -> unsupported_expr({:in, [], [l, r]})
    end
  end

  defp comp({op, _, [l, r]} = e, source, st) when op in @cmp_ops do
    case {classify(l, st.params), classify(r, st.params)} do
      {{:field, f}, {:field, g}} ->
        {lv, c1, st} = field_var(f, source, st)
        {rv, c2, st} = field_var(g, source, st)
        {c1 ++ c2 ++ ["[(#{@cmp_text[op]} #{lv} #{rv})]"], st}

      {{:field, f}, {:value, v}} ->
        field_vs_value(op, f, v, source, st)

      {{:value, v}, {:field, f}} ->
        field_vs_value(@flip[op], f, v, source, st)

      _ ->
        unsupported_expr(e)
    end
  end

  defp comp(other, _source, _st), do: unsupported_expr(other)

  defp field_vs_value(:==, f, v, source, st) when f != :id do
    {var, st} = input(:scalar, v, st)
    {["[?e #{Attr.kw(Attr.attr(source, f))} #{var}]"], st}
  end

  defp field_vs_value(op, f, v, source, st) do
    {lv, clauses, st} = field_var(f, source, st)
    {var, st} = input(:scalar, v, st)
    {clauses ++ ["[(#{@cmp_text[op]} #{lv} #{var})]"], st}
  end

  defp field_var(:id, _source, st), do: {"?e", [], st}

  defp field_var(f, source, st) do
    var = "?v#{st.n}"
    {var, ["[?e #{Attr.kw(Attr.attr(source, f))} #{var}]"], %{st | n: st.n + 1}}
  end

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

  defp rows(meta, source, marker, ids, needs_entities?, verify?) do
    if needs_entities? and ids != [] do
      entities = Backend.get_entities(meta, ids) |> Backend.ok!()
      pairs = Enum.zip(ids, entities)

      pairs =
        if verify?, do: Enum.filter(pairs, fn {_, e} -> Map.has_key?(e, marker) end), else: pairs

      _ = source
      Enum.map(pairs, fn {id, entity} -> %{id: id, entity: entity} end)
    else
      Enum.map(ids, &%{id: &1, entity: %{}})
    end
  end

  defp sort(rows, keys, source) do
    keys = if keys == [], do: [{:asc, :id}], else: keys
    Enum.sort(rows, &before?(&1, &2, keys, source))
  end

  defp before?(_a, _b, [], _source), do: true

  defp before?(a, b, [{dir, f} | rest], source) do
    x = field_value(a, f, source)
    y = field_value(b, f, source)

    cond do
      x == y -> before?(a, b, rest, source)
      dir == :asc -> x < y
      true -> x > y
    end
  end

  defp field_value(%{id: id}, :id, _source), do: id
  defp field_value(%{entity: e}, f, source), do: Backend.first(e, Attr.attr(source, f))

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

  defp value({:field, f}, row, source, schema),
    do: Types.read(schema, f, field_value(row, f, source))

  defp value({:lit, v}, _row, _source, _schema), do: v
end
