defmodule Ecto.Hosi.Hybrid do
  @moduledoc false
  # Queries the one-schema reader cannot answer: joins, group_by, having, distinct, aggregates,
  # fragments, like. The way Datalog is comfortable with:
  #
  #   1. The base table and the leading run of inner/cross joins are ONE Datalog query. A join is
  #      a shared variable (`[?e1 :note/account_id ?e0]`), and every part of the `where` that
  #      Datalog can say is pushed into it. What comes back are tuples of entity ids.
  #   2. A left join, or a join after one, reads its table once, narrowed by the keys of the tuples
  #      so far, and is joined here.
  #   3. The WHOLE where (and the ON of every inner join) is then checked again in Elixir with SQL's
  #      meaning (`Ecto.Hosi.Eval`): the pushdown only has to be a superset, never exact. What Datalog
  #      cannot say (fragment, like, coalesce) is decided here.
  #   4. group_by, having, aggregates, distinct, order_by, offset, limit and select are done here.
  #
  # A query that names no restricting `where` reads whole tables: write it with `Repo.datalog/1`
  # when that matters.

  alias Ecto.Hosi.{Attr, Backend, Eval, Types}

  @cmp_text %{==: "=", !=: "not=", <: "<", <=: "<=", >: ">", >=: ">="}
  @flip %{==: :==, !=: :!=, <: :>, <=: :>=, >: :<, >=: :<=}
  @chunk 500

  def all(meta, %Ecto.Query{} = query, params) do
    check!(query)
    bindings = bindings(query)
    ctx = %{meta: meta, params: params, schemas: Map.new(bindings, &{&1.ix, &1.schema})}
    span = span(bindings)
    {where, wheres} = where_parts(query)

    on_conj =
      bindings
      |> Enum.slice(1, span - 1)
      |> Enum.flat_map(&split_and(&1.on))

    envs = base(meta, Enum.take(bindings, span), wheres ++ on_conj, params)

    envs =
      bindings
      |> Enum.drop(span)
      |> Enum.reduce(envs, fn b, envs -> join(meta, envs, b, ctx) end)

    # the where, whole, and the ON of the inner joins, are checked again with SQL's meaning:
    # what was pushed into Datalog only has to be a superset
    checks = List.wrap(where) ++ on_conj
    envs = Enum.filter(envs, fn env -> Enum.all?(checks, &(Eval.eval(&1, env, ctx) == true)) end)
    rows = shape(query, envs, ctx)
    {length(rows), rows}
  end

  ## ---------------------------------------------------------------- bindings

  defp check!(q) do
    for {present?, what} <- [
          {q.combinations != [], "union/except/intersect"},
          {not is_nil(q.with_ctes), "with_cte"},
          {q.windows != [], "windows"},
          {not is_nil(q.lock), "lock"}
        ],
        present? do
      unsupported(what)
    end
  end

  defp bindings(query) do
    {from, from_schema} = source!(query.from.source)

    [
      %{ix: 0, source: from, schema: from_schema, qual: :from, on: nil}
      | for {j, ix} <- Enum.with_index(query.joins, 1) do
          unless j.qual in [:inner, :left, :cross] do
            unsupported("#{j.qual} join (only inner, left and cross)")
          end

          {source, schema} = source!(j.source)
          %{ix: ix, source: source, schema: schema, qual: j.qual, on: j.on.expr}
        end
    ]
  end

  defp source!({source, schema}) when is_binary(source), do: {source, schema}
  defp source!(other), do: unsupported("this query source (#{inspect(other)}), a subquery or fragment")

  # how many bindings (the base and the leading inner/cross joins) Datalog answers at once
  defp span(bindings) do
    1 + (bindings |> tl() |> Enum.take_while(&(&1.qual in [:inner, :cross])) |> length())
  end

  defp where_parts(%{wheres: wheres}) do
    expr =
      Enum.reduce(wheres, nil, fn
        %{expr: e}, nil -> e
        %{op: :or, expr: e}, acc -> {:or, [], [acc, e]}
        %{expr: e}, acc -> {:and, [], [acc, e]}
      end)

    {expr, if(expr, do: split_and(expr), else: [])}
  end

  defp split_and({:and, _, [a, b]}), do: split_and(a) ++ split_and(b)
  defp split_and(true), do: []
  defp split_and(e), do: [e]

  ## ---------------------------------------------------------------- the Datalog part

  defp base(meta, bindings, conjuncts, params) do
    sources = Map.new(bindings, &{&1.ix, &1.source})
    st = %{n: 0, inputs: [], params: params, span: length(bindings), sources: sources, kw: Backend.keywords(meta)}

    {clauses, st} =
      Enum.reduce(conjuncts, {[], st}, fn c, {acc, st} ->
        try do
          {cl, st2} = push(c, st)
          {acc ++ cl, st2}
        catch
          {:no_push, _} -> {acc, st}
        end
      end)

    vars = for b <- bindings, do: "?e#{b.ix}"
    markers = for b <- bindings, do: "[?e#{b.ix} #{Attr.kw(Attr.marker(b.source))} _]"
    inputs = Enum.reverse(st.inputs)
    specs = Enum.map(inputs, &elem(&1, 0))

    q =
      "[:find #{Enum.join(vars, " ")} :in #{Enum.join(["$" | specs], " ")} " <>
        ":where #{Enum.join(markers ++ clauses, " ")}]"

    rows = meta |> Backend.query(%{q: q, inputs: Enum.map(inputs, &elem(&1, 1))}) |> Backend.ok!()
    ids = rows |> List.flatten() |> Enum.uniq()
    entities = load(meta, ids)
    for tuple <- rows do
      for {id, b} <- Enum.zip(tuple, bindings), into: %{} do
        {b.ix, %{id: id, entity: Map.get(entities, id, %{}), source: sources[b.ix]}}
      end
    end
  end

  defp load(_meta, []), do: %{}

  defp load(meta, ids) do
    ids
    |> Enum.chunk_every(@chunk)
    |> Enum.flat_map(fn chunk ->
      meta |> Backend.get_entities(chunk) |> Backend.ok!() |> then(&Enum.zip(chunk, &1))
    end)
    |> Map.new()
  end

  # --- an expression -> clauses (throws {:no_push, _} for what Datalog cannot say)

  defp push({:and, _, [a, b]}, st) do
    {ca, st} = push(a, st)
    {cb, st} = push(b, st)
    {ca ++ cb, st}
  end

  defp push({:or, _, _} = e, st) do
    {branches, st} =
      e
      |> flatten_or()
      |> Enum.map_reduce(st, fn b, st ->
        {cl, st} = push(b, st)
        {"(and #{Enum.join(cl, " ")})", st}
      end)

    text = Enum.join(branches, " ")
    {["(or-join [#{Enum.join(join_vars(text), " ")}] #{text})"], st}
  end

  defp push({:not, _, [{:is_nil, _, [f]}]}, st) do
    {:field, ix, field} = classify(f, st)
    {["[?e#{ix} #{Attr.kw(attr(ix, field, st))} _]"], st}
  end

  defp push({:not, _, [inner]}, st) do
    {cl, st} = push(inner, st)
    text = Enum.join(cl, " ")
    {["(not-join [#{Enum.join(join_vars(text), " ")}] #{text})"], st}
  end

  defp push({:is_nil, _, [f]}, st) do
    {:field, ix, field} = classify(f, st)
    {["(not-join [?e#{ix}] [?e#{ix} #{Attr.kw(attr(ix, field, st))} _])"], st}
  end

  defp push({:in, _, [l, r]}, st) do
    with {:field, ix, f} <- classify(l, st),
         {kind, vs} when kind in [:list, :value] <- classify(r, st) do
      case vs |> Types.unpack_list() |> List.wrap() do
        [] ->
          {["[(= 0 1)]"], st}

        vs ->
          {var, st} = input(:coll, store(st, ix, f, vs), st)

          if f == :id,
            do: {["[(= ?e#{ix} #{var})]"], st},
            else: {["[?e#{ix} #{Attr.kw(attr(ix, f, st))} #{var}]"], st}
      end
    else
      _ -> throw({:no_push, :in})
    end
  end

  defp push({:==, _, [l, r]} = e, st) do
    case {classify(l, st), classify(r, st)} do
      {{:field, i, f}, {:field, j, g}} -> join_eq(i, f, j, g, st)
      _ -> compare(e, st)
    end
  end

  defp push({op, _, [_, _]} = e, st) when op in [:!=, :<, :<=, :>, :>=], do: compare(e, st)
  defp push(other, _st), do: throw({:no_push, other})

  # `a.f == b.g`: the two sides share one variable, which is what a join is
  defp join_eq(i, :id, j, :id, st), do: {["[(= ?e#{i} ?e#{j})]"], st}
  defp join_eq(i, :id, j, g, st), do: {["[?e#{j} #{Attr.kw(attr(j, g, st))} ?e#{i}]"], st}
  defp join_eq(i, f, j, :id, st), do: {["[?e#{i} #{Attr.kw(attr(i, f, st))} ?e#{j}]"], st}

  defp join_eq(i, f, j, g, st) do
    var = "?j#{st.n}"

    {["[?e#{i} #{Attr.kw(attr(i, f, st))} #{var}]", "[?e#{j} #{Attr.kw(attr(j, g, st))} #{var}]"],
     %{st | n: st.n + 1}}
  end

  defp compare({op, _, [l, r]}, st) do
    case {classify(l, st), classify(r, st)} do
      {{:field, i, f}, {:field, j, g}} ->
        {lv, c1, st} = field_var(i, f, st)
        {rv, c2, st} = field_var(j, g, st)
        {c1 ++ c2 ++ ["[(#{@cmp_text[op]} #{lv} #{rv})]"], st}

      {{:field, i, f}, {:value, v}} ->
        field_vs_value(op, i, f, v, st)

      {{:value, v}, {:field, i, f}} ->
        field_vs_value(@flip[op], i, f, v, st)

      _ ->
        throw({:no_push, op})
    end
  end

  defp field_vs_value(:==, ix, f, v, st) when f != :id and not is_nil(v) do
    {var, st} = input(:scalar, store(st, ix, f, v), st)
    {["[?e#{ix} #{Attr.kw(attr(ix, f, st))} #{var}]"], st}
  end

  defp field_vs_value(_op, _ix, _f, nil, _st), do: throw({:no_push, :nil_value})

  defp field_vs_value(op, ix, f, v, st) do
    {lv, clauses, st} = field_var(ix, f, st)
    {var, st} = input(:scalar, store(st, ix, f, v), st)
    {clauses ++ ["[(#{@cmp_text[op]} #{lv} #{var})]"], st}
  end

  # a `hosi_keyword` field is compared as the keyword hosigraph keeps (a string for Elixir)
  defp store(_st, _ix, :id, v), do: v
  defp store(st, ix, f, v), do: Ecto.Hosi.Kw.to_store(st.kw, attr(ix, f, st), v)

  defp field_var(ix, :id, st), do: {"?e#{ix}", [], st}

  defp field_var(ix, f, st) do
    var = "?v#{st.n}"
    {var, ["[?e#{ix} #{Attr.kw(attr(ix, f, st))} #{var}]"], %{st | n: st.n + 1}}
  end

  defp input(kind, value, st) do
    var = "?p#{st.n}"
    spec = if kind == :coll, do: "[#{var} ...]", else: var
    {var, %{st | n: st.n + 1, inputs: [{spec, [kind, value]} | st.inputs]}}
  end

  defp flatten_or({:or, _, [a, b]}), do: flatten_or(a) ++ flatten_or(b)
  defp flatten_or(e), do: [e]

  defp join_vars(text) do
    entities = Regex.scan(~r/\?e\d+/, text) |> List.flatten()
    others = Regex.scan(~r/\?(?:p|j)\d+/, text) |> List.flatten()
    Enum.uniq(entities ++ others)
  end

  defp classify({{:., _, [{:&, _, [ix]}, f]}, _, []}, _st), do: {:field, ix, f}
  defp classify(%Ecto.Query.Tagged{value: v}, st), do: classify(v, st)
  defp classify({:^, _, [ix]}, st), do: {:value, Enum.at(st.params, ix)}
  defp classify({:^, _, [ix, len]}, st), do: {:list, Enum.slice(st.params, ix, len)}
  defp classify(list, _st) when is_list(list), do: {:list, list}

  defp classify(v, _st) when is_number(v) or is_binary(v) or is_boolean(v), do: {:value, v}
  defp classify(other, _st), do: throw({:no_push, other})

  # the ix of a binding past the span can not appear in a pushed clause
  defp attr(ix, f, %{span: span}) when ix >= span, do: throw({:no_push, {:outside, ix, f}})
  defp attr(ix, f, st), do: Attr.attr(source_of(ix, st), f)
  defp source_of(ix, st), do: Map.fetch!(st.sources, ix)

  ## ---------------------------------------------------------------- joins past the Datalog span

  defp join(meta, envs, %{ix: ix, source: source, qual: qual, on: on}, ctx) do
    marker = Attr.marker(source)
    conj = split_and(on)
    {equi, rest} = equi(conj, ix)

    index =
      case equi do
        nil -> all_rows(meta, source)
        {f, expr} -> rows_by(meta, source, marker, f, keys(envs, expr, ctx))
      end

    Enum.flat_map(envs, fn env ->
      candidates =
        case equi do
          nil -> index
          {_f, expr} -> Map.get(index, Eval.eval(expr, env, ctx), [])
        end

      matches =
        for row <- candidates,
            env2 = Map.put(env, ix, row),
            Enum.all?(rest, &(Eval.eval(&1, env2, ctx) == true)),
            do: env2

      cond do
        matches != [] -> matches
        qual == :left -> [Map.put(env, ix, nil)]
        true -> []
      end
    end)
  end

  # `ix.f == <expression of earlier bindings>` among the ON conditions
  defp equi(conj, ix) do
    Enum.reduce(conj, {nil, []}, fn c, {found, rest} ->
      case {found, equi_pair(c, ix)} do
        {nil, {_, _} = pair} -> {pair, rest}
        _ -> {found, rest ++ [c]}
      end
    end)
  end

  defp equi_pair({:==, _, [a, b]}, ix) do
    cond do
      own_field?(a, ix) and earlier?(b, ix) -> {field_of(a), b}
      own_field?(b, ix) and earlier?(a, ix) -> {field_of(b), a}
      true -> nil
    end
  end

  defp equi_pair(_, _), do: nil

  defp own_field?({{:., _, [{:&, _, [ix]}, _]}, _, []}, ix), do: true
  defp own_field?(_, _), do: false
  defp field_of({{:., _, [_, f]}, _, []}), do: f
  defp earlier?(e, ix), do: refs(e) != [] and Enum.all?(refs(e), &(&1 < ix))

  defp refs({{:., _, [{:&, _, [i]}, _]}, _, []}), do: [i]
  defp refs({_, _, args}) when is_list(args), do: args |> Enum.flat_map(&refs/1) |> Enum.uniq()
  defp refs(l) when is_list(l), do: l |> Enum.flat_map(&refs/1) |> Enum.uniq()
  defp refs(_), do: []

  defp keys(envs, expr, ctx) do
    envs |> Enum.map(&Eval.eval(expr, &1, ctx)) |> Enum.reject(&is_nil/1) |> Enum.uniq()
  end

  defp all_rows(meta, source) do
    req = %{q: "[:find ?e :in $ :where [?e #{Attr.kw(Attr.marker(source))} _]]", inputs: []}
    ids = meta |> Backend.query(req) |> Backend.ok!() |> Enum.map(fn [id] -> id end)
    entities = load(meta, ids)
    rows = for id <- Enum.sort(ids), do: %{id: id, entity: entities[id], source: source}
    # no key to look up by: every row is a candidate for every tuple
    rows
  end

  # %{key => [row]} of the rows whose `f` is one of `keys`
  defp rows_by(_meta, _source, _marker, _f, []), do: %{}

  defp rows_by(meta, source, marker, :id, keys) do
    entities = load(meta, Enum.filter(keys, &is_integer/1))

    for {id, entity} <- entities, Map.has_key?(entity, marker), into: %{} do
      {id, [%{id: id, entity: entity, source: source}]}
    end
  end

  defp rows_by(meta, source, marker, f, keys) do
    attr = Attr.attr(source, f)

    pairs =
      keys
      |> Enum.chunk_every(@chunk)
      |> Enum.flat_map(fn chunk ->
        req = %{
          q:
            "[:find ?e ?v :in $ [?v ...] :where [?e #{Attr.kw(marker)} _] [?e #{Attr.kw(attr)} ?v]]",
          inputs: [[:coll, chunk]]
        }

        meta |> Backend.query(req) |> Backend.ok!()
      end)

    entities = load(meta, pairs |> Enum.map(&hd/1) |> Enum.uniq())

    pairs
    |> Enum.sort()
    |> Enum.group_by(fn [_e, v] -> v end, fn [e, _v] ->
      %{id: e, entity: entities[e], source: source}
    end)
  end

  ## ---------------------------------------------------------------- shape

  defp shape(query, envs, ctx) do
    fields = query.select.fields
    havings = Enum.flat_map(query.havings, &split_and(&1.expr))
    grouped? = query.group_bys != [] or Enum.any?(fields, &Eval.aggregate?/1) or havings != []

    items =
      cond do
        query.group_bys != [] -> group(envs, query, ctx)
        grouped? -> [envs]
        true -> Enum.map(envs, &[&1])
      end

    items =
      Enum.filter(items, fn g -> Enum.all?(havings, &(Eval.eval_group(&1, g, ctx) == true)) end)

    orders = order_exprs(query)

    items
    |> Enum.map(fn g ->
      row = Enum.map(fields, &project(&1, g, ctx))
      {Enum.map(orders, fn {_dir, e} -> Eval.eval_group(e, g, ctx) end), row, g}
    end)
    |> distinct(query, ctx)
    |> sort(orders)
    |> window(query, ctx)
    |> Enum.map(fn {_keys, row, _g} -> row end)
  end

  # a plain field is read as Ecto should see it (a binary is kept as text); the rest is computed
  defp project({{:., _, [{:&, _, [ix]}, f]}, _, []} = expr, g, ctx) do
    Types.read(ctx.schemas[ix], f, Eval.eval_group(expr, g, ctx))
  end

  defp project(expr, g, ctx), do: Eval.eval_group(expr, g, ctx)

  defp group(envs, query, ctx) do
    exprs = Enum.flat_map(query.group_bys, & &1.expr)

    envs
    |> Enum.group_by(fn env -> Enum.map(exprs, &Eval.eval(&1, env, ctx)) end)
    |> Enum.sort_by(fn {k, _} -> k end)
    |> Enum.map(fn {_k, g} -> g end)
  end

  defp order_exprs(%{order_bys: order_bys}) do
    for %{expr: exprs} <- order_bys, {dir, e} <- exprs, do: {dir, e}
  end

  defp distinct(items, %{distinct: nil}, _ctx), do: items

  defp distinct(items, %{distinct: %{expr: true}}, _ctx), do: Enum.uniq_by(items, fn {_, row, _} -> row end)

  defp distinct(items, %{distinct: %{expr: exprs}}, ctx) do
    exprs = for {_dir, e} <- List.wrap(exprs), do: e
    Enum.uniq_by(items, fn {_, _, g} -> Enum.map(exprs, &Eval.eval_group(&1, g, ctx)) end)
  end

  defp sort(items, []), do: items

  defp sort(items, orders) do
    dirs = Enum.map(orders, &elem(&1, 0))
    Enum.sort(items, fn {a, _, _}, {b, _, _} -> before?(a, b, dirs) end)
  end

  defp before?([], [], _), do: true

  defp before?([x | xs], [y | ys], [dir | dirs]) do
    case cmp(x, y, dir) do
      :eq -> before?(xs, ys, dirs)
      :lt -> true
      :gt -> false
    end
  end

  # NULLs sort last when ascending and first when descending, as PostgreSQL does, unless said
  defp cmp(x, y, _dir) when x == y, do: :eq

  defp cmp(x, y, dir) do
    nulls_first? = dir in [:asc_nulls_first, :desc, :desc_nulls_first]
    desc? = dir in [:desc, :desc_nulls_first, :desc_nulls_last]

    cond do
      is_nil(x) -> if nulls_first?, do: :lt, else: :gt
      is_nil(y) -> if nulls_first?, do: :gt, else: :lt
      (x < y) != desc? -> :lt
      true -> :gt
    end
  end

  defp window(items, query, ctx) do
    items = Enum.drop(items, int(query.offset, ctx) || 0)

    case int(query.limit, ctx) do
      nil -> items
      n -> Enum.take(items, n)
    end
  end

  defp int(nil, _ctx), do: nil
  defp int(%{expr: n}, _ctx) when is_integer(n), do: n
  defp int(%{expr: {:^, _, [ix]}}, ctx), do: Enum.at(ctx.params, ix)

  defp unsupported(what) do
    raise ArgumentError,
          "Ecto.Adapters.Hosi does not support #{what} in Ecto queries; " <>
            "write it as a Datalog query with Repo.datalog/1"
  end
end
