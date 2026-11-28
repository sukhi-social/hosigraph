defmodule Ecto.Hosi.Hybrid do
  @moduledoc false
  # Queries the one-schema reader cannot answer: joins, group_by, having, distinct, aggregates,
  # fragments, like. The way Datalog is comfortable with:
  #
  #   1. The first view and the leading run of inner/cross joins are ONE Datalog query. A join is
  #      a shared variable (`[?e1 :note/account_id ?e0]`), and every part of the `where` that
  #      Datalog can say is pushed into it. What comes back are tuples of entity ids.
  #   2. A left join, or a join after one, reads the records of its view once, narrowed by the keys
  #      of the tuples so far, and is joined here.
  #   3. The WHOLE where (and the ON of every inner join) is then checked again in Elixir with SQL's
  #      meaning (`Ecto.Hosi.Eval`): the pushdown only has to be a superset, never exact. What Datalog
  #      cannot say (fragment, like, coalesce) is decided here.
  #   4. group_by, having, aggregates, distinct, order_by, offset, limit and select are done here.
  #
  # A query that names no restricting `where` reads every record of its views: write it with
  # `Repo.datalog/1`
  # when that matters.

  alias Ecto.Hosi.{Backend, Eval, Model, Reader, Store, Types}

  @cmp_text %{==: "=", !=: "not=", <: "<", <=: "<=", >: ">", >=: ">="}
  @flip %{==: :==, !=: :!=, <: :>, <=: :>=, >: :<, >=: :<=}
  @chunk 500

  def all(meta, %Ecto.Query{} = query, params) do
    check!(query)
    bindings = bindings(meta, query)
    ctx = %{meta: meta, params: params, views: Map.new(bindings, &{&1.ix, &1.view})}
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

  defp bindings(meta, query) do
    [
      %{ix: 0, view: view!(meta, query.from.source), qual: :from, on: nil}
      | for {j, ix} <- Enum.with_index(query.joins, 1) do
          unless j.qual in [:inner, :left, :cross] do
            unsupported("#{j.qual} join (only inner, left and cross)")
          end

          %{ix: ix, view: view!(meta, j.source), qual: j.qual, on: j.on.expr}
        end
    ]
  end

  defp view!(meta, {name, _} = source) when is_binary(name), do: Reader.view!(meta, source)

  defp view!(_meta, other),
    do: unsupported("this query source (#{inspect(other)}), a subquery or fragment")

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
    {request, _} = base_request(meta, bindings, conjuncts, params)
    rows = meta |> Backend.query(request) |> Backend.ok!()
    entities = load(meta, rows |> List.flatten() |> Enum.uniq())

    for tuple <- rows do
      for {id, b} <- Enum.zip(tuple, bindings), into: %{} do
        {b.ix, %{id: id, entity: Map.get(entities, id, %{})}}
      end
    end
  end

  # the Datalog request of the first bindings, and the conjuncts that went into it
  defp base_request(meta, bindings, conjuncts, params) do
    attrs_of = Map.new(bindings, &{&1.ix, &1.view})
    st = %{
      n: 0,
      inputs: [],
      params: params,
      span: length(bindings),
      views: attrs_of,
      model: meta.model,
      negated: false
    }

    {clauses, st, pushed} =
      Enum.reduce(conjuncts, {[], st, []}, fn c, {acc, st, pushed} ->
        try do
          {cl, st2} = push(c, st)
          {acc ++ cl, st2, pushed ++ [c]}
        catch
          {:no_push, _} -> {acc, st, pushed}
        end
      end)

    vars = for b <- bindings, do: "?e#{b.ix}"
    members = Enum.flat_map(bindings, &Reader.member_clauses(meta, &1.view, "?e#{&1.ix}"))
    inputs = Enum.reverse(st.inputs)
    specs = Enum.map(inputs, &elem(&1, 0))

    q =
      "[:find #{Enum.join(vars, " ")} :in #{Enum.join(["$" | specs], " ")} " <>
        ":where #{Enum.join(members ++ clauses, " ")}]"

    {%{q: q, inputs: Enum.map(inputs, &elem(&1, 1))}, pushed}
  end

  defp load(meta, ids), do: Map.new(Backend.read(meta, ids))

  @doc false
  # What a query becomes, without running it: the first Datalog request, the joins read after it,
  # and how much of the where went into Datalog.
  def plan(meta, %Ecto.Query{} = query, params) do
    check!(query)
    bindings = bindings(meta, query)
    span = span(bindings)
    {where, wheres} = where_parts(query)
    on_conj = bindings |> Enum.slice(1, span - 1) |> Enum.flat_map(&split_and(&1.on))
    conjuncts = wheres ++ on_conj
    {request, pushed} = base_request(meta, Enum.take(bindings, span), conjuncts, params)

    joins =
      for b <- Enum.drop(bindings, span) do
        {equi, _} = equi(split_and(b.on), b.ix)
        {b.view, b.qual, equi && elem(equi, 0)}
      end

    %{
      request: request,
      first: Enum.map(Enum.take(bindings, span), & &1.view),
      joins: joins,
      where: where,
      pushed: length(pushed),
      conjuncts: length(conjuncts)
    }
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
    {["[?e#{ix} #{Model.kw(attr(ix, field, st))} _]"], st}
  end

  # (inside a not, values are text: see `Ecto.Hosi.Reader`)
  defp push({:not, _, [inner]}, st) do
    {cl, inside} = push(inner, %{st | negated: true})
    text = Enum.join(cl, " ")
    joined = Regex.scan(~r/\?e\d+/, text) |> List.flatten() |> Enum.uniq()
    {["(not-join [#{Enum.join(joined, " ")}] #{text})"], %{inside | negated: st.negated}}
  end

  defp push({:is_nil, _, [f]}, st) do
    {:field, ix, field} = classify(f, st)
    {["(not-join [?e#{ix}] [?e#{ix} #{Model.kw(attr(ix, field, st))} _])"], st}
  end

  defp push({:in, _, [l, r]}, st) do
    case {classify(l, st), classify(r, st)} do
      {{:value, v}, {:field, ix, f}} ->
        attr = attr(ix, f, st)
        unless match?(%{card: :many}, st.model.attrs[attr]), do: throw({:no_push, :in})
        {var, st} = input(:scalar, Store.out(st.model, attr, v), st)
        {["[?e#{ix} #{Model.kw(attr)} #{var}]"], st}

      {{:field, ix, f}, {kind, vs}} when kind in [:list, :value] ->
        case vs |> Types.unpack_list() |> List.wrap() do
          [] ->
            {["[(= 0 1)]"], st}

          vs ->
            one_of(ix, f, store(st, ix, f, vs), st)
        end

      _ ->
        throw({:no_push, :in})
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

  # the field is one of the values
  defp one_of(ix, f, values, %{negated: false} = st) do
    {var, st} = input(:coll, values, st)

    if f == :id,
      do: {["[(= ?e#{ix} #{var})]"], st},
      else: {["[?e#{ix} #{Model.kw(attr(ix, f, st))} #{var}]"], st}
  end

  # (too long to write into the query: it is only checked here, after the read)
  defp one_of(ix, f, values, st) do
    if length(values) > Reader.inline_limit(), do: throw({:no_push, :long_list})
    {var, clauses, st} = field_var(ix, f, st)
    {clauses ++ [Reader.any_of(var, values)], st}
  end

  # `a.f == b.g`: the two sides share one variable, which is what a join is
  defp join_eq(i, :id, j, :id, st), do: {["[(= ?e#{i} ?e#{j})]"], st}
  defp join_eq(i, :id, j, g, st), do: {["[?e#{j} #{Model.kw(attr(j, g, st))} ?e#{i}]"], st}
  defp join_eq(i, f, j, :id, st), do: {["[?e#{i} #{Model.kw(attr(i, f, st))} ?e#{j}]"], st}

  defp join_eq(i, f, j, g, st) do
    var = "?j#{st.n}"

    {["[?e#{i} #{Model.kw(attr(i, f, st))} #{var}]", "[?e#{j} #{Model.kw(attr(j, g, st))} #{var}]"],
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
    {["[?e#{ix} #{Model.kw(attr(ix, f, st))} #{var}]"], st}
  end

  defp field_vs_value(_op, _ix, _f, nil, _st), do: throw({:no_push, :nil_value})

  defp field_vs_value(op, ix, f, v, st) do
    {lv, clauses, st} = field_var(ix, f, st)
    {var, st} = input(:scalar, store(st, ix, f, v), st)
    {clauses ++ ["[(#{@cmp_text[op]} #{lv} #{var})]"], st}
  end

  # a value as hosigraph keeps it for the field (a keyword, a UUID, a list kept as one text)
  defp store(_st, _ix, :id, v), do: v

  defp store(st, ix, f, vs) when is_list(vs) do
    attr = attr(ix, f, st)

    case st.model.attrs[attr] do
      %{enc: {:array, _}, card: :one} -> throw({:no_push, {:list, attr}})
      _ -> Store.out(st.model, attr, vs)
    end
  end

  defp store(st, ix, f, v), do: Store.out(st.model, attr(ix, f, st), v)

  defp field_var(ix, :id, st), do: {"?e#{ix}", [], st}

  defp field_var(ix, f, st) do
    var = "?v#{st.n}"
    {var, ["[?e#{ix} #{Model.kw(attr(ix, f, st))} #{var}]"], %{st | n: st.n + 1}}
  end

  # a value of the query: an input, or inside a `not` its text
  defp input(:scalar, value, %{negated: true} = st), do: {Reader.literal(value), st}

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

  # the ix of a binding past the span can not appear in a pushed clause (a field of a query
  # expression is its attribute)
  defp attr(ix, f, %{span: span}) when ix >= span, do: throw({:no_push, {:outside, ix, f}})
  defp attr(_ix, f, _st), do: f

  ## ---------------------------------------------------------------- joins past the Datalog span

  defp join(meta, envs, %{ix: ix, view: view, qual: qual, on: on}, ctx) do
    conj = split_and(on)
    {equi, rest} = equi(conj, ix)

    index =
      case equi do
        nil -> all_rows(meta, view)
        {f, expr} -> rows_by(meta, view, f, keys(envs, expr, ctx))
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

  defp all_rows(meta, view) do
    clauses = Enum.join(Reader.member_clauses(meta, view, "?e"), " ")
    req = %{q: "[:find ?e :in $ :where #{clauses}]", inputs: []}
    ids = meta |> Backend.query(req) |> Backend.ok!() |> Enum.map(fn [id] -> id end)
    entities = load(meta, ids)
    # no key to look up by: every record is a candidate for every tuple
    for id <- Enum.sort(ids), do: %{id: id, entity: entities[id]}
  end

  # %{key => [row]} of the records whose `f` is one of `keys`
  defp rows_by(_meta, _view, _f, []), do: %{}

  defp rows_by(meta, view, :id, keys) do
    for {id, entity} <- Backend.read(meta, Enum.filter(keys, &is_integer/1)),
        Model.member?(view, entity),
        into: %{} do
      {id, [%{id: id, entity: entity}]}
    end
  end

  defp rows_by(meta, view, attr, keys) do
    members = Enum.join(Reader.member_clauses(meta, view, "?e"), " ")

    pairs =
      keys
      |> Enum.chunk_every(@chunk)
      |> Enum.flat_map(fn chunk ->
        req = %{
          q: "[:find ?e ?v :in $ [?v ...] :where #{members} [?e #{Model.kw(attr)} ?v]]",
          inputs: [[:coll, Store.out(meta.model, attr, chunk)]]
        }

        meta |> Backend.query(req) |> Backend.ok!()
      end)

    entities = load(meta, pairs |> Enum.map(&hd/1) |> Enum.uniq())

    pairs
    |> Enum.sort()
    |> Enum.group_by(
      fn [_e, v] -> Store.back(meta.model, attr, v) end,
      fn [e, _v] -> %{id: e, entity: entities[e]} end
    )
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
    value = Eval.eval_group(expr, g, ctx)

    case ctx.meta.model.attrs[f] do
      # the many values of a field (none: an empty set; a left join that found nothing: nil)
      %{card: :many} -> value
      _ -> Types.read(ctx.views[ix].types[f], value)
    end
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
          "Ecto.Adapters.Hosi does not support #{what} in Ecto queries (ecto_hosi's limit); " <>
            "write it as a Datalog query with Repo.datalog/1"
  end
end
