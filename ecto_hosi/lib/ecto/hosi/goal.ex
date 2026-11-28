defmodule Ecto.Hosi.Goal do
  @moduledoc false
  # The small language shared by rule bodies and `datalog` queries.
  #
  #   match Follow, from: a, to: b     a record the view shows; a, b are variables
  #   reachable(b, c)                  a call of a rule
  #   a != c                           == != < <= > >=
  #   not match(Follow, from: a, to: c)   negation (one goal)
  #
  # Terms: a bare name is a variable, `_` is anonymous, a number/string/atom is a literal,
  # and `^expr` (queries only) is a value taken from the surrounding Elixir code.
  #
  # Parsing happens at compile time and yields plain data; rendering to EDN text happens when
  # a request is made, with the model of the Repo that is asked: `match` then also asks for the
  # facts that make a record show up in the view, and a literal or `^value` next to a field is
  # written as hosigraph keeps that field (a keyword, a UUID, a time as a number).

  alias Ecto.Hosi.{Model, Reader, Store, Types}

  @cmp_ops [:==, :!=, :<, :<=, :>, :>=]
  @cmp_text %{==: "=", !=: "not=", <: "<", <=: "<=", >: ">", >=: ">="}

  ## ------------------------------------------------------------ parsing

  def parse_body({:__block__, _, stmts}, env, params?),
    do: Enum.map(stmts, &parse_goal(&1, env, params?))

  def parse_body(list, env, params?) when is_list(list),
    do: Enum.map(list, &parse_goal(&1, env, params?))

  def parse_body(stmt, env, params?), do: [parse_goal(stmt, env, params?)]

  def var_name!({name, _, ctx}) when is_atom(name) and is_atom(ctx), do: Atom.to_string(name)

  def var_name!(other),
    do: raise(ArgumentError, "expected a variable name, got: #{Macro.to_string(other)}")

  defp parse_goal({:match, _, [schema, binds]}, env, p) when is_list(binds) do
    {:match, expand_schema(schema, env), Enum.map(binds, &parse_bind(&1, p))}
  end

  defp parse_goal({:match, _, [schema]}, env, _p), do: {:match, expand_schema(schema, env), []}
  defp parse_goal({:not, _, [goal]}, env, p), do: {:not, [parse_goal(goal, env, p)]}

  defp parse_goal({op, _, [l, r]}, _env, p) when op in @cmp_ops,
    do: {:cmp, op, parse_term(l, p), parse_term(r, p)}

  defp parse_goal({name, _, args}, _env, p) when is_atom(name) and is_list(args),
    do: {:call, name, Enum.map(args, &parse_term(&1, p))}

  defp parse_goal(other, _env, _p),
    do: raise(ArgumentError, "cannot read this as a goal: #{Macro.to_string(other)}")

  defp parse_bind({field, term}, p) when is_atom(field), do: {field, parse_term(term, p)}

  defp parse_bind(other, _p),
    do: raise(ArgumentError, "match expects `field: term` pairs, got: #{Macro.to_string(other)}")

  defp expand_schema(ast, env) do
    case Macro.expand(ast, env) do
      mod when is_atom(mod) -> mod
      _ -> raise ArgumentError, "match expects a schema module, got: #{Macro.to_string(ast)}"
    end
  end

  defp parse_term({:^, _, [expr]}, true), do: {:param, expr}

  defp parse_term({:^, _, _}, false),
    do: raise(ArgumentError, "^values are only allowed in queries, not inside a rule")

  defp parse_term({:_, _, ctx}, _) when is_atom(ctx), do: :wild

  defp parse_term({name, _, ctx}, _) when is_atom(name) and is_atom(ctx),
    do: {:var, Atom.to_string(name)}

  defp parse_term(v, _) when is_number(v) or is_binary(v) or is_atom(v), do: {:lit, v}

  defp parse_term(other, _),
    do: raise(ArgumentError, "cannot read this as a term: #{Macro.to_string(other)}")

  @doc "Parsed goals -> quoted data (so `^expr` is evaluated in the caller)."
  def quote_goals(goals), do: Enum.map(goals, &q_goal/1)

  defp q_goal({:match, mod, binds}) do
    b = for {f, t} <- binds, do: quote(do: {unquote(f), unquote(q_term(t))})
    quote(do: {:match, unquote(mod), unquote(b)})
  end

  defp q_goal({:not, gs}), do: quote(do: {:not, unquote(Enum.map(gs, &q_goal/1))})

  defp q_goal({:cmp, op, l, r}),
    do: quote(do: {:cmp, unquote(op), unquote(q_term(l)), unquote(q_term(r))})

  defp q_goal({:call, n, ts}),
    do: quote(do: {:call, unquote(n), unquote(Enum.map(ts, &q_term/1))})

  defp q_term({:param, expr}), do: quote(do: {:param, unquote(expr)})
  defp q_term(other), do: Macro.escape(other)

  ## ------------------------------------------------------------ rendering

  # `model` is nil while a rule module is compiled: only the field names are looked at then
  defp new_state(model),
    do: %{n: 0, inputs: [], vars: %{}, model: model, negated: false, extra: []}

  @doc "One rule as EDN text: `[(reachable ?a ?b) [?m :from ?a] ...]`."
  def render_rule({name, head, goals}, model \\ nil) do
    # Datalevin refuses to join the clauses of one rule when their heads name the variables
    # differently ("Can't sum relations with different attrs"), so the head variables are renamed
    # to ?__h0, ?__h1, ... in the head and everywhere in the body.
    names = head |> Enum.with_index() |> Map.new(fn {v, i} -> {v, "?__h#{i}"} end)
    {clauses, _} = render_goals(goals, %{new_state(model) | vars: names})
    "[(#{name} #{Enum.map_join(head, " ", &Map.fetch!(names, &1))}) #{Enum.join(clauses, " ")}]"
  end

  @doc "All rules of the given modules as one EDN vector."
  def rules_text(mods, model) do
    body = mods |> Enum.flat_map(& &1.__hosi_rules__()) |> Enum.map_join(" ", &render_rule(&1, model))
    "[" <> body <> "]"
  end

  @doc "A query as the request hosigraph takes: `%{q: text, inputs: [...]}`."
  def render_query(find, goals, rule_mods, model) do
    {clauses, st} = render_goals(goals, new_state(model))
    inputs = Enum.reverse(st.inputs)
    specs = Enum.map(inputs, &elem(&1, 0))
    values = Enum.map(inputs, &elem(&1, 1))

    {specs, values} =
      if rule_mods == [],
        do: {specs, values},
        else: {specs ++ ["%"], values ++ [[:rules, rules_text(rule_mods, model)]]}

    q =
      "[:find #{Enum.map_join(find, " ", &("?" <> &1))} " <>
        ":in #{Enum.join(["$" | specs], " ")} :where #{Enum.join(clauses, " ")}]"

    %{q: q, inputs: values}
  end

  defp render_goals(goals, st) do
    {lists, st} = Enum.map_reduce(goals, st, &render_goal/2)
    {List.flatten(lists), st}
  end

  defp render_goal({:match, mod, binds}, st) do
    Code.ensure_compiled!(mod)
    view = st.model && Model.view(st.model, mod)
    {id_binds, others} = Enum.split_with(binds, fn {f, _} -> f == :id end)

    {ent, st} =
      case id_binds do
        [{:id, t}] -> field_term(t, mod, :id, st)
        [] -> {"?__e#{st.n}", %{st | n: st.n + 1}}
      end

    {patterns, st} =
      Enum.map_reduce(others, st, fn {field, t}, st ->
        unless mod.__schema__(:type, field) do
          raise ArgumentError, "#{inspect(mod)} has no field #{inspect(field)}"
        end

        {text, st} = field_term(t, mod, field, st)
        {"[#{ent} #{Model.kw(mod.__schema__(:field_source, field))} #{text}]", st}
      end)

    members = if view, do: Reader.member_clauses(%{model: st.model}, view, ent), else: []
    # (a field that hosi_where also asks for is said once)
    {Enum.uniq(members ++ patterns) ++ Enum.reverse(st.extra), %{st | extra: []}}
  end

  defp render_goal({:cmp, op, l, r}, st) do
    {lt, st} = term(l, st)
    {rt, st} = term(r, st)
    {["[(#{@cmp_text[op]} #{lt} #{rt})]"], st}
  end

  defp render_goal({:call, name, terms}, st) do
    {texts, st} = Enum.map_reduce(terms, st, &term/2)
    {["(#{name} #{Enum.join(texts, " ")})"], st}
  end

  # Inside a `not`, values are written into the query's text (see `Ecto.Hosi.Reader`).
  defp render_goal({:not, goals}, st) do
    join = goals |> goal_vars() |> Enum.map(&rename(&1, st.vars)) |> Enum.uniq()

    if join == [] do
      raise ArgumentError,
            "a `not` needs a variable that the rest of the query binds: Datalog says \"not\" " <>
              "about something (hosigraph's limit)"
    end

    {inner, inside} = render_goals(goals, %{st | negated: true})
    {["(not-join [#{Enum.join(join, " ")}] #{Enum.join(inner, " ")})"], %{inside | negated: st.negated}}
  end

  defp term({:var, n}, st), do: {Map.get(st.vars, n, "?" <> n), st}
  defp term(:wild, st), do: {"_", st}
  defp term({:lit, v}, st), do: {literal(v), st}

  # (a value with no field to say what it is: times, dates and decimals as hosigraph keeps them)
  defp term({:param, v}, %{negated: true}) when is_list(v) do
    raise ArgumentError,
          "a list inside `not` goes next to a field of a match (not match(View, field: ^list))"
  end

  defp term({:param, v}, st) when is_list(v), do: param(:coll, Enum.map(v, &Types.raw_dump/1), st)
  defp term({:param, v}, st), do: param(:scalar, Types.raw_dump(v), st)

  defp param(:scalar, value, %{negated: true} = st), do: {literal(value), st}

  # a list inside a `not`: the field is a variable that has to be one of them
  defp param(:coll, values, %{negated: true} = st) do
    if length(values) > Reader.inline_limit() do
      raise ArgumentError,
            "a list of #{length(values)} values inside `not`: Datalevin cannot compile a query " <>
              "of that size (hosigraph's limit: #{Reader.inline_limit()} here)"
    end

    var = "?__n#{st.n}"
    clause = if values == [], do: "[(= 0 1)]", else: Reader.any_of(var, values)
    {var, %{st | n: st.n + 1, extra: [clause | st.extra]}}
  end

  defp param(kind, value, st) do
    var = "?__p#{st.n}"
    spec = if kind == :coll, do: "[#{var} ...]", else: var
    {var, %{st | n: st.n + 1, inputs: [{spec, [kind, value]} | st.inputs]}}
  end

  # a literal or a `^value` next to a field of a view
  defp field_term({kind, value}, mod, field, %{model: model} = st)
       when kind in [:lit, :param] and model != nil do
    attr = if field == :id, do: nil, else: mod.__schema__(:field_source, field)
    spec = attr && model.attrs[attr]
    type = mod.__schema__(:type, field)

    {type, list?} =
      case {spec, type} do
        {%{card: :many}, {:array, inner}} -> {inner, is_list(value)}
        # a list kept as one text: a list is one value, a list of lists several
        {%{enc: {:array, _}}, _} -> {type, is_list(value) and value != [] and Enum.all?(value, &is_list/1)}
        _ -> {type, is_list(value)}
      end

    stored = fn v -> stored(model, mod, field, attr, spec, type, v) end

    case {kind, list?} do
      {:lit, _} -> {literal(stored.(value)), st}
      {:param, true} -> param(:coll, Enum.map(value, stored), st)
      {:param, false} -> param(:scalar, stored.(value), st)
    end
  end

  defp field_term(other, _mod, _field, st), do: term(other, st)

  # a keyword may be written as one
  defp stored(_model, _mod, _field, _attr, %{keyword: names}, _type, value)
       when names != nil and is_atom(value) and value not in [nil, true, false],
       do: value

  defp stored(model, mod, field, attr, spec, type, value) do
    with {:ok, cast} <- Ecto.Type.cast(type, value),
         {:ok, dumped} <- Ecto.Type.adapter_dump(Ecto.Adapters.Hosi, type, cast) do
      cond do
        attr == nil -> dumped
        spec.card == :many -> Store.out(model, attr, dumped)
        true -> Store.out(model, attr, Store.normalize(model, attr, dumped))
      end
    else
      _ ->
        raise ArgumentError,
              "match #{inspect(mod)}, #{field}: #{inspect(value)} is not a value of the field"
    end
  end

  defp rename("?" <> n = var, vars), do: Map.get(vars, n, var)

  defp literal(v), do: Reader.literal(v)

  ## ------------------------------------------------------------ inspection (used by the checks)

  @doc "Names of the variables a list of goals mentions, as `\"?a\"`."
  def goal_vars(goals) do
    goals |> Enum.flat_map(&vars_of/1) |> Enum.uniq() |> Enum.map(&("?" <> &1))
  end

  def vars_of({:match, _, binds}), do: Enum.flat_map(binds, fn {_, t} -> term_vars(t) end)
  def vars_of({:cmp, _, l, r}), do: term_vars(l) ++ term_vars(r)
  def vars_of({:call, _, ts}), do: Enum.flat_map(ts, &term_vars/1)
  def vars_of({:not, gs}), do: Enum.flat_map(gs, &vars_of/1)

  defp term_vars({:var, n}), do: [n]
  defp term_vars(_), do: []
end
