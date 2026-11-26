defmodule Ecto.Hosi.Goal do
  @moduledoc false
  # The small language shared by rule bodies and `datalog` queries.
  #
  #   match Follow, from: a, to: b     a row of the schema; a, b are variables
  #   reachable(b, c)                  a call of a rule
  #   a != c                           == != < <= > >=
  #   not match(Follow, from: a, to: c)   negation (one goal)
  #
  # Terms: a bare name is a variable, `_` is anonymous, a number/string/atom is a literal,
  # and `^expr` (queries only) is a value taken from the surrounding Elixir code.
  #
  # Parsing happens at compile time and yields plain data; rendering to EDN text happens when
  # a request is made, so a schema is looked up only then.

  alias Ecto.Hosi.Attr

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

  defp new_state, do: %{n: 0, inputs: [], vars: %{}}

  @doc "One rule as EDN text: `[(reachable ?a ?b) [?m :follow/from ?a] ...]`."
  def render_rule({name, head, goals}) do
    # Datalevin refuses to join the clauses of one rule when their heads name the variables
    # differently ("Can't sum relations with different attrs"), so the head variables are renamed
    # to ?__h0, ?__h1, ... in the head and everywhere in the body.
    names = head |> Enum.with_index() |> Map.new(fn {v, i} -> {v, "?__h#{i}"} end)
    {clauses, _} = render_goals(goals, %{new_state() | vars: names})
    "[(#{name} #{Enum.map_join(head, " ", &Map.fetch!(names, &1))}) #{Enum.join(clauses, " ")}]"
  end

  @doc "All rules of the given modules as one EDN vector."
  def rules_text(mods) do
    body = mods |> Enum.flat_map(& &1.__hosi_rules__()) |> Enum.map_join(" ", &render_rule/1)
    "[" <> body <> "]"
  end

  @doc "A query as the request hosigraph takes: `%{q: text, inputs: [...]}`."
  def render_query(find, goals, rule_mods) do
    {clauses, st} = render_goals(goals, new_state())
    inputs = Enum.reverse(st.inputs)
    specs = Enum.map(inputs, &elem(&1, 0))
    values = Enum.map(inputs, &elem(&1, 1))

    {specs, values} =
      if rule_mods == [],
        do: {specs, values},
        else: {specs ++ ["%"], values ++ [[:rules, rules_text(rule_mods)]]}

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
    source = mod.__schema__(:source)
    {id_binds, others} = Enum.split_with(binds, fn {f, _} -> f == :id end)

    {ent, st} =
      case id_binds do
        [{:id, t}] -> term(t, st)
        [] -> {"?__e#{st.n}", %{st | n: st.n + 1}}
      end

    {patterns, st} =
      Enum.map_reduce(others, st, fn {field, t}, st ->
        unless mod.__schema__(:type, field) do
          raise ArgumentError, "#{inspect(mod)} has no field #{inspect(field)}"
        end

        {text, st} = term(t, st)
        {"[#{ent} #{Attr.kw(Attr.attr(source, field))} #{text}]", st}
      end)

    patterns = if others == [], do: ["[#{ent} #{Attr.kw(Attr.marker(source))} _]"], else: patterns
    {patterns, st}
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

  defp render_goal({:not, goals}, st) do
    before = length(st.inputs)
    {inner, st2} = render_goals(goals, st)
    fresh = st2.inputs |> Enum.take(length(st2.inputs) - before) |> Enum.map(&elem(&1, 0))
    param_vars = Regex.scan(~r/\?__p\d+/, Enum.join(fresh, " ")) |> List.flatten()
    join = Enum.uniq(Enum.map(goal_vars(goals), &rename(&1, st.vars)) ++ param_vars)
    {["(not-join [#{Enum.join(join, " ")}] #{Enum.join(inner, " ")})"], st2}
  end

  defp term({:var, n}, st), do: {Map.get(st.vars, n, "?" <> n), st}
  defp term(:wild, st), do: {"_", st}
  defp term({:lit, v}, st), do: {literal(v), st}

  defp term({:param, v}, st) do
    var = "?__p#{st.n}"
    {spec, input} = if is_list(v), do: {"[#{var} ...]", [:coll, v]}, else: {var, [:scalar, v]}
    {var, %{st | n: st.n + 1, inputs: [{spec, input} | st.inputs]}}
  end

  defp rename("?" <> n = var, vars), do: Map.get(vars, n, var)

  defp literal(v), do: v |> Hosi.Edn.encode() |> IO.iodata_to_binary()

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
