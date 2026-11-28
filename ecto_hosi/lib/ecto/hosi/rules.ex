defmodule Ecto.Hosi.Rules do
  @moduledoc """
  Rules: named relations derived from facts, recursion included.

      defmodule MyApp.Rules do
        use Ecto.Hosi.Rules

        # a reaches b if a follows b, or follows someone who reaches b
        rule :reachable, [a, b] do
          match MyApp.Follow, from: a, to: b
        end

        rule :reachable, [a, c] do
          match MyApp.Follow, from: a, to: b
          reachable(b, c)
        end
      end

  Rules are sent to hosigraph as the rules input of a query (see `Ecto.Hosi.Query.datalog/1`).

  Checked when the module is compiled:

    * every variable in a rule's head, in a comparison, or in a `not` is bound by a `match` or a call;
    * a rule is always called with the same number of arguments;
    * **stratification**: no rule may reach itself through a `not`. Such a rule has no single answer.
  """

  alias Ecto.Hosi.Goal

  defmacro __using__(_opts) do
    quote do
      import Ecto.Hosi.Rules, only: [rule: 3]
      Module.register_attribute(__MODULE__, :hosi_rules, accumulate: true)
      @before_compile Ecto.Hosi.Rules
    end
  end

  @doc "Define one clause of a rule."
  defmacro rule(name, args, do: body) when is_atom(name) and is_list(args) do
    head = Enum.map(args, &Goal.var_name!/1)
    goals = Goal.parse_body(body, __CALLER__, false)
    escaped = Macro.escape({name, head, goals})

    quote do
      Module.put_attribute(__MODULE__, :hosi_rules, unquote(escaped))
    end
  end

  defmacro __before_compile__(env) do
    rules = env.module |> Module.get_attribute(:hosi_rules) |> Enum.reverse()
    check!(rules, env.module)
    # Rendering once checks every field name against its schema while compiling.
    Enum.each(rules, &Goal.render_rule(&1, nil))
    escaped = Macro.escape(rules)

    quote do
      @doc false
      def __hosi_rules__, do: unquote(escaped)
    end
  end

  @doc false
  def check!(rules, module) do
    arities = rules |> Enum.group_by(&elem(&1, 0), fn {_, head, _} -> length(head) end)

    for {name, list} <- arities, length(Enum.uniq(list)) > 1 do
      raise ArgumentError,
            "#{inspect(module)}: rule #{name} is defined with different numbers of arguments"
    end

    for {name, head, goals} <- rules do
      if length(Enum.uniq(head)) != length(head) do
        raise ArgumentError,
              "#{inspect(module)}: the head of rule #{name} repeats a variable; " <>
                "use a comparison (a == b) for that"
      end

      for {callee, n} <- calls(goals), Map.has_key?(arities, callee) do
        unless n in arities[callee] do
          raise ArgumentError,
                "#{inspect(module)}: rule #{name} calls #{callee} with #{n} arguments, " <>
                  "but #{callee} takes #{inspect(arities[callee])}"
        end
      end

      bound = bound_vars(goals)
      needed = head ++ Enum.flat_map(goals, &needs_binding/1)

      for v <- Enum.uniq(needed), v not in bound do
        raise ArgumentError,
              "#{inspect(module)}: in rule #{name}, the variable #{v} is not bound by any match or call " <>
                "(heads, comparisons and `not` can only use variables bound elsewhere)"
      end
    end

    check_stratified!(rules, module)
  end

  # variables a positive match or call binds
  defp bound_vars(goals) do
    goals
    |> Enum.flat_map(fn
      {:match, _, _} = g -> Goal.vars_of(g)
      {:call, _, _} = g -> Goal.vars_of(g)
      _ -> []
    end)
    |> Enum.uniq()
  end

  defp needs_binding({:cmp, _, _, _} = g), do: Goal.vars_of(g)
  defp needs_binding({:not, gs}), do: Enum.flat_map(gs, &Goal.vars_of/1)
  defp needs_binding(_), do: []

  defp calls(goals) do
    Enum.flat_map(goals, fn
      {:call, n, ts} -> [{n, length(ts)}]
      {:not, gs} -> calls(gs)
      _ -> []
    end)
  end

  defp edges({name, _, goals}), do: edges(name, goals, :pos)

  defp edges(from, goals, polarity) do
    Enum.flat_map(goals, fn
      {:call, to, _} -> [{from, to, polarity}]
      {:not, gs} -> edges(from, gs, :neg)
      _ -> []
    end)
  end

  defp check_stratified!(rules, module) do
    all = Enum.flat_map(rules, &edges/1)

    for {from, to, :neg} <- all, reaches?(all, to, from, MapSet.new()) do
      raise ArgumentError,
            "#{inspect(module)}: rule #{from} reaches itself through `not` (via #{to}); " <>
              "a rule that depends on its own negation has no single answer (not stratified)"
    end

    :ok
  end

  defp reaches?(_edges, same, same, _seen), do: true

  defp reaches?(edges, from, to, seen) do
    if from in seen do
      false
    else
      seen = MapSet.put(seen, from)
      Enum.any?(edges, fn {a, b, _} -> a == from and reaches?(edges, b, to, seen) end)
    end
  end
end
