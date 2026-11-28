defmodule Ecto.Hosi.Query do
  @moduledoc """
  Datalog queries written the Ecto way.

      import Ecto.Hosi.Query

      query =
        datalog find: [b],
                where: [reachable(^me, b)],
                rules: MyApp.Rules

      MyApp.Repo.datalog(query)   #=> [[12], [47]]

  `find:` lists variables, `where:` lists goals (see `Ecto.Hosi.Rules`; `^value` takes a value from
  your code) and `rules:` the rule modules the goals may call.

  `match View, field: term` finds the records the view shows (`hosi_where`), and a literal or a
  `^value` next to a field is the field's value in Elixir: `match Note, visibility: "public"`.
  """

  alias Ecto.Hosi.Goal

  defstruct find: [], goals: [], rules: []

  defmacro datalog(opts) do
    find = opts |> Keyword.fetch!(:find) |> List.wrap() |> Enum.map(&Goal.var_name!/1)

    goals =
      opts |> Keyword.fetch!(:where) |> Goal.parse_body(__CALLER__, true) |> Goal.quote_goals()

    rules =
      opts |> Keyword.get(:rules, []) |> List.wrap() |> Enum.map(&Macro.expand(&1, __CALLER__))

    quote do
      %Ecto.Hosi.Query{find: unquote(find), goals: unquote(goals), rules: unquote(rules)}
    end
  end

  @doc "The request hosigraph takes for this query on `repo`: `%{q: text, inputs: [...]}`."
  def to_request(%__MODULE__{} = query, repo),
    do: render(query, Ecto.Adapter.lookup_meta(repo.get_dynamic_repo()).model)

  @doc false
  def render(%__MODULE__{find: find, goals: goals, rules: rules}, model),
    do: Goal.render_query(find, goals, rules, model)
end
