defmodule Mix.Tasks.Hosi.Attributes do
  @shortdoc "Lists the attributes of a Repo's views, their types and rules"

  @moduledoc """
  Every attribute the views of a Repo declare: its type, its rules, and the fields that are it.

      mix hosi.attributes                 # the application's Ecto repos
      mix hosi.attributes -r MyApp.Repo

  It reads the declarations only: the database is not opened.
  """

  use Mix.Task

  alias Ecto.Hosi.Model

  @impl Mix.Task
  def run(args) do
    for repo <- Mix.Ecto.parse_repo(args) do
      Mix.Ecto.ensure_repo(repo, args)

      if repo.__adapter__() == Ecto.Adapters.Hosi do
        model = repo.config() |> Keyword.get(:schemas, []) |> Model.build()
        Mix.shell().info(["#{inspect(repo)}\n\n", report(model)])
      else
        Mix.shell().info("#{inspect(repo)}: not a hosigraph Repo, skipped")
      end
    end
  end

  @doc "The text of the list."
  def report(%Model{attrs: attrs, views: views, rules: rules}) do
    rows =
      for {attr, spec} <- Enum.sort(attrs) do
        {Model.kw(attr), type(spec), Enum.join(said(spec, rules), ", "), fields(views, attr)}
      end

    wide = &(rows |> Enum.map(fn row -> row |> elem(&1) |> String.length() end) |> Enum.max(fn -> 0 end))
    {names, types} = {wide.(0), wide.(1)}

    Enum.map(rows, fn {name, type, said, fields} ->
      line = "#{String.pad_trailing(name, names)}  #{String.pad_trailing(type, types)}  #{said}"
      [String.trim_trailing(line), "\n", Enum.map(fields, &["    ", &1, "\n"])]
    end)
  end

  # as hosigraph keeps it, and the Ecto type where that says more (a time kept as a long)
  defp type(%{made: made}) when made in [:tuple, :index], do: "tuple"

  defp type(%{card: card, type: type, ecto: ecto}) do
    many = if card == :many, do: "many ", else: ""

    as =
      if type in [:ref, :keyword] or ecto in [nil, :string, :integer, :id, :boolean, :float, Ecto.UUID],
        do: "",
        else: " (#{inspect(ecto)})"

    "#{many}#{type}#{as}"
  end

  defp said(%{made: :tuple, tuple: parts}, _rules), do: ["unique over #{names(parts)}"]
  defp said(%{made: :index, tuple: parts}, _rules), do: ["index over #{names(parts)} (hosi_tuple)"]

  defp said(%{made: :derived, attr: attr}, rules) do
    rule = Enum.find(rules, &(&1.attr == attr))
    ["unique over #{names(rule.attrs)}, kept by the adapter (#{rule.name})"]
  end

  defp said(spec, _rules) do
    Enum.reject(
      [
        spec.keyword && "one of #{spec.keyword |> Map.keys() |> Enum.sort() |> Enum.join(" | ")}",
        spec.unique && "unique",
        spec.final && "final",
        spec.record_type && "record type",
        spec.file && "file",
        spec.on_delete && "on delete: #{spec.on_delete}",
        spec.attr == Model.owner() && "the maker of a record (hosi_owner)"
      ],
      &(&1 in [nil, false])
    )
  end

  defp names(attrs), do: Enum.map_join(attrs, " + ", &Model.kw/1)

  defp fields(views, attr) do
    for {module, view} <- Enum.sort(views),
        field = view.field_of[attr],
        do: "#{inspect(module)}.#{field}"
  end
end
