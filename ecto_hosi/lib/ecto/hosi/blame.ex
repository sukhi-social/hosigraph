defmodule Ecto.Hosi.Blame do
  @moduledoc false
  # Which step of a `write_batch` hosigraph's refusal is about. The write goes in as one list of
  # ops, so the step is found again from what the answer names: the failed `when:` conditions, or
  # the attribute and value of the unique rule. `:batch` when the answer does not tell.

  alias Ecto.Hosi.{Backend, Kw}

  @doc "A condition did not hold: the message is the failed conditions, as EDN."
  def conflict(meta, steps, message) do
    kws = Backend.keywords(meta)

    with {:ok, failed} when is_list(failed) <- Hosi.Edn.decode(message),
         %{name: name} <-
           Enum.find(steps, fn step -> Enum.any?(Kw.conds(kws, step.conds), &(&1 in failed)) end) do
      name
    else
      _ -> :batch
    end
  end

  # the words of Datalevin 1.2.10, as hosigraph passes them on
  @unique ~r/#datalevin\/Datom \[\d+ :(\S+) (.*?)\] because of unique constraint/s

  @doc "A unique rule refused a value."
  def rejected(meta, steps, message) do
    case Regex.run(@unique, message) do
      [_, attr, value] ->
        kws = Backend.keywords(meta)
        written = Enum.map(steps, &{&1.name, written(Kw.ops(kws, &1.ops))})
        parts = parts(meta, attr)

        # steps that write a part of the rule, and among them those that write just this value
        touching = Enum.filter(written, fn {_, facts} -> Enum.any?(parts, &Map.has_key?(facts, &1)) end)
        exact = Enum.filter(touching, fn {_, facts} -> value_of(facts, attr, parts) == value end)

        # of two steps with one value, the later is the one that could not be added
        case {exact, touching} do
          {[_ | _], _} -> exact |> List.last() |> elem(0)
          {[], [{name, _}]} -> name
          _ -> :batch
        end

      nil ->
        :batch
    end
  end

  # `%{attribute name => value}` of what a step creates
  defp written(ops),
    do: for([:create, _, attr, value] <- ops, into: %{}, do: {Atom.to_string(attr), value})

  # the attributes a rule is made of: a tuple's parts, or the attribute itself
  defp parts(meta, attr) do
    meta.derived
    |> Map.values()
    |> Enum.concat()
    |> Enum.find_value([attr], fn spec ->
      if spec.native and Atom.to_string(spec.attr) == attr,
        do: Enum.map(spec.attrs, &Atom.to_string/1)
    end)
  end

  # the value as the message writes it
  defp value_of(facts, attr, [attr]), do: edn(Map.get(facts, attr))

  defp value_of(facts, _tuple, parts) do
    if Enum.all?(parts, &Map.has_key?(facts, &1)), do: edn(Enum.map(parts, &facts[&1]))
  end

  defp edn(value), do: Hosi.Edn.encode(value)
end
