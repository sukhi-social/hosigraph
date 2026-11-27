defmodule Ecto.Hosi.Check do
  @moduledoc false
  # Two mistakes in a Datalog request that hosigraph does not say well:
  #
  #   * a misspelt attribute finds nothing (`[]`), or, inside `not`, everything;
  #   * a string where a keyword is kept is a Java cast error.
  #
  # An attribute is called misspelt only if no schema declares it, **no record has it**, and it
  # looks like a slip: it is under a name the schemas use (`user/..`), or very close to a declared
  # one. So a query that could find something is never refused. `Repo.datalog(query, check: false)`
  # leaves everything to hosigraph.
  #
  # A request this module cannot read is left to hosigraph too.

  alias Ecto.Hosi.{Attr, Backend}

  @close 0.9

  def request!(meta, %{q: text} = request) do
    case Hosi.Edn.decode(text) do
      {:ok, form} ->
        sections = sections(form)
        inputs = bind(Map.get(sections, :in, []), Map.get(request, :inputs, []))

        patterns =
          Enum.flat_map(Map.get(sections, :where, []), &patterns/1) ++
            Enum.flat_map(inputs.rules, &rule_patterns/1)

        patterns |> Enum.map(&elem(&1, 0)) |> Enum.uniq() |> Enum.each(&known!(meta, &1))
        Enum.each(patterns, &keyword!(meta, &1, inputs.values))

      {:error, _} ->
        :ok
    end
  end

  ## ---- reading the request

  # `[:find .. :in .. :where ..]`, or the same as a map
  defp sections(%{} = map), do: map

  defp sections(list) when is_list(list) do
    list
    |> Enum.reduce({nil, %{}}, fn
      key, {_, acc} when key in [:find, :keys, :with, :in, :where] -> {key, Map.put(acc, key, [])}
      _, {nil, acc} -> {nil, acc}
      form, {key, acc} -> {key, Map.update!(acc, key, &(&1 ++ [form]))}
    end)
    |> elem(1)
  end

  defp sections(_), do: %{}

  # which values the variables of `:in` stand for, and the rules
  defp bind(specs, inputs) do
    specs
    |> Enum.reject(&match?({:symbol, "$" <> _}, &1))
    |> Enum.zip(inputs)
    |> Enum.reduce(%{values: %{}, rules: []}, fn
      {{:symbol, "%"}, [:rules, text]}, acc when is_binary(text) ->
        case Hosi.Edn.decode(text) do
          {:ok, rules} when is_list(rules) -> %{acc | rules: rules}
          _ -> acc
        end

      {{:symbol, "?" <> _ = var}, [:scalar, value]}, acc ->
        put_in(acc.values[var], [value])

      {[{:symbol, var}, {:symbol, "..."}], [:coll, values]}, acc when is_list(values) ->
        put_in(acc.values[var], values)

      _, acc ->
        acc
    end)
  end

  defp rule_patterns([_head | clauses]), do: Enum.flat_map(clauses, &patterns/1)
  defp rule_patterns(_), do: []

  # `[{attribute, value term | nil}]` of the data patterns in one clause. (EDN lists and vectors
  # read alike, so a clause is told apart by its first element.)
  defp patterns([{:symbol, op} | clauses]) when op in ["not", "or", "and"],
    do: Enum.flat_map(clauses, &patterns/1)

  defp patterns([{:symbol, op}, _vars | clauses]) when op in ["not-join", "or-join"],
    do: Enum.flat_map(clauses, &patterns/1)

  # a pattern or a `not` of a named source
  defp patterns([{:symbol, "$" <> _} | clause]), do: patterns(clause)

  defp patterns([{:symbol, "?" <> _}, attr | rest]), do: pattern(attr, rest)
  defp patterns([{:symbol, "_"}, attr | rest]), do: pattern(attr, rest)
  defp patterns([id, attr | rest]) when is_integer(id), do: pattern(attr, rest)
  # a lookup ref in the entity's place: [[:user/email "a@x"] :user/name ?n]
  defp patterns([[ref, _], attr | rest]) when is_atom(ref), do: pattern(ref, []) ++ pattern(attr, rest)
  defp patterns(_), do: []

  defp pattern(attr, rest) when is_atom(attr) and attr not in [nil, true, false],
    do: [{attr, List.first(rest)}]

  defp pattern(_, _), do: []

  ## ---- the checks

  defp known!(meta, attr) do
    declared = meta.declared
    name = Atom.to_string(attr)

    unless Map.has_key?(declared, attr) or builtin?(name) do
      closest = closest(declared, name)

      own = namespace(name)

      slip? =
        (own != nil and Enum.any?(Map.keys(declared), &(namespace(Atom.to_string(&1)) == own))) or
          (closest != nil and elem(closest, 1) >= @close)

      if slip? and not used?(meta, attr) do
        hint =
          case closest do
            {meant, score} when score >= 0.8 -> " Did you mean :#{meant}?"
            _ -> ""
          end

        raise ArgumentError,
              "Repo.datalog: no schema declares :#{name} and no record has it.#{hint} " <>
                "(ecto_hosi's check; hosigraph would answer as if no record had it. " <>
                "check: false skips it.)"
      end
    end
  end

  defp builtin?(name), do: String.starts_with?(name, ["db/", "db.", "hosi/", "hosi."])

  defp namespace(name) do
    case String.split(name, "/", parts: 2) do
      [namespace, _] -> namespace
      [_] -> nil
    end
  end

  # the declared name most like `name`, and how alike (the adapter's own attributes are not offered)
  defp closest(declared, name) do
    declared
    |> Map.keys()
    |> Enum.map(&Atom.to_string/1)
    |> Enum.reject(&(&1 =~ ~r{/(uq-|ecto-kind\z)}))
    |> Enum.map(&{&1, String.jaro_distance(&1, name)})
    |> Enum.max_by(&elem(&1, 1), fn -> nil end)
  end

  defp used?(meta, attr) do
    request = %{q: "[:find ?e :where [?e #{Attr.kw(attr)} _]]", inputs: []}
    meta |> Backend.query(request) |> Backend.ok!() != []
  end

  defp keyword!(meta, {attr, term}, values) do
    with %{type: :keyword} <- Map.get(meta.declared, attr),
         string when is_binary(string) <- string_in(term, values) do
      allowed =
        case Map.get(Backend.keywords(meta), attr) do
          nil -> ""
          names -> " The values are #{names |> Map.values() |> Enum.sort() |> Enum.map_join(", ", &inspect/1)}."
        end

      raise ArgumentError,
            "Repo.datalog: :#{attr} is a keyword attribute, and Datalog compares what is stored: " <>
              "write :#{string}, not #{inspect(string)}.#{allowed} (hosigraph's limit)"
    end
  end

  defp string_in(string, _values) when is_binary(string), do: string

  defp string_in({:symbol, var}, values),
    do: values |> Map.get(var, []) |> Enum.find(&is_binary/1)

  defp string_in(_, _), do: nil
end
