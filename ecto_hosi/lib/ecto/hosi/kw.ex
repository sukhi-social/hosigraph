defmodule Ecto.Hosi.Kw do
  @moduledoc false
  # Fields kept as keywords (`hosi_keyword :visibility, ["public", "followers"]`).
  #
  # Elixir (Ecto, changesets, the app) sees the string, "public". hosigraph keeps the keyword,
  # `:public`, which is what Datalog reads naturally (`[?n :note/visibility :public]`).
  # The conversion happens at the edge and nowhere else:
  #
  #   * going out: the ops and conditions of `Backend.write`, and the inputs of a query
  #     (`to_store/3`, where the attribute is known);
  #   * coming back: the entities of `Backend.get_entities` (`entity/2`).
  #
  # `kws` is `%{attr => %{"public" => :public, ...}}`, built once from the schemas. The values are
  # a closed list written in the schema, so an atom is made only for a name the code already has,
  # and a string that is not on the list is refused here, before hosigraph sees it.

  alias Ecto.Hosi.Attr

  # what a keyword may look like in EDN without being quoted (no `/`, no `:` and no spaces)
  @name ~r/\A[A-Za-z][A-Za-z0-9_.\-]*\z/

  @doc "`%{attr => %{string => atom}}` for the `hosi_keyword`s of these schema modules."
  def build(schemas) do
    for mod <- schemas,
        Code.ensure_compiled!(mod) && function_exported?(mod, :__hosi__, 1),
        {field, values} <- mod.__hosi__(:keywords),
        into: %{} do
      source = mod.__schema__(:source)
      check!(mod, field, values)
      {Attr.attr(source, field), Map.new(values, &{&1, String.to_atom(&1)})}
    end
  end

  defp check!(mod, field, values) do
    unless mod.__schema__(:type, field) == :string do
      raise ArgumentError,
            "#{inspect(mod)}: hosi_keyword #{inspect(field)} needs a :string field " <>
              "(Elixir keeps seeing the string, hosigraph keeps the keyword)"
    end

    unless is_list(values) and values != [] and Enum.all?(values, &(is_binary(&1) and &1 =~ @name)) and
             values == Enum.uniq(values) do
      raise ArgumentError,
            "#{inspect(mod)}: hosi_keyword #{inspect(field)} takes a list of distinct strings that are " <>
              "keyword names (letters, digits, _ - .; no / or spaces), got #{inspect(values)}"
    end
  end

  @doc """
  A value on its way to hosigraph for `attr`: the keyword for a string on the list (a list of
  them for a `:coll` input). Anything that is not a keyword attribute is returned as it is.
  """
  def to_store(kws, attr, value) do
    case kws do
      %{^attr => names} -> convert(attr, names, value)
      _ -> value
    end
  end

  defp convert(_attr, _names, nil), do: nil
  defp convert(attr, names, vs) when is_list(vs), do: Enum.map(vs, &convert(attr, names, &1))

  defp convert(attr, names, v) when is_binary(v) do
    case names do
      %{^v => keyword} ->
        keyword

      _ ->
        raise ArgumentError,
              "#{inspect(v)} is not one of the values of #{inspect(attr)} (hosi_keyword, " <>
                "ecto_hosi's limit): " <> inspect(Map.keys(names))
    end
  end

  # already a keyword (the adapter's own reads, an undo of a write)
  defp convert(_attr, _names, v) when is_atom(v), do: v

  @doc "The ops of a write (`[:create | :delete, id, attr, value]`) with the keywords made."
  def ops(kws, ops) when map_size(kws) == 0, do: ops

  def ops(kws, ops) do
    Enum.map(ops, fn
      [kind, ref, attr, value] when kind in [:create, :delete] ->
        [kind, ref, attr, to_store(kws, attr, value)]

      other ->
        other
    end)
  end

  @doc "The `when:` conditions of a write, with the keywords made."
  def conds(kws, conds) when map_size(kws) == 0, do: conds

  def conds(kws, conds) do
    Enum.map(conds, fn
      [kind, id, attr, value] when kind in [:holds, :lacks] -> [kind, id, attr, to_store(kws, attr, value)]
      other -> other
    end)
  end

  @doc "The options of `Backend.write` with the `:when` conditions converted."
  def opts(kws, opts) do
    case Keyword.fetch(opts, :when) do
      {:ok, conds} -> Keyword.put(opts, :when, conds(kws, conds))
      :error -> opts
    end
  end

  @doc "An entity (`%{attr => [values]}`) as read from hosigraph, with the keywords back as strings."
  def entity(kws, entity) when map_size(kws) == 0, do: entity

  def entity(kws, entity) do
    Enum.reduce(kws, entity, fn {attr, _names}, acc ->
      case acc do
        %{^attr => values} -> Map.put(acc, attr, Enum.map(values, &from_store/1))
        _ -> acc
      end
    end)
  end

  defp from_store(v) when is_atom(v) and not is_nil(v) and not is_boolean(v), do: Atom.to_string(v)
  defp from_store(v), do: v
end
