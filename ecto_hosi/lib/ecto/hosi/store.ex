defmodule Ecto.Hosi.Store do
  @moduledoc false
  # A value has two forms.
  #
  #   * the adapter's: what Ecto's dumpers give and its loaders take. A time is microseconds, a
  #     keyword is a string, a UUID is its text, a list that is kept as one value is JSON text, and
  #     the many values of an attribute are a list.
  #   * hosigraph's: a keyword is an atom, a UUID is `{:uuid, text}`.
  #
  # The change from one to the other is made here, at the edge, and nowhere else: the ops and
  # conditions of a write and the inputs of a query on the way out, what is read on the way back.

  alias Ecto.Hosi.Model

  @doc "A field's dumped value as the adapter holds it: a list as JSON text, or as a set of many."
  def normalize(_model, _attr, nil), do: nil

  def normalize(model, attr, value) do
    case model.attrs do
      %{^attr => %{card: :many}} -> value |> List.wrap() |> Enum.reject(&is_nil/1) |> Enum.uniq() |> Enum.sort()
      %{^attr => %{enc: {:array, _}}} when is_list(value) -> JSON.encode!(value)
      _ -> value
    end
  end

  @doc "The values an attribute of a record holds when it is set to `value`: one fact each."
  def elements(_model, _attr, nil), do: []

  def elements(model, attr, value) do
    case model.attrs do
      %{^attr => %{card: :many}} -> value |> List.wrap() |> Enum.uniq()
      _ -> [value]
    end
  end

  @doc "A value on its way to hosigraph for `attr` (a list: each of them, for a collection input)."
  def out(model, attr, value) do
    case model.attrs do
      %{^attr => %{keyword: names}} when names != nil -> each(value, &keyword!(attr, names, &1))
      %{^attr => %{type: :uuid}} -> each(value, &uuid/1)
      _ -> value
    end
  end

  @doc "A value as hosigraph answered it, for `attr`."
  def back(model, attr, value) do
    case model.attrs do
      %{^attr => %{keyword: names}} when names != nil -> each(value, &keyword_text/1)
      %{^attr => %{type: :uuid}} -> each(value, &uuid_text/1)
      _ -> value
    end
  end

  @doc "The ops of a write (`[:create | :delete, ref, attr, value]`) in hosigraph's form."
  def ops(model, ops) do
    Enum.map(ops, fn
      [kind, ref, attr, value] when kind in [:create, :delete] -> [kind, ref, attr, out(model, attr, value)]
      other -> other
    end)
  end

  @doc "The `when:` conditions of a write in hosigraph's form."
  def conds(model, conds) do
    Enum.map(conds, fn
      [kind, id, attr, value] when kind in [:holds, :lacks] -> [kind, id, attr, out(model, attr, value)]
      other -> other
    end)
  end

  @doc "An entity (`%{attr => [values]}`) as read from hosigraph, in the adapter's form."
  def entity(%Model{} = model, entity) do
    Map.new(entity, fn {attr, values} -> {attr, Enum.map(values, &back(model, attr, &1))} end)
  end

  defp each(values, fun) when is_list(values) do
    case values do
      [:ref, _] -> values
      [:tmp, _] -> values
      _ -> Enum.map(values, &each(&1, fun))
    end
  end

  defp each(nil, _fun), do: nil
  defp each(value, fun), do: fun.(value)

  defp keyword!(attr, names, value) when is_binary(value) do
    case names do
      %{^value => keyword} ->
        keyword

      _ ->
        raise ArgumentError,
              "#{inspect(value)} is not one of the values of #{inspect(attr)} (a keyword's closed " <>
                "list, ecto_hosi's limit): " <> inspect(Map.keys(names))
    end
  end

  # already a keyword (the adapter's own reads, an undo of a write)
  defp keyword!(_attr, _names, value), do: value

  defp keyword_text(v) when is_atom(v) and not is_boolean(v), do: Atom.to_string(v)
  defp keyword_text(v), do: v

  defp uuid(text) when is_binary(text), do: {:uuid, text}
  defp uuid(other), do: other

  defp uuid_text({:uuid, text}), do: text
  defp uuid_text(other), do: other
end
