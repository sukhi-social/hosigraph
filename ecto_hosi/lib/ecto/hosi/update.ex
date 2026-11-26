defmodule Ecto.Hosi.Update do
  @moduledoc false
  # The `update:` of a query (`set:` and `inc:`) as hosigraph ops for one row. Used by
  # `update_all` and by `on_conflict: [set: ..]` / an update query.
  #
  # Values may be literals, `^params`, or another field of the same row.

  alias Ecto.Hosi.{Attr, Backend}

  @doc """
  `{ops, conds}` that apply `updates` to the row `id`, whose current `entity` was just read.
  """
  def ops(meta, source, id, entity, updates, params, raw \\ nil) do
    raw = raw || params

    changes =
      for %{expr: kinds} <- updates, {kind, kvs} <- kinds, {field, expr} <- kvs do
        {kind, field, expr}
      end

    sets =
      Enum.map(changes, fn
        {:set, field, expr} ->
          {Attr.attr(source, field), set_value(expr, id, entity, source, params, raw)}

        {:inc, field, expr} ->
          attr = Attr.attr(source, field)
          by = value(expr, id, entity, source, params)
          old = Backend.first(entity, attr) || 0

          unless is_number(old) and is_number(by) do
            raise ArgumentError, "Ecto.Adapters.Hosi can inc only numbers (field #{field})"
          end

          {attr, old + by}

        {other, field, _} ->
          raise ArgumentError,
                "Ecto.Adapters.Hosi does not support `#{other}:` in update (field #{field}); " <>
                  "use set: or inc:"
      end)

    sets = Ecto.Hosi.Derived.extend(meta, source, id, sets, fn -> entity end)
    set_ops(meta, id, entity, sets)
    |> then(fn {ops, conds} -> {ops, [[:holds, id, Attr.marker(source), true] | conds]} end)
  end

  @doc """
  `{ops, conds}` that make `attr_values` (`[{attr, value | nil}]`) the row's values:
  a value is written, `nil` retracts what the row holds now.
  """
  def set_ops(meta, id, entity, attr_values) do
    Enum.reduce(attr_values, {[], []}, fn {attr, v}, {ops, conds} ->
      old = Map.get(entity, attr, [])

      cond do
        is_nil(v) and old == [] ->
          {ops, conds}

        is_nil(v) ->
          {ops ++ for(o <- old, do: [:delete, id, attr, Backend.wrap(meta, attr, o)]),
           conds ++ for(o <- old, do: [:holds, id, attr, o])}

        old == [v] ->
          {ops, conds}

        true ->
          {ops ++ [[:create, id, attr, Backend.wrap(meta, attr, v)]], conds}
      end
    end)
  end

  # A list is a value of an array column, stored as JSON text. The `params` have had `raw_dump`
  # applied element by element (they are also `in ^list` lists), so the list comes from `raw`.
  defp set_value({:^, _, [ix]} = expr, id, entity, source, params, raw) do
    case value(expr, id, entity, source, params) do
      list when is_list(list) -> JSON.encode!(Enum.at(raw, ix))
      v -> v
    end
  end

  defp set_value(expr, id, entity, source, params, _raw), do: value(expr, id, entity, source, params)

  defp value({:^, _, [ix]}, _id, _entity, _source, params), do: Enum.at(params, ix)
  defp value(%Ecto.Query.Tagged{value: v}, id, entity, source, params), do: value(v, id, entity, source, params)

  defp value({{:., _, [{:&, _, [0]}, :id]}, _, []}, id, _entity, _source, _params), do: id

  defp value({{:., _, [{:&, _, [0]}, f]}, _, []}, _id, entity, source, _params),
    do: Backend.first(entity, Attr.attr(source, f))

  defp value(v, _id, _entity, _source, _params)
       when is_number(v) or is_binary(v) or is_boolean(v) or is_nil(v) or is_atom(v),
       do: v

  defp value(other, _id, _entity, _source, _params) do
    raise ArgumentError,
          "Ecto.Adapters.Hosi cannot evaluate #{Macro.to_string(other)} in an update; " <>
            "use a literal, a ^parameter or a field of the same row"
  end
end
