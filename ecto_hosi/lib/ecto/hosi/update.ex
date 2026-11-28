defmodule Ecto.Hosi.Update do
  @moduledoc false
  # The `update:` of a query (`set:` and `inc:`) as hosigraph ops for one record. Used by
  # `update_all` and by `on_conflict: [set: ..]` / an update query.
  #
  # Values may be literals, `^params`, or another field of the same record.

  alias Ecto.Hosi.{Backend, Change, Store}

  @doc """
  `{ops, conds}` that apply `updates` to the record `id`, whose current `entity` was just read.
  """
  def ops(meta, view, id, entity, updates, params) do
    changes =
      for %{expr: kinds} <- updates, {kind, kvs} <- kinds, {attr, expr} <- kvs do
        {kind, attr, expr}
      end

    sets =
      Enum.map(changes, fn
        {:set, attr, expr} ->
          {attr, Store.normalize(meta.model, attr, value(expr, meta, id, entity, params))}

        {:inc, attr, expr} ->
          by = value(expr, meta, id, entity, params)
          old = Backend.first(entity, attr) || 0

          unless is_number(old) and is_number(by) do
            raise ArgumentError,
                  "Ecto.Adapters.Hosi can inc only numbers (#{inspect(attr)}; ecto_hosi's limit)"
          end

          {attr, old + by}

        {other, attr, _} ->
          raise ArgumentError,
                "Ecto.Adapters.Hosi does not support `#{other}:` in update (#{inspect(attr)}; " <>
                  "ecto_hosi's limit); use set: or inc:"
      end)

    {ops, conds} = Change.update(meta, id, entity, sets)
    {ops, Change.existence(meta, view, id, entity) ++ conds}
  end

  defp value({:^, _, [ix]}, _meta, _id, _entity, params), do: Enum.at(params, ix)

  defp value(%Ecto.Query.Tagged{value: v}, meta, id, entity, params),
    do: value(v, meta, id, entity, params)

  defp value({{:., _, [{:&, _, [0]}, :id]}, _, []}, _meta, id, _entity, _params), do: id

  defp value({{:., _, [{:&, _, [0]}, attr]}, _, []}, meta, _id, entity, _params) do
    case meta.model.attrs[attr] do
      %{card: :many} -> Map.get(entity, attr, [])
      _ -> Backend.first(entity, attr)
    end
  end

  defp value(v, _meta, _id, _entity, _params)
       when is_number(v) or is_binary(v) or is_boolean(v) or is_nil(v) or is_atom(v) or is_list(v),
       do: v

  defp value(other, _meta, _id, _entity, _params) do
    raise ArgumentError,
          "Ecto.Adapters.Hosi cannot evaluate #{Macro.to_string(other)} in an update " <>
            "(ecto_hosi's limit); use a literal, a ^parameter or a field of the same record"
  end
end
