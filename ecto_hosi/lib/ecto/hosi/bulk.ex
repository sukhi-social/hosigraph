defmodule Ecto.Hosi.Bulk do
  @moduledoc false
  # `Repo.update_all/3` and `Repo.delete_all/2` on one schema: find the matching ids with the
  # same `where` as a read, then change them in ONE hosigraph write, guarded so that a row
  # that vanished meanwhile fails the write rather than being resurrected.

  alias Ecto.Hosi.{Attr, Backend, Eval, Reader, Types, Update}

  def update_all(meta, %Ecto.Query{} = query, params) do
    raw = params
    params = Types.normalize_params(params)
    {source, schema, rows} = matching(meta, query, params)
    Ecto.Hosi.Cascade.append_only!(meta, source)

    {ops, conds} =
      Enum.reduce(rows, {[], []}, fn {id, entity}, {ops, conds} ->
        {o, c} = Update.ops(meta, source, id, entity, query.updates, params, raw)
        {ops ++ o, conds ++ c}
      end)

    write!(meta, ops, conds)

    # `returning`: the rows as they are after the update
    after_rows =
      if query.select && rows != [] do
        ids = Enum.map(rows, &elem(&1, 0))
        entities = meta |> Backend.get_entities(ids) |> Backend.ok!()
        Enum.zip(ids, entities)
      else
        rows
      end

    {length(rows), returned(meta, query, params, source, schema, after_rows)}
  end

  def delete_all(meta, %Ecto.Query{} = query, params) do
    params = Types.normalize_params(params)
    {source, schema, rows} = matching(meta, query, params)
    Ecto.Hosi.Cascade.append_only!(meta, source)

    case rows do
      [] ->
        :ok

      _ ->
        case Ecto.Hosi.Cascade.plan(meta, source, rows) do
          {:ok, ops, conds} ->
            write!(meta, ops, conds)

          {:restrict, name} ->
            raise Ecto.Hosi.Error,
              message: "delete_all on #{source} violates the foreign key #{name}: rows still point at it"
        end
    end

    {length(rows), returned(meta, query, params, source, schema, rows)}
  end

  # The rows `select`/`returning` asks for, from `[{id, entity}]`; nil when nothing is asked for.
  defp returned(_meta, %{select: nil}, _params, _source, _schema, _rows), do: nil

  defp returned(meta, %{select: %{fields: fields}}, params, source, schema, rows) do
    ctx = %{meta: meta, params: params}

    for {id, entity} <- rows do
      env = %{0 => %{id: id, entity: entity, source: source}}

      for expr <- fields do
        value = Eval.eval(expr, env, ctx)

        case expr do
          {{:., _, [{:&, _, [0]}, f]}, _, []} -> Types.read(schema, f, value)
          _ -> value
        end
      end
    end
  end

  # [{id, entity}] of the rows that really are rows of the schema.
  defp matching(meta, query, params) do
    {source, schema, ids} = Reader.candidate_ids(meta, query, params)
    marker = Attr.marker(source)

    rows =
      if ids == [] do
        []
      else
        meta
        |> Backend.get_entities(ids)
        |> Backend.ok!()
        |> then(&Enum.zip(ids, &1))
        |> Enum.filter(fn {_, entity} -> Map.has_key?(entity, marker) end)
      end

    {source, schema, rows}
  end

  defp write!(_meta, [], _conds), do: :ok

  defp write!(meta, ops, conds) do
    case Backend.write(meta, ops, when: conds) do
      {:ok, _} ->
        :ok

      {:error, {_, :conflict, _}} ->
        raise Ecto.Hosi.Error, message: "a row changed while it was being updated; try again"

      {:error, {_, :"write-rejected", msg}} ->
        raise Ecto.Hosi.Error, message: "hosigraph refused the write: " <> msg

      {:error, reason} ->
        raise Ecto.Hosi.Error, reason: reason
    end
  end
end
