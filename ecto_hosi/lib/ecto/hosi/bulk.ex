defmodule Ecto.Hosi.Bulk do
  @moduledoc false
  # `Repo.update_all/3` and `Repo.delete_all/2` through one view: find the matching ids with the
  # same `where` as a read, then change them in ONE hosigraph write, guarded so that a record
  # that changed meanwhile fails the write rather than being brought back.

  alias Ecto.Hosi.{Backend, Delete, Eval, Model, Reader, Types, Update}

  def update_all(meta, %Ecto.Query{} = query, params) do
    params = Types.normalize_params(params)
    {view, rows} = matching(meta, query, params)

    {ops, conds} =
      rows
      |> Enum.map(fn {id, entity} -> Update.ops(meta, view, id, entity, query.updates, params) end)
      |> Enum.unzip()

    write!(meta, Enum.concat(ops), Enum.concat(conds))

    # `returning`: the records as they are after the update
    after_rows =
      if query.select && rows != [] do
        ids = Enum.map(rows, &elem(&1, 0))
        entities = Backend.read_back(fn -> Backend.get_entities(meta, ids) end) |> Backend.ok!()
        Enum.zip(ids, entities)
      else
        rows
      end

    {length(rows), returned(meta, query, params, view, after_rows)}
  end

  def delete_all(meta, %Ecto.Query{} = query, params) do
    params = Types.normalize_params(params)
    {view, rows} = matching(meta, query, params)

    case rows do
      [] ->
        :ok

      _ ->
        case Delete.plan(meta, view, rows) do
          {:ok, ops, conds} ->
            write!(meta, ops, conds)

          {:restrict, name} ->
            raise Ecto.Hosi.Error,
              message:
                "delete_all through #{inspect(view.module)} violates the foreign key #{name}: " <>
                  "records still refer to what it would delete"

          {:refused, info} ->
            raise Ecto.Hosi.Error, reason: {:refused, info}, message: Delete.message(meta, info)
        end
    end

    {length(rows), returned(meta, query, params, view, rows)}
  end

  # The rows `select`/`returning` asks for, from `[{id, entity}]`; nil when nothing is asked for.
  defp returned(_meta, %{select: nil}, _params, _view, _rows), do: nil

  defp returned(meta, %{select: %{fields: fields}}, params, view, rows) do
    ctx = %{meta: meta, params: params}

    for {id, entity} <- rows do
      env = %{0 => %{id: id, entity: entity}}

      for expr <- fields do
        value = Eval.eval(expr, env, ctx)

        case expr do
          {{:., _, [{:&, _, [0]}, attr]}, _, []} -> Types.read(view.types[attr], value)
          _ -> value
        end
      end
    end
  end

  # `{view, [{id, entity}]}` of the records the query is about, that the view shows.
  defp matching(meta, query, params) do
    {view, ids} = Reader.candidate_ids(meta, query, params)
    rows = meta |> Backend.read(ids) |> Enum.filter(fn {_, e} -> Model.member?(view, e) end)
    {view, rows}
  end

  defp write!(_meta, [], _conds), do: :ok

  defp write!(meta, ops, conds) do
    case Backend.write(meta, ops, when: conds) do
      {:ok, _} ->
        :ok

      {:error, {_, :conflict, _}} ->
        raise Ecto.Hosi.Error, message: "a record changed while it was being written; try again"

      {:error, {_, :"write-rejected", msg}} ->
        raise Ecto.Hosi.Error, message: "hosigraph refused the write: " <> msg

      {:error, reason} ->
        raise Ecto.Hosi.Error, reason: reason
    end
  end
end
