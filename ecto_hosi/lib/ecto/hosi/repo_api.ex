defmodule Ecto.Hosi.RepoApi do
  @moduledoc false
  # What `Ecto.Adapters.Hosi.__before_compile__/1` adds to a Repo.

  alias Ecto.Hosi.{Attr, Backend, Query}

  defp meta(repo), do: Ecto.Adapter.lookup_meta(repo.get_dynamic_repo())

  def datalog(repo, %Query{} = query, opts), do: datalog(repo, Query.to_request(query), opts)

  def datalog(repo, %{q: _} = request, _opts) do
    meta = meta(repo)
    meta |> Backend.query(request) |> Backend.ok!()
  end

  def history(repo, id) do
    meta = meta(repo)
    meta.backend.history(meta.conn, id) |> Backend.ok!()
  end

  def erase(repo, erasures) do
    meta = meta(repo)
    meta.backend.erase(meta.conn, erasures) |> Backend.ok!()
  end

  def highest(repo, schema, field, limit, opts) do
    meta = meta(repo)
    source = schema.__schema__(:source)
    attr = Attr.attr(source, field)

    places = meta.backend.highest(meta.conn, attr, limit, opts) |> Backend.ok!()
    ids = Enum.map(places, & &1.id)
    entities = if ids == [], do: [], else: Backend.get_entities(meta, ids) |> Backend.ok!()

    structs =
      for {id, entity} <- Enum.zip(ids, entities) do
        data =
          for f <- schema.__schema__(:fields), into: %{} do
            if f == :id, do: {:id, id}, else: {f, Backend.first(entity, Attr.attr(source, f))}
          end

        repo.load(schema, data)
      end

    {structs, List.last(places)}
  end

  @doc """
  The newest rows of some keys, by the `(key, order)` index the schema declares with
  `hosi_tuple key, order`. Returns places, newest first: `%{id: id, key: key, at: order_value}`,
  `at` loaded as the order field's type. `after:` is a place from an earlier call (or
  `%{at: value, id: id}`) and the page starts after it. About one seek per key.
  """
  def highest_in(repo, schema, {key, order}, keys, limit, opts) do
    meta = meta(repo)
    source = schema.__schema__(:source)
    attr = Attr.tuple(source, key, order)
    id_type = schema.__schema__(:type, :id)
    key_type = schema.__schema__(:type, key)
    order_type = schema.__schema__(:type, order)

    keys = Enum.map(keys, &dump1(key_type, &1))

    opts =
      case opts[:after] do
        nil -> []
        %{at: at, id: id} -> [after: %{val: dump1(order_type, at), id: dump1(id_type, id)}]
      end

    places = meta.backend.highest_in(meta.conn, attr, keys, limit, opts) |> Backend.ok!()

    for %{key: k, val: v, id: id} <- places do
      %{id: load1(id_type, id), key: load1(key_type, k), at: load1(order_type, v)}
    end
  end

  defp dump1(type, v) do
    {:ok, d} = Ecto.Type.adapter_dump(Ecto.Adapters.Hosi, type, v)
    d
  end

  defp load1(type, v) do
    {:ok, l} = Ecto.Type.adapter_load(Ecto.Adapters.Hosi, type, v)
    l
  end
end
