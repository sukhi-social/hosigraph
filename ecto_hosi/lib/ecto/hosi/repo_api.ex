defmodule Ecto.Hosi.RepoApi do
  @moduledoc false
  # What `Ecto.Adapters.Hosi.__before_compile__/1` adds to a Repo (the larger ones have modules of
  # their own: `Facts`, `Account`, `Explain`, `Batch`).

  alias Ecto.Hosi.{Backend, Check, Model, Query, Store, Types}

  def meta(repo), do: Ecto.Adapter.lookup_meta(repo.get_dynamic_repo())

  ## ------------------------------------------------------------ Datalog

  def datalog(repo, %Query{} = query, opts) do
    meta = meta(repo)
    run(repo, meta, Query.render(query, meta.model), opts)
  end

  def datalog(repo, %{q: _} = request, opts), do: run(repo, meta(repo), request, opts)

  defp run(repo, meta, request, opts) do
    load = Keyword.get(opts, :load)
    views = for schema <- List.wrap(load), do: schema && Model.view(meta.model, schema)
    if Keyword.get(opts, :check, true), do: Check.request!(meta, request)
    rows = meta |> Backend.query(request) |> Backend.ok!()

    cond do
      is_nil(load) -> rows
      is_list(load) -> loaded(repo, meta, rows, views)
      true -> for [struct] <- loaded(repo, meta, rows, views), do: struct
    end
  end

  # the rows with structs in place of the ids of the columns that name a view; a row with a record
  # that its view does not show is left out
  defp loaded(repo, meta, rows, views) do
    Enum.each(rows, fn row ->
      if length(row) != length(views) do
        raise ArgumentError,
              "Repo.datalog: load: names #{length(views)} column(s), and the query finds " <>
                "#{length(row)}. load: View is for a query that finds one id; otherwise give a " <>
                "list with a view or nil for each column"
      end
    end)

    ids =
      for row <- rows, {id, view} <- Enum.zip(row, views), view != nil, uniq: true do
        is_integer(id) ||
          raise ArgumentError,
                "Repo.datalog: load: #{inspect(view.module)} needs an id in its column, got #{inspect(id)}"

        id
      end

    entities = Map.new(Backend.read(meta, ids))

    Enum.flat_map(rows, fn row ->
      cells =
        Enum.zip_with(row, views, fn
          value, nil ->
            {:ok, value}

          id, view ->
            entity = Map.fetch!(entities, id)

            if Model.member?(view, entity),
              do: {:ok, struct_of(repo, meta, view, id, entity)},
              else: :none
        end)

      if :none in cells, do: [], else: [Enum.map(cells, fn {:ok, value} -> value end)]
    end)
  end

  @doc "The struct of a view for a record that holds `entity`, as a query would load it."
  def struct_of(repo, meta, view, id, entity) do
    # (Ecto loads by source, which is the attribute)
    data =
      for {_field, attr} <- view.attr_of, into: %{id: id} do
        value =
          case meta.model.attrs[attr] do
            %{card: :many} -> entity |> Map.get(attr, []) |> Enum.sort()
            _ -> Types.read(view.types[attr], Backend.first(entity, attr))
          end

        {attr, value}
      end

    repo.load(view.module, data)
  end

  ## ------------------------------------------------------------ one record

  def facts(repo, struct_or_id) do
    meta = meta(repo)
    id = entity_id(struct_or_id, "Repo.facts/1")

    case meta.backend.get_entities(meta.conn, [id]) |> Backend.ok!() do
      [entity] -> entity
      _ -> %{}
    end
  end

  def history(repo, struct_or_id) do
    meta = meta(repo)
    id = entity_id(struct_or_id, "Repo.history/1")
    events = meta.backend.history(meta.conn, id) |> Backend.ok!()

    case struct_or_id do
      %schema{} -> by_field(meta, Model.view(meta.model, schema), events)
      _ -> events
    end
  end

  defp by_field(meta, view, events) do
    Enum.group_by(
      events,
      &Map.get(view.field_of, &1.attr, &1.attr),
      fn %{seq: seq, kind: kind, attr: attr, val: value} ->
        value =
          if Map.has_key?(view.field_of, attr),
            do: element(meta, view.types[attr], attr, value),
            else: value

        %{seq: seq, kind: kind, value: value}
      end
    )
  end

  # one fact's value as a field of `type` reads it; as it is kept, where it does not load
  defp element(meta, type, attr, kept) do
    type =
      case {meta.model.attrs[attr], type} do
        {%{card: :many}, {:array, inner}} -> inner
        _ -> type
      end

    value = Types.read(type, Store.back(meta.model, attr, kept))

    case Ecto.Type.adapter_load(Ecto.Adapters.Hosi, type, value) do
      {:ok, loaded} -> loaded
      _ -> kept
    end
  end

  @doc "The id, as hosigraph keeps it, of a struct or an integer (a schema's id may be a custom type)."
  def entity_id(id, _caller) when is_integer(id), do: id

  def entity_id(%schema{id: id} = struct, caller) when not is_nil(id) do
    if function_exported?(schema, :__schema__, 2),
      do: dump1(schema.__schema__(:type, :id), id),
      else: not_an_entity!(struct, caller)
  end

  def entity_id(other, caller), do: not_an_entity!(other, caller)

  defp not_an_entity!(other, caller) do
    raise ArgumentError,
          "#{caller} takes a struct or its integer id (as hosigraph keeps it), got: #{inspect(other)}"
  end

  ## ------------------------------------------------------------ erase and clean

  def erase(repo, erasures, opts) do
    unless is_list(erasures) and Enum.all?(erasures, &match?([kind | _] when is_atom(kind), &1)) do
      raise ArgumentError,
            "Repo.erase takes a list of erasures, each a tagged list: [[:ids, [id]]], " <>
              "[[:range, first, last]], [[:\"owned-by\", attributes, owners]] and the others of " <>
              "hosigraph's erase. Got: #{inspect(erasures)}"
    end

    repo |> meta() |> Backend.with_timeout(opts) |> erase!(erasures)
  end

  @doc "hosigraph's erase, waited for as long as the meta says; its report."
  def erase!(meta, erasures) do
    timeout = Backend.timeout(meta)

    fn -> meta.backend.erase(meta.conn, erasures, timeout: timeout) end
    |> then(&Backend.waited(timeout, &1))
    |> Backend.ok!()
  end

  # no write of facts, but it can run long: it waits for the end unless told otherwise
  def clean(repo, opts) do
    meta = meta(repo)
    opts = Keyword.put_new(opts, :timeout, :infinity)

    fn -> meta.backend.clean(meta.conn, opts) end
    |> then(&Backend.waited(opts[:timeout], &1))
    |> Backend.ok!()
  end

  ## ------------------------------------------------------------ newest first

  def highest(repo, attr, limit, opts) do
    meta = meta(repo)

    unless is_atom(attr) and match?(%{tuple: nil}, meta.model.attrs[attr]) do
      raise ArgumentError,
            "Repo.highest takes an attribute that a view of the Repo declares (such as " <>
              ":\"ap/published\"), got #{inspect(attr)}"
    end

    %{ecto: type} = meta.model.attrs[attr]
    view = opts[:load] && Model.view(meta.model, opts[:load])

    after_ =
      case opts[:after] do
        nil -> []
        %{value: value, id: id} -> [after: %{val: Store.out(meta.model, attr, dump1(type, value)), id: id}]
      end

    places =
      meta.backend.highest(meta.conn, attr, limit, after_)
      |> Backend.ok!()
      |> Enum.map(fn %{val: value, id: id} -> %{id: id, value: element(meta, type, attr, value)} end)

    found =
      case view do
        nil ->
          places

        view ->
          entities = Map.new(Backend.read(meta, places |> Enum.map(& &1.id) |> Enum.uniq()))

          for %{id: id} <- places,
              Model.member?(view, entities[id]),
              do: struct_of(repo, meta, view, id, entities[id])
      end

    {found, List.last(places)}
  end

  @doc """
  The newest records of some keys, by the `(key, order)` index a view declares with
  `hosi_tuple key, order`. Places, newest first: `%{id: id, key: key, at: order_value}`, `at`
  loaded as the order field's type. `after:` is a place from an earlier call (or
  `%{at: value, id: id}`) and the page starts after it. Only records the view shows are answered.
  About one seek per key.
  """
  def highest_in(repo, schema, {key, order}, keys, limit, opts) do
    meta = meta(repo)
    view = Model.view(meta.model, schema)
    key_attr = view.attr_of[key]
    order_attr = view.attr_of[order]
    attr = key_attr && order_attr && Model.tuple_attr(key_attr, order_attr)

    unless attr && match?(%{made: :index}, meta.model.attrs[attr]) do
      raise ArgumentError,
            "Repo.highest_in: no view of the Repo declares hosi_tuple #{inspect(key)}, #{inspect(order)} " <>
              "for the fields of #{inspect(schema)}"
    end

    id_type = schema.__schema__(:type, :id)
    key_type = view.types[key_attr]
    order_type = view.types[order_attr]
    keys = Enum.map(keys, &Store.out(meta.model, key_attr, dump1(key_type, &1)))

    after_ =
      case opts[:after] do
        nil -> nil
        %{at: at, id: id} -> %{val: dump1(order_type, at), id: dump1(id_type, id)}
      end

    meta
    |> shown(view, attr, keys, limit, after_)
    |> Enum.map(fn %{key: k, val: v, id: id} ->
      %{
        id: load1(id_type, id),
        key: element(meta, key_type, key_attr, k),
        at: element(meta, order_type, order_attr, v)
      }
    end)
  end

  # the first `limit` places whose record the view shows: the index holds every record that has
  # the two attributes, so it is walked until there are enough
  defp shown(_meta, _view, _attr, _keys, limit, _after) when limit <= 0, do: []

  defp shown(meta, view, attr, keys, limit, after_) do
    page = max(limit, 50)
    opts = if after_, do: [after: after_], else: []
    places = meta.backend.highest_in(meta.conn, attr, keys, page, opts) |> Backend.ok!()
    entities = Map.new(Backend.read(meta, places |> Enum.map(& &1.id) |> Enum.uniq()))
    found = places |> Enum.filter(&Model.member?(view, entities[&1.id])) |> Enum.take(limit)
    missing = limit - length(found)

    if missing > 0 and length(places) == page do
      last = List.last(places)
      found ++ shown(meta, view, attr, keys, missing, %{val: last.val, id: last.id})
    else
      found
    end
  end

  defp dump1(type, v) do
    case Ecto.Type.adapter_dump(Ecto.Adapters.Hosi, type, v) do
      {:ok, dumped} -> dumped
      _ -> raise ArgumentError, "#{inspect(v)} is not a value of the type #{inspect(type)}"
    end
  end

  defp load1(type, v) do
    {:ok, loaded} = Ecto.Type.adapter_load(Ecto.Adapters.Hosi, type, v)
    loaded
  end
end
