defmodule Ecto.Hosi.Delete do
  @moduledoc false
  # What a delete writes: the tombstone of a record type or the removal of the record, and what
  # that does to the records that refer to it (`on_delete`). All of it goes into one write,
  # guarded by conditions on what was read.
  #
  # A delete says what it removes. Through view V, the record R may go if
  #
  #   * R has a record type, and V is a view of that type; or
  #   * R has no record type, and no facts outside V.
  #
  # Otherwise the delete is refused, so that nobody removes facts they did not see.

  alias Ecto.Hosi.{Backend, Change, Model, Store}

  @owner :"hosi/owner"
  @chunk 500

  @doc """
  The write that deletes `rows` (`[{id, entity}]`) through `view`: `{:ok, ops, conds}`,
  `{:restrict, constraint_name}`, or `{:refused, %{id, view, reason, attributes}}`. `gone` are
  records that will be removed by other means: they neither follow nor hold on.
  """
  def plan(meta, view, rows, gone \\ []) do
    with {:ok, roots} <- each(rows, fn {id, entity} -> root(meta, view, id, entity) end) do
      state = %{plan: Map.new(roots), restrict: [], nilify: %{}, gone: MapSet.new(gone)}

      case walk(meta, state, Enum.map(roots, &elem(&1, 0))) do
        {:refused, _} = refused ->
          refused

        {:ok, state} ->
          case blocked(meta, state) do
            [] ->
              {ops, conds} = ops(meta, view, state)
              {:ok, ops, conds}

            [{_x, name} | _] ->
              {:restrict, name}
          end
      end
    end
  end

  @doc """
  What the references of other records to `ids`, which are being erased, ask for:
  `{:ok, ops, conds, left}`, where `left` is `[{id, constraint_name}]` of the records a
  `:restrict` would have held on to (an erase cannot be refused; they are left as they are).
  """
  def after_erase(meta, ids) do
    state = %{plan: %{}, restrict: [], nilify: %{}, gone: MapSet.new(ids)}

    with {:ok, state} <- walk(meta, state, ids) do
      {ops, conds} = ops(meta, nil, state)
      {:ok, ops, conds, blocked(meta, state)}
    end
  end

  defp each(items, fun) do
    items
    |> Enum.reduce_while({:ok, []}, fn item, {:ok, acc} ->
      case fun.(item) do
        {:refused, _} = refused -> {:halt, refused}
        value -> {:cont, {:ok, [value | acc]}}
      end
    end)
    |> case do
      {:ok, acc} -> {:ok, Enum.reverse(acc)}
      refused -> refused
    end
  end

  # how a record the caller deletes goes
  defp root(meta, view, id, entity) do
    case Model.record_types(meta.model, entity) do
      [] ->
        case outside(meta, view, entity) do
          [] -> mode(meta, view, id, entity, [])
          others -> refused(id, view, :other_facts, others)
        end

      types ->
        if types -- view.record_types == [],
          do: mode(meta, view, id, entity, types),
          else: refused(id, view, :not_type_view, Enum.map(types, &elem(&1, 0)))
    end
  end

  # how a record goes because the record it refers to by `attr` goes
  defp child(meta, attr, id, entity) do
    case Model.record_types(meta.model, entity) do
      [] ->
        holders = for {_, v} <- Enum.sort(meta.model.views), Map.has_key?(v.field_of, attr), do: v

        case Enum.find(holders, &(outside(meta, &1, entity) == [])) do
          nil ->
            view = List.first(holders)
            refused(id, view, :other_facts, if(view, do: outside(meta, view, entity), else: Map.keys(entity)))

          view ->
            mode(meta, view, id, entity, [])
        end

      types ->
        case Model.views_of(meta.model, types) do
          [] -> refused(id, nil, :no_view, Enum.map(types, &elem(&1, 0)))
          [view | _] -> mode(meta, view, id, entity, types)
        end
    end
  end

  defp mode(meta, view, id, entity, types) do
    case types != [] && Model.tombstone(meta.model, types) do
      %{} = tombstone ->
        {id, {:tombstone, entity, tombstone, view}}

      _ ->
        case files(meta, entity) do
          [] -> {id, {:remove, entity, view}}
          attrs -> refused(id, view, :has_file, attrs)
        end
    end
  end

  # the attributes of a record that a view does not have
  defp outside(meta, view, entity) do
    for attr <- Enum.sort(Map.keys(entity)),
        not Map.has_key?(view.field_of, attr),
        attr != @owner,
        not Model.made?(meta.model, attr),
        do: attr
  end

  defp files(meta, entity) do
    for attr <- Enum.sort(Map.keys(entity)), match?(%{file: true}, meta.model.attrs[attr]), do: attr
  end

  defp refused(id, view, reason, attributes),
    do: {:refused, %{id: id, view: view && view.module, reason: reason, attributes: attributes}}

  # follow the references to the records that go
  defp walk(_meta, state, []), do: {:ok, state}

  defp walk(meta, state, ids) do
    rules = Model.delete_rules(meta.model)

    Enum.reduce_while(rules, {:ok, state, []}, fn {attr, spec}, {:ok, state, next} ->
      pairs = referring(meta, attr, ids)

      case spec.on_delete do
        :restrict ->
          {:cont, {:ok, %{state | restrict: state.restrict ++ Enum.map(pairs, fn [x, _] -> {x, attr} end)}, next}}

        :nilify ->
          nilify =
            Enum.reduce(pairs, state.nilify, fn [x, target], acc ->
              Map.update(acc, x, [{attr, target}], &[{attr, target} | &1])
            end)

          {:cont, {:ok, %{state | nilify: nilify}, next}}

        :cascade ->
          fresh =
            pairs
            |> Enum.map(&hd/1)
            |> Enum.uniq()
            |> Enum.reject(&(Map.has_key?(state.plan, &1) or MapSet.member?(state.gone, &1)))

          # one that is a tombstone already has gone
          kids = meta |> Backend.read(fresh) |> Enum.reject(fn {_, e} -> Model.tombstoned?(meta.model, e) end)

          case each(kids, fn {id, entity} -> child(meta, attr, id, entity) end) do
            {:ok, planned} ->
              state = %{state | plan: Map.merge(state.plan, Map.new(planned))}
              {:cont, {:ok, state, next ++ Enum.map(planned, &elem(&1, 0))}}

            refused ->
              {:halt, refused}
          end
      end
    end)
    |> case do
      {:ok, state, next} -> walk(meta, state, next)
      refused -> refused
    end
  end

  # [[id, target]] of the records whose `attr` is one of `ids`
  defp referring(meta, attr, ids) do
    ids
    |> Enum.chunk_every(@chunk)
    |> Enum.flat_map(fn chunk ->
      req = %{
        q: "[:find ?e ?v :in $ [?v ...] :where [?e #{Model.kw(attr)} ?v]]",
        inputs: [[:coll, chunk]]
      }

      meta |> Backend.query(req) |> Backend.ok!()
    end)
    |> Enum.sort()
  end

  # [{id, constraint name}] of the records a :restrict holds on to
  defp blocked(meta, state) do
    held =
      for {x, attr} <- state.restrict,
          not Map.has_key?(state.plan, x),
          not MapSet.member?(state.gone, x),
          do: {x, attr}

    entities = Map.new(Backend.read(meta, held |> Enum.map(&elem(&1, 0)) |> Enum.uniq()))
    for {x, attr} <- held, do: {x, constraint_name(meta, attr, entities[x])}
  end

  # the name `foreign_key_constraint/3` expects: of the view the record is seen through
  defp constraint_name(meta, attr, entity) do
    case meta.model.attrs[attr].fk_name do
      nil ->
        holders = for {_, v} <- Enum.sort(meta.model.views), Map.has_key?(v.field_of, attr), do: v
        view = Enum.find(holders, &Model.member?(&1, entity || %{})) || List.first(holders)
        "#{if view, do: view.source, else: "hosi"}_#{attr}_fkey"

      name ->
        name
    end
  end

  ## ------------------------------------------------------------ the ops

  defp ops(meta, root_view, state) do
    deleted =
      for {id, how} <- Enum.sort(state.plan) do
        case how do
          {:remove, entity, _view} -> remove(meta, id, entity)
          {:tombstone, entity, tombstone, view} -> bury(meta, id, entity, tombstone, [view, root_view])
        end
      end

    # a record that is being deleted is not also nilified
    nilified =
      for {id, refs} <- Enum.sort(state.nilify),
          not Map.has_key?(state.plan, id),
          not MapSet.member?(state.gone, id) do
        entity = Backend.read_one(meta, id)

        changes =
          refs
          |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
          |> Enum.map(fn {attr, targets} ->
            case Map.get(entity, attr, []) -- targets do
              [] -> {attr, nil}
              left -> {attr, left}
            end
          end)

        {ops, conds} = Change.update(meta, id, entity, changes)
        # the references go only while they still are what was read
        held = for {attr, target} <- refs, do: [:holds, id, attr, target]
        {ops, Enum.uniq(conds ++ held)}
      end

    {ops, conds} = Enum.unzip(deleted ++ nilified)
    {Enum.concat(ops), Enum.concat(conds)}
  end

  # every fact goes (a tuple is hosigraph's own, and goes with its parts)
  defp remove(meta, id, entity) do
    facts =
      for {attr, values} <- Enum.sort(entity),
          not match?(%{tuple: t} when t != nil, meta.model.attrs[attr]),
          v <- values,
          do: {attr, v}

    {for({attr, v} <- facts, do: [:delete, id, attr, Backend.wrap(meta, attr, v)]),
     for({attr, v} <- facts, do: [:holds, id, attr, v])}
  end

  defp bury(meta, id, entity, tombstone, views) do
    set =
      Map.new(tombstone.set)
      |> Map.merge(
        case tombstone.mark do
          nil -> %{}
          {attr, type} -> %{attr => now(type)}
        end
      )

    gone =
      for {attr, _} <- entity,
          spec = meta.model.attrs[attr],
          not Map.has_key?(set, attr),
          not kept?(spec, attr, tombstone),
          into: %{},
          do: {attr, nil}

    changes = Map.merge(gone, set)
    # the record type changes here, which an update may not do
    derived = Ecto.Hosi.Derived.changes(meta.model, changes, entity)

    {ops, conds} =
      (Enum.sort(changes) ++ derived)
      |> Enum.map(fn {attr, value} ->
        old = Map.get(entity, attr, [])
        new = Store.elements(meta.model, attr, value)

        ops =
          for(v <- old -- new, do: [:delete, id, attr, Backend.wrap(meta, attr, v)]) ++
            for(v <- new -- old, do: [:create, id, attr, Backend.wrap(meta, attr, v)])

        {ops, for(v <- old, do: [:holds, id, attr, v])}
      end)
      |> Enum.unzip()

    # and nothing of the views has appeared since the record was read: it would stay in the tombstone
    appeared =
      for view <- views,
          view != nil,
          attr <- Map.keys(view.field_of),
          not Map.has_key?(entity, attr),
          uniq: true,
          do: [:unset, id, attr]

    {Enum.concat(ops), Enum.concat(conds) ++ Enum.sort(appeared)}
  end

  # a tombstone keeps what it is told to, and what cannot change or names a file
  defp kept?(_spec, @owner, _tombstone), do: true
  defp kept?(nil, attr, tombstone), do: MapSet.member?(tombstone.keep, attr)

  defp kept?(spec, attr, tombstone),
    do: spec.final or spec.file or spec.made != nil or MapSet.member?(tombstone.keep, attr)

  # the time of the delete, as the mark's field keeps times
  defp now(type) do
    {:ok, cast} = Ecto.Type.cast(type, DateTime.utc_now())
    {:ok, dumped} = Ecto.Type.adapter_dump(Ecto.Adapters.Hosi, type, cast)
    dumped
  end

  ## ------------------------------------------------------------ messages

  @doc "What to tell somebody whose delete met `{:refused, info}`."
  def message(meta, %{id: id, view: view, reason: reason, attributes: attributes}) do
    through = if view, do: " through #{inspect(view)}", else: ""
    facts = Enum.map_join(attributes, ", ", &Model.kw/1)

    why =
      case reason do
        :other_facts ->
          "it also has #{facts}, which this view does not show. Nothing was written. To take the " <>
            "record out of a view, update the view's fields to nil. To delete it, call " <>
            "Repo.delete through a view that covers its facts"

        :not_type_view ->
          types = Model.record_types(meta.model, Backend.read_one(meta, id))
          named = Enum.map_join(types, ", ", fn {a, v} -> "#{Model.kw(a)} #{inspect(v)}" end)

          ways =
            case Model.views_of(meta.model, types) do
              [] -> "no view of the Repo is one of that type"
              views -> "call Repo.delete through " <> Enum.map_join(views, " or ", &inspect(&1.module))
            end

          "it is #{named}, and only a view of that type deletes it. Nothing was written. To " <>
            "take it out of this view, update the view's fields to nil. To delete it, #{ways}"

        :no_view ->
          "it has the record type #{facts}, and no view of the Repo is one of that type. " <>
            "Nothing was written"

        :has_file ->
          "it names a file (#{facts}), and a record with a file is not removed outright: " <>
            "declare a tombstone (hosi_tombstone) for its record type. Nothing was written"
      end

    "Ecto.Adapters.Hosi will not delete record #{id}#{through}: #{why}. (ecto_hosi's limit, not hosigraph's)"
  end
end
