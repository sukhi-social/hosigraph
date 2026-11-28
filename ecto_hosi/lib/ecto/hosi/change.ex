defmodule Ecto.Hosi.Change do
  @moduledoc false
  # What a create or an update writes, with the rules of the attributes. Every way of writing
  # comes through here: insert, insert_all, update, update_all, on_conflict, write_batch and
  # write_facts.
  #
  # Values are in the adapter's form (`Ecto.Hosi.Store`). `nil` removes; a value of an attribute
  # with many values is the whole set.

  alias Ecto.Hosi.{Backend, Derived, Model, Store}

  @owner :"hosi/owner"

  ## ------------------------------------------------------------ create

  @doc """
  The facts of a new record made through `view` from `values` (`[{attr, value | nil}]`):
  `%{attr => value}`, with the facts the view's `hosi_where` writes, the owner, and what the
  unique rules derive.
  """
  def new(meta, view, values), do: add_derived(meta, base(meta, view, values))

  @doc "As `new/3`, before the unique rules derive anything (`write_batch` may add to it)."
  def base(meta, view, values) do
    facts = for {attr, v} <- values, not empty?(v), into: %{}, do: {attr, v}
    facts = Enum.reduce(view.where, facts, &where!(meta, view, &1, &2))
    owned(meta, view, facts)
  end

  @doc "`facts` with what the unique rules derive from them."
  def add_derived(meta, facts) do
    derived = Derived.changes(meta.model, facts, %{})
    Enum.reduce(derived, facts, fn {attr, v}, acc -> if is_nil(v), do: acc, else: Map.put(acc, attr, v) end)
  end

  defp empty?(nil), do: true
  defp empty?([]), do: true
  defp empty?(_), do: false

  defp where!(_meta, view, {attr, :present}, facts) do
    unless Map.has_key?(facts, attr) do
      raise ArgumentError,
            "a record made through #{inspect(view.module)} shows up there only if it has " <>
              "#{Model.kw(attr)}: give the field #{inspect(view.field_of[attr])} a value " <>
              "(hosi_where)"
    end

    facts
  end

  defp where!(meta, view, {attr, {:eq, value}}, facts) do
    case {meta.model.attrs[attr].card, facts} do
      {:many, _} ->
        Map.update(facts, attr, [value], fn vs -> Enum.uniq([value | vs]) end)

      {:one, %{^attr => ^value}} ->
        facts

      {:one, %{^attr => other}} ->
        raise ArgumentError,
              "a record made through #{inspect(view.module)} has #{Model.kw(attr)} " <>
                "#{inspect(value)} (hosi_where); it was given #{inspect(other)}"

      {:one, _} ->
        Map.put(facts, attr, value)
    end
  end

  # the maker: a reference to whoever the view says owns what it makes
  defp owned(_meta, %{owner: :nobody}, facts), do: facts

  defp owned(_meta, %{owner: attr} = view, facts) do
    case facts do
      %{^attr => maker} ->
        case facts do
          %{@owner => other} when other != maker ->
            raise ArgumentError,
                  "a record made through #{inspect(view.module)} is owned by its " <>
                    "#{inspect(view.field_of[attr])}; hosi/owner was given another value"

          _ ->
            Map.put(facts, @owner, maker)
        end

      _ ->
        raise ArgumentError,
              "a record made through #{inspect(view.module)} is owned by its maker: give the " <>
                "field #{inspect(view.field_of[attr])} a value (hosi_owner)"
    end
  end

  @doc "The ops that create a record with these facts under `ref` (an id, or `[:tmp, n]`)."
  def create_ops(meta, ref, facts) do
    for {attr, value} <- Enum.sort(facts),
        element <- Store.elements(meta.model, attr, value),
        do: [:create, ref, attr, Backend.wrap(meta, attr, element)]
  end

  ## ------------------------------------------------------------ update

  @doc """
  Whether an update through `view` may reach a record that holds `entity`: it has to exist, and a
  view of a record type changes only records of that type.
  """
  def reachable?(_view, entity) when map_size(entity) == 0, do: false
  def reachable?(nil, _entity), do: true

  def reachable?(view, entity),
    do: Enum.all?(view.record_types, fn {attr, v} -> v in Map.get(entity, attr, []) end)

  @doc """
  `{ops, conds}` that make `changes` (`[{attr, value | nil}]`) in the record `id`, which holds
  `entity` now. `conds` holds what the ops count on, and not that the record exists
  (`existence/3`).
  """
  def update(meta, id, entity, changes) do
    changes = Map.new(changes)
    allowed!(meta, changes, entity)
    derived = Derived.changes(meta.model, changes, entity)

    changes
    |> Enum.sort()
    |> Kernel.++(derived)
    |> Enum.map(fn {attr, value} -> set(meta, id, entity, attr, value) end)
    |> Enum.unzip()
    |> then(fn {ops, conds} -> {Enum.concat(ops), Enum.concat(conds)} end)
  end

  @doc """
  What a record that holds `entity` holds of the attributes that `changes` touch, once they are
  made: `%{attribute => value | nil}`, with what the unique rules derive.
  """
  def sets(meta, changes, entity) do
    changed = Map.new(changes)
    Map.merge(changed, Map.new(Derived.changes(meta.model, changed, entity)))
  end

  # the rules that look at what a record holds now
  defp allowed!(meta, changes, entity) do
    for {attr, value} <- changes, spec = meta.model.attrs[attr], spec != nil do
      old = entity |> Map.get(attr, []) |> Enum.sort()
      new = meta.model |> Store.elements(attr, value) |> Enum.sort()

      cond do
        old == [] or old == new ->
          :ok

        spec.record_type ->
          raise Ecto.Hosi.Error,
            reason: {:record_type, attr},
            message:
              "#{Model.kw(attr)} says what a record is (#{inspect(hd(old))}): an update cannot " <>
                "change it. Delete does that, and leaves the tombstone. (ecto_hosi's limit)"

        spec.final ->
          raise Ecto.Hosi.Error,
            reason: {:final, attr},
            message:
              "#{Model.kw(attr)} is final: it is #{inspect(show(old))} and cannot be changed " <>
                "or removed (ecto_hosi's limit)"

        true ->
          :ok
      end
    end
  end

  defp show([one]), do: one
  defp show(many), do: many

  defp set(meta, id, entity, attr, value) do
    old = Map.get(entity, attr, [])
    new = Store.elements(meta.model, attr, value)
    spec = Map.get(meta.model.attrs, attr)
    many? = match?(%{card: :many}, spec)

    gone =
      cond do
        many? -> old -- new
        new == [] -> old
        true -> []
      end

    added = new -- old

    ops =
      for(v <- gone, do: [:delete, id, attr, Backend.wrap(meta, attr, v)]) ++
        for(v <- added, do: [:create, id, attr, Backend.wrap(meta, attr, v)])

    # a final value is set only where there is none; a removal takes what was read
    first? = old == [] and added != [] and match?(%{final: true}, spec)

    conds =
      for(v <- gone, do: [:holds, id, attr, v]) ++ if(first?, do: [[:unset, id, attr]], else: [])

    {ops, conds}
  end

  @doc """
  The condition (a list of one) that holds while the record is the one that was read: on its
  record type, a final fact, its owner, what the view asks of it, or else any fact it has. `[]`
  when it has no fact.
  """
  def existence(meta, view, id, entity) do
    attrs = meta.model.attrs

    rank = fn attr ->
      spec = Map.get(attrs, attr)

      cond do
        spec == nil -> 5
        spec.made != nil -> 9
        spec.record_type -> 0
        spec.final and attr != @owner -> 1
        attr == @owner -> 2
        view != nil and List.keymember?(view.where, attr, 0) -> 3
        true -> 4
      end
    end

    entity
    |> Enum.reject(fn {attr, values} -> values == [] or match?(%{tuple: t} when t != nil, attrs[attr]) end)
    |> Enum.min_by(fn {attr, _} -> {rank.(attr), attr} end, fn -> nil end)
    |> case do
      nil -> []
      {attr, [v | _]} -> [[:holds, id, attr, v]]
    end
  end

  @doc """
  `facts` of a record that is not written yet, with `changes` made to them. As for `base/3`, what
  the unique rules derive is added at the end (`add_derived/2`).
  """
  def amend(meta, facts, changes) do
    allowed!(meta, Map.new(changes), entity(meta, facts))

    Enum.reduce(changes, facts, fn {attr, v}, acc ->
      if empty?(v), do: Map.delete(acc, attr), else: Map.put(acc, attr, v)
    end)
  end

  @doc "The entity (`%{attribute => values}`) that `facts` (`%{attribute => value}`) would be."
  def entity(meta, facts),
    do: Map.new(facts, fn {attr, v} -> {attr, Store.elements(meta.model, attr, v)} end)
end
