defmodule Ecto.Hosi.Cascade do
  @moduledoc false
  # What deleting rows does to the rows that point at them (`hosi_ref`), and the append-only rule
  # (`hosi_append_only`). hosigraph has no foreign keys, so the adapter reads the children and
  # plans their retraction or nilification into the SAME write as the delete, guarded by `holds`
  # conditions, so a child that appeared meanwhile fails the write rather than being orphaned.

  alias Ecto.Hosi.{Attr, Backend, Derived, Update}

  @chunk 500

  @doc "`%{parent_source => [%{child, field, attr, action, name}]}` from the schemas' `hosi_ref`s."
  def build(schemas) do
    for mod <- schemas,
        function_exported?(mod, :__hosi__, 1),
        {field, opts} <- mod.__hosi__(:refs),
        reduce: %{} do
      acc ->
        child = mod.__schema__(:source)
        to = Keyword.fetch!(opts, :to)
        parent = if is_atom(to), do: to.__schema__(:source), else: to

        ref = %{
          child: child,
          field: field,
          attr: Attr.attr(child, field),
          action: Keyword.get(opts, :on_delete, :none),
          name: Keyword.get(opts, :name, "#{child}_#{field}_fkey")
        }

        # two schemas of one source may declare the same key
        Map.update(acc, parent, [ref], &Enum.uniq(&1 ++ [ref]))
    end
  end

  @doc """
  The `belongs_to` keys of these schemas that say nothing about a delete of what they point at, and
  so are left pointing at nothing: `[{schema, key, parent}]`. Only when the parent is kept here too.
  A parent's `has_many .., on_delete:` counts as saying it.
  """
  def undeclared(schemas) do
    declared =
      for mod <- schemas,
          function_exported?(mod, :__hosi__, 1),
          {field, _} <- mod.__hosi__(:refs),
          into: MapSet.new(),
          do: {mod.__schema__(:source), field}

    listed = MapSet.new(schemas, & &1.__schema__(:source))

    for mod <- schemas,
        name <- mod.__schema__(:associations),
        %Ecto.Association.BelongsTo{owner_key: key, related: parent} <-
          [mod.__schema__(:association, name)],
        Code.ensure_loaded?(parent),
        MapSet.member?(listed, parent.__schema__(:source)),
        not MapSet.member?(declared, {mod.__schema__(:source), key}),
        not parent_says?(parent, mod, key),
        do: {mod, key, parent}
  end

  defp parent_says?(parent, child, key) do
    Enum.any?(parent.__schema__(:associations), fn name ->
      match?(
        %Ecto.Association.Has{related: ^child, related_key: ^key, on_delete: on_delete}
        when on_delete != :nothing,
        parent.__schema__(:association, name)
      )
    end)
  end

  @doc "The sources of the schemas declared append-only."
  def append_only(schemas) do
    for mod <- schemas,
        function_exported?(mod, :__hosi__, 1),
        mod.__hosi__(:append_only),
        into: MapSet.new(),
        do: mod.__schema__(:source)
  end

  def append_only!(meta, source) do
    if MapSet.member?(Map.get(meta, :append_only, MapSet.new()), source) do
      raise ArgumentError,
            "#{source} is append-only: its rows are never updated or deleted " <>
              "(hosi_append_only: ecto_hosi's limit, not hosigraph's)"
    end

    :ok
  end

  @doc """
  The write that deletes `rows` (`[{id, entity}]` of `source`) and what hangs off them:
  `{:ok, ops, conds}`; or `{:restrict, constraint_name}` when a row a `:restrict`/`:none` key
  points at would be left behind; or `{:other_facts, %{id, source, attributes}}` when a row to be
  deleted holds facts that are not its schema's, which the delete would remove with it.
  """
  def plan(meta, source, rows) do
    state = %{deleted: %{}, nilify: [], restrict: nil}
    state = Enum.reduce(rows, state, fn {id, e}, st -> put_deleted(st, source, id, e) end)
    state = walk(meta, source, rows, state)

    case {state.restrict, other_facts(state.deleted)} do
      {nil, nil} ->
        {ops, conds} = ops(meta, state)
        {:ok, ops, conds}

      {nil, found} ->
        {:other_facts, found}

      {name, _} ->
        {:restrict, name}
    end
  end

  # A schema's own facts are the ones under its name (`:"user/.."` for "user"). Anything else is
  # another schema's, or a direct write's, and is not removed by deleting this one.
  defp other_facts(deleted) do
    deleted
    |> Enum.sort()
    |> Enum.find_value(fn {id, {source, entity}} ->
      others =
        for attr <- Map.keys(entity),
            not String.starts_with?(Atom.to_string(attr), source <> "/"),
            do: attr

      if others != [], do: %{id: id, source: source, attributes: Enum.sort(others)}
    end)
  end

  @doc "What to tell somebody whose delete met `{:other_facts, found}`."
  def other_facts_message(%{id: id, source: source, attributes: attributes}) do
    "Ecto.Adapters.Hosi will not delete record #{id} through #{inspect(source)}: it also has " <>
      "#{Enum.map_join(attributes, ", ", &inspect_attr/1)}, which the delete would remove " <>
      "with it. Nothing was written. If those facts are meant to go, retract them first " <>
      "([:delete, id, attribute, value] through Hosi.write/2). (ecto_hosi's limit, not hosigraph's)"
  end

  defp inspect_attr(attr), do: ":" <> Atom.to_string(attr)

  defp put_deleted(st, source, id, entity),
    do: %{st | deleted: Map.put(st.deleted, id, {source, entity})}

  defp walk(meta, source, rows, st) do
    refs = Enum.reject(Map.get(Map.get(meta, :refs, %{}), source, []), &(&1.action == :keep))
    ids = Enum.map(rows, &elem(&1, 0))

    if refs == [] or ids == [] do
      st
    else
      Enum.reduce(refs, st, fn ref, st ->
        pairs = children(meta, ref, ids)
        fresh = pairs |> Enum.map(&hd/1) |> Enum.uniq() |> Enum.reject(&Map.has_key?(st.deleted, &1))

        cond do
          st.restrict != nil or fresh == [] ->
            st

          ref.action in [:restrict, :none] ->
            %{st | restrict: ref.name}

          ref.action == :cascade ->
            append_only!(meta, ref.child)
            entities = meta |> Backend.get_entities(fresh) |> Backend.ok!()
            kids = Enum.zip(fresh, entities)
            st = Enum.reduce(kids, st, fn {id, e}, st -> put_deleted(st, ref.child, id, e) end)
            walk(meta, ref.child, kids, st)

          ref.action == :nilify ->
            append_only!(meta, ref.child)
            %{st | nilify: [{ref, fresh} | st.nilify]}
        end
      end)
    end
  end

  # [[child_id, parent_id]] of the live rows of the child schema that point at one of `ids`.
  defp children(meta, ref, ids) do
    marker = Attr.marker(ref.child)

    ids
    |> Enum.chunk_every(@chunk)
    |> Enum.flat_map(fn chunk ->
      req = %{
        q:
          "[:find ?e ?v :in $ [?v ...] :where [?e #{Attr.kw(marker)} _] [?e #{Attr.kw(ref.attr)} ?v]]",
        inputs: [[:coll, chunk]]
      }

      meta |> Backend.query(req) |> Backend.ok!()
    end)
  end

  # `{ops, conds}` of the whole delete: made row by row, and joined once at the end
  defp ops(meta, st) do
    deletes =
      for {id, {source, entity}} <- st.deleted do
        {Backend.retract_all(meta, id, entity), [[:holds, id, Attr.marker(source), true]]}
      end

    # a row that is being deleted is not also nilified
    nilifies =
      for {ref, ids} <- Enum.reverse(st.nilify),
          id <- ids,
          not Map.has_key?(st.deleted, id) do
        [entity] = meta |> Backend.get_entities([id]) |> Backend.ok!()
        attrs = Derived.extend(meta, ref.child, id, [{ref.attr, nil}], fn -> entity end)
        {o, c} = Update.set_ops(meta, id, entity, attrs)
        {o, [[:holds, id, Attr.marker(ref.child), true] | c]}
      end

    {ops, conds} = Enum.unzip(deletes ++ nilifies)
    {Enum.concat(ops), Enum.concat(conds)}
  end
end
