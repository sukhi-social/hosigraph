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

        Map.update(acc, parent, [ref], &(&1 ++ [ref]))
    end
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
            "#{source} is append-only: its rows are never updated or deleted"
    end

    :ok
  end

  @doc """
  The write that deletes `rows` (`[{id, entity}]` of `source`) and what hangs off them:
  `{:ok, ops, conds}`, or `{:restrict, constraint_name}` when a row a `:restrict`/`:none` key
  points at would be left behind.
  """
  def plan(meta, source, rows) do
    state = %{deleted: %{}, nilify: [], restrict: nil}
    state = Enum.reduce(rows, state, fn {id, e}, st -> put_deleted(st, source, id, e) end)
    state = walk(meta, source, rows, state)

    case state.restrict do
      nil ->
        {ops, conds} = ops(meta, state)
        {:ok, ops, conds}

      name -> {:restrict, name}
    end
  end

  defp put_deleted(st, source, id, entity),
    do: %{st | deleted: Map.put(st.deleted, id, {source, entity})}

  defp walk(meta, source, rows, st) do
    refs = Map.get(Map.get(meta, :refs, %{}), source, [])
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

  defp ops(meta, st) do
    deletes =
      for {id, {source, entity}} <- st.deleted, reduce: {[], []} do
        {ops, conds} ->
          {ops ++ Backend.retract_all(meta, id, entity),
           conds ++ [[:holds, id, Attr.marker(source), true]]}
      end

    # a row that is being deleted is not also nilified
    nilifies =
      for {ref, ids} <- Enum.reverse(st.nilify),
          id <- ids,
          not Map.has_key?(st.deleted, id),
          reduce: {[], []} do
        {ops, conds} ->
          [entity] = meta |> Backend.get_entities([id]) |> Backend.ok!()
          attrs = Derived.extend(meta, ref.child, id, [{ref.attr, nil}], fn -> entity end)
          {o, c} = Update.set_ops(meta, id, entity, attrs)
          {ops ++ o, conds ++ [[:holds, id, Attr.marker(ref.child), true] | c]}
      end

    {dops, dconds} = deletes
    {nops, nconds} = nilifies
    {dops ++ nops, dconds ++ nconds}
  end
end
