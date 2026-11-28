defmodule Ecto.Hosi.Tx do
  @moduledoc false
  # `Repo.transaction/2`, `Repo.rollback/1` and `Ecto.Multi`.
  #
  # hosigraph writes ONE batch atomically; it has no begin/commit around many steps, and a step
  # often needs the id an earlier step made. So a transaction here runs its writes at once and
  # remembers how to take them back. On rollback (`Repo.rollback/1`, a `Multi` step that fails, or
  # an exception) the changes are undone with one more write: rows made in the transaction are
  # removed, and rows it changed or deleted get their old values back.
  #
  # That is NOT a SQL transaction:
  #
  #   * other processes see the writes before the commit, and still see them if a rollback undoes them
  #   * a crash of the node between the writes loses the rollback (use `Repo.write_batch/2` where
  #     that matters: it is one atomic write)
  #   * hosigraph keeps history, so the undone writes stay in `Repo.history/1`
  #   * `Repo.erase/1` is not undone
  #
  # A nested `transaction` joins the outer one; rolling it back marks the outer one failed.

  alias Ecto.Hosi.Backend

  def active?(meta), do: Process.get(key(meta)) != nil

  def run(meta, fun) do
    key = key(meta)

    case Process.get(key) do
      nil -> outer(meta, key, fun)
      _ -> nested(key, fun)
    end
  end

  def rollback(meta, value) do
    key = key(meta)

    if Process.get(key) == nil do
      raise ArgumentError, "cannot call rollback outside of a transaction"
    end

    throw({:ecto_hosi_rollback, key, value})
  end

  defp outer(meta, key, fun) do
    Process.put(key, %{snapshots: %{}, created: [], failed: false})

    try do
      result = fun.()
      st = Process.get(key)

      if st.failed do
        undo(meta, st)
        {:error, :rollback}
      else
        {:ok, result}
      end
    catch
      {:ecto_hosi_rollback, ^key, value} ->
        undo(meta, Process.get(key))
        {:error, value}

      kind, reason ->
        stacktrace = __STACKTRACE__
        undo(meta, Process.get(key))
        :erlang.raise(kind, reason, stacktrace)
    after
      Process.delete(key)
    end
  end

  defp nested(key, fun) do
    {:ok, fun.()}
  catch
    {:ecto_hosi_rollback, ^key, value} ->
      Process.put(key, %{Process.get(key) | failed: true})
      {:error, value}
  end

  ## ---------------------------------------------------------------- recording (Backend.write)

  @doc "Before a write: remember what the rows it touches held."
  def before_write(meta, ops) do
    key = key(meta)
    st = Process.get(key)

    ids =
      for [_kind, id | _] <- ops, is_integer(id), not Map.has_key?(st.snapshots, id), uniq: true,
          do: id

    if ids != [] do
      entities = meta.backend.get_entities(meta.conn, ids) |> Backend.ok!()
      snaps = Map.merge(st.snapshots, Map.new(Enum.zip(ids, entities)))
      Process.put(key, %{st | snapshots: snaps})
    end

    :ok
  end

  @doc "After a write: remember the rows it made."
  def after_write(meta, report) do
    key = key(meta)
    st = Process.get(key)

    made =
      for t <- Map.get(report, :temps) || [],
          id = temp_entity(t),
          do: id

    Process.put(key, %{st | created: st.created ++ made})
    :ok
  end

  defp temp_entity([_n, id]), do: id
  defp temp_entity({_n, id}), do: id
  defp temp_entity(_), do: nil

  ## ---------------------------------------------------------------- undo

  defp undo(meta, st) do
    created = Enum.uniq(st.created)
    restore = for {id, _} <- st.snapshots, id not in created, do: id
    ids = created ++ restore

    if ids != [] do
      current = meta.backend.get_entities(meta.conn, ids) |> Backend.ok!() |> then(&Enum.zip(ids, &1))

      {deletes, creates} =
        Enum.reduce(current, {[], []}, fn {id, now}, {del, add} ->
          before = if id in created, do: %{}, else: Map.get(st.snapshots, id, %{})
          {d, a} = diff(meta, id, now, before)
          {del ++ d, add ++ a}
        end)

      ops = deletes ++ creates

      if ops != [] do
        case Backend.send_write(meta, ops) do
          {:ok, _} -> :ok
          {:error, reason} -> raise Ecto.Hosi.Error, reason: reason
        end
      end
    end

    :ok
  end

  # What takes an entity from `now` back to `before`.
  defp diff(meta, id, now, before) do
    attrs =
      (Map.keys(now) ++ Map.keys(before))
      |> Enum.uniq()
      |> Enum.reject(&match?(%{tuple: parts} when parts != nil, Map.get(meta.model.attrs, &1)))

    Enum.reduce(attrs, {[], []}, fn attr, {del, add} ->
      cur = Map.get(now, attr, [])
      old = Map.get(before, attr, [])

      {del ++ for(v <- cur -- old, do: [:delete, id, attr, Backend.wrap(meta, attr, v)]),
       add ++ for(v <- old -- cur, do: [:create, id, attr, Backend.wrap(meta, attr, v)])}
    end)
  end

  defp key(meta), do: {__MODULE__, meta.conn}
end
