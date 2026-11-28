defmodule Ecto.Hosi.Account do
  @moduledoc """
  `Repo.erase_account/2`: erase what an account made, and keep the account as its tombstone.

      {:ok, report} =
        MyApp.Repo.erase_account(actor, delete_files: &MyApp.Files.delete_all/1)

  The account is a struct (or the id) of a record whose type has a tombstone (`hosi_tombstone`).
  What it made is every record whose `hosi/owner` is the account. In this order:

    1. Checks. A delete of the account's record has to be possible once its records are gone, and
       `on_delete` has to allow what happens to other records' references.
    2. **Files.** The values of the file attributes (`hosi_file`) of the records it made go to
       `delete_files:`, which answers `:ok` or `{:error, reason}`. An error stops here: nothing
       in the database has changed. It has to cope with a file that is gone already.
    3. What the references of other records to those records ask for (`on_delete`) is written.
    4. hosigraph **erases** every record that ever had the account as `hosi/owner`.
    5. The account's record becomes its tombstone, as `Repo.delete` would leave it.

  It can be run again after an interruption: each step finds less to do. **Erase cannot be
  undone**, so a grace period, and telling other servers, come before this call. The bytes leave
  the disk when `Repo.clean/1` has run (call it when the server is quiet).

  Answers `{:ok, %{erased: ids, left: [{id, constraint_name}]}}`: `left` are records of others
  that refer to an erased record by a `:restrict` reference (an erase is not refused; they are
  left as they are). Or

    * `{:error, :stale}` – there is no such record, or a record changed meanwhile (run it again)
    * `{:error, {:files, reason}}` – `delete_files:` answered an error
    * `{:error, {:foreign_key, name}}` – a record of another holds on to the account by a
      `:restrict` reference; nothing was changed

  `timeout:` is how long each write is waited for. Not inside a transaction.
  """

  alias Ecto.Hosi.{Backend, Delete, Model, RepoApi}

  @doc false
  def erase(repo, account, opts \\ []) do
    meta = repo |> RepoApi.meta() |> Backend.with_timeout(opts)

    if Ecto.Hosi.Tx.active?(meta) do
      raise ArgumentError,
            "Repo.erase_account cannot be undone, so it does not run inside a transaction"
    end

    id = RepoApi.entity_id(account, "Repo.erase_account/2")
    entity = Backend.read_one(meta, id)

    if entity == %{} do
      {:error, :stale}
    else
      if id in Map.get(entity, Model.owner(), []) do
        raise ArgumentError,
              "Repo.erase_account: record #{id} is its own hosi/owner, so erasing what it owns " <>
                "would erase it too. An account's view says hosi_owner :nobody"
      end

      # nil for an account that is a tombstone already (a run again)
      view = unless Model.tombstoned?(meta.model, entity), do: view!(meta, id, entity)
      owned = owned(meta, id)

      with :ok <- possible(meta, view, id, entity, owned),
           :ok <- delete_files(meta, owned, opts),
           {:ok, left} <- references(meta, owned),
           %{owned: erased} <- RepoApi.erase!(meta, [[:"owned-by", [Model.owner()], [id]]]),
           :ok <- bury(meta, view, id) do
        {:ok, %{erased: erased, left: left}}
      end
    end
  end

  # the view whose tombstone the account becomes
  defp view!(meta, id, entity) do
    types = Model.record_types(meta.model, entity)

    meta.model |> Model.views_of(types) |> Enum.find(& &1.tombstone) ||
      raise ArgumentError,
            "Repo.erase_account keeps the account as a tombstone, and no view of the type of " <>
              "record #{id} (#{inspect(types)}) declares one (hosi_tombstone). Nothing was changed"
  end

  defp owned(meta, id) do
    request = %{
      q: "[:find ?e :in $ ?o :where [?e #{Model.kw(Model.owner())} ?o]]",
      inputs: [[:scalar, id]]
    }

    meta |> Backend.query(request) |> Backend.ok!() |> Enum.map(fn [e] -> e end) |> Enum.sort()
  end

  # is every later step allowed? asked before anything is changed
  defp possible(meta, view, id, entity, owned) do
    case Delete.after_erase(meta, owned) do
      {:ok, _, _, _} -> if view, do: plan(meta, view, id, entity, owned) |> checked(), else: :ok
      {:refused, info} -> refused!(meta, info)
    end
  end

  defp checked({:ok, _, _}), do: :ok
  defp checked(other), do: other

  # the tombstone of the account: `{:ok, ops, conds}` or `{:error, {:foreign_key, name}}`
  defp plan(meta, view, id, entity, gone) do
    case Delete.plan(meta, view, [{id, entity}], gone) do
      {:ok, ops, conds} -> {:ok, ops, conds}
      {:restrict, name} -> {:error, {:foreign_key, name}}
      {:refused, info} -> refused!(meta, info)
    end
  end

  defp refused!(meta, info),
    do: raise(Ecto.Hosi.Error, reason: {:refused, info}, message: Delete.message(meta, info))

  defp delete_files(meta, owned, opts) do
    paths =
      for {_, entity} <- Backend.read(meta, owned),
          {attr, values} <- entity,
          match?(%{file: true}, meta.model.attrs[attr]),
          value <- values,
          uniq: true,
          do: value

    case {paths, opts[:delete_files]} do
      {[], _} ->
        :ok

      {_, nil} ->
        raise ArgumentError,
              "Repo.erase_account: the account's records name #{length(paths)} file(s): give " <>
                "delete_files: fn paths -> :ok end, which deletes them before the records are " <>
                "erased (an erase also removes the names). Nothing was changed"

      {_, fun} ->
        case fun.(Enum.sort(paths)) do
          :ok ->
            :ok

          {:error, reason} ->
            {:error, {:files, reason}}

          other ->
            raise ArgumentError,
                  "delete_files: answers :ok or {:error, reason}, got #{inspect(other)}"
        end
    end
  end

  defp references(meta, owned) do
    case Delete.after_erase(meta, owned) do
      {:ok, ops, conds, left} -> with :ok <- write(meta, ops, conds), do: {:ok, left}
      {:refused, info} -> refused!(meta, info)
    end
  end

  defp bury(_meta, nil, _id), do: :ok

  defp bury(meta, view, id) do
    entity = Backend.read_one(meta, id)

    if Model.tombstoned?(meta.model, entity) do
      :ok
    else
      with {:ok, ops, conds} <- plan(meta, view, id, entity, []), do: write(meta, ops, conds)
    end
  end

  defp write(_meta, [], _conds), do: :ok

  defp write(meta, ops, conds) do
    case Backend.write(meta, ops, when: conds) do
      {:ok, _} -> :ok
      {:error, {_, :conflict, _}} -> {:error, :stale}
      {:error, reason} -> raise Ecto.Hosi.Error, reason: reason
    end
  end
end
