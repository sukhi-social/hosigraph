defmodule Ecto.Hosi.StaticMulti do
  @moduledoc """
  `Repo.transaction(multi)` as ONE atomic hosigraph write, when the `Ecto.Multi` can be.

  A multi can be when every step is a changeset that is already known: `Multi.insert/delete/update`
  with a changeset or struct, no function. Those are compiled into `Repo.write_batch/2`, which
  hosigraph applies all or nothing, even if the node crashes. A multi with anything else (a
  `Multi.run`, a step that takes a function, `update_all`, options such as `on_conflict`) runs as
  before, step by step, with the undo-on-rollback of `Ecto.Hosi.Tx`.

  Put it in the repo after `use Ecto.Repo`:

      use Ecto.Hosi.StaticMulti

  (a repo that uses `Ecto.Adapters.Hosi` directly has it already). On a repo whose store is not
  hosigraph it passes everything to Ecto untouched.

  What differs from a step-by-step multi:

    * all or nothing means the changes of the steps before a failing one were never made, so the
      `changes` of `{:error, name, value, changes}` is `%{}`
    * a stale update or delete raises `Ecto.Hosi.Error` instead of `Ecto.StaleEntryError`
    * changesets with associations, `prepare_changes`, an optimistic lock or a prefix are not
      compiled: they run step by step
  """

  defmacro __using__(_opts) do
    quote do
      defoverridable transaction: 2

      def transaction(%Ecto.Multi{} = multi, opts) do
        case Ecto.Hosi.StaticMulti.run(__MODULE__, multi, opts) do
          :not_static -> super(multi, opts)
          result -> result
        end
      end

      def transaction(fun, opts), do: super(fun, opts)
    end
  end

  @doc "`:not_static`, or the result `Repo.transaction/2` returns for the multi."
  def run(repo, %Ecto.Multi{} = multi, opts \\ []) do
    meta = Ecto.Adapter.lookup_meta(repo.get_dynamic_repo())

    with true <- hosi?(meta),
         false <- Ecto.Hosi.Tx.active?(meta),
         {:ok, steps} <- plan(multi) do
      case Ecto.Hosi.Batch.run(repo, steps, opts) do
        {:ok, values} ->
          {:ok, values}

        {:error, name, %Ecto.Changeset{} = cs} ->
          {:error, name, cs, %{}}

        {:error, name, :stale} ->
          raise Ecto.Hosi.Error,
            message: "the row of step #{inspect(name)} is stale (changed or deleted meanwhile)"

        {:error, name, {:rejected, msg}} ->
          raise Ecto.Hosi.Error,
            message: "hosigraph refused step #{inspect(name)}: #{msg}"

        {:error, name, {:foreign_key, key}} ->
          raise Ecto.Hosi.Error,
            message: "step #{inspect(name)} violates the foreign key #{key}: rows still point at it"

        {:error, name, {:other_facts, found}} ->
          raise Ecto.Hosi.Error,
            reason: {:other_facts, found},
            message: "step #{inspect(name)}: " <> Ecto.Hosi.Cascade.other_facts_message(found)
      end
    else
      _ -> :not_static
    end
  end

  defp hosi?(meta), do: match?(%{backend: _, conn: _, attrs: _}, meta)

  @doc false
  # `{:ok, write_batch steps}` or `:error`.
  def plan(%Ecto.Multi{} = multi) do
    steps = Ecto.Multi.to_list(multi)
    names = Enum.map(steps, &elem(&1, 0))

    planned = Enum.map(steps, fn {name, op} -> step(name, op) end)

    cond do
      steps == [] -> :error
      Enum.uniq(names) != names -> :error
      Enum.any?(planned, &(&1 == :error)) -> :error
      true -> {:ok, planned}
    end
  end

  defp step(name, {op, %Ecto.Changeset{} = cs, []}) when op in [:insert, :update, :delete] do
    cond do
      not plain?(cs) -> :error
      op == :delete -> {:delete, {name, cs.data}}
      op == :insert -> {:insert, {name, cs}}
      op == :update -> {:update, {name, cs}}
    end
  end

  defp step(_name, _op), do: :error

  # a changeset write_batch applies exactly: only plain fields, nothing to run before the write
  defp plain?(%Ecto.Changeset{data: %{__struct__: schema}} = cs) when is_atom(schema) do
    function_exported?(schema, :__schema__, 1) and
      cs.prepare == [] and cs.filters == %{} and cs.repo_opts == [] and
      Enum.all?(Map.keys(cs.changes), &(&1 in schema.__schema__(:fields))) and
      cs.data.__meta__.prefix == nil
  end

  defp plain?(_), do: false
end
