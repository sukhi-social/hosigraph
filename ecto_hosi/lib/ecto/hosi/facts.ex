defmodule Ecto.Hosi.Facts do
  @moduledoc """
  Facts written directly, with the rules of the attributes: `Repo.write_facts/2`.

      MyApp.Repo.write_facts([
        [:create, note_id, :"ap/alt-id", "https://old.example/notes/1"],
        [:create, [:tmp, 1], :"hosi/owner", account_id],
        [:create, [:tmp, 1], :content, "hello"]
      ])
      #=> {:ok, %{1 => 1005}}

  Each op is `[:create | :delete, id or [:tmp, n], attribute, value]`, as hosigraph's own
  `write`, and all of them are one atomic write. The value is the Elixir value of the type that
  the views and vocabularies give the attribute (a `DateTime`, a string for a keyword or a UUID);
  a reference takes an id or `[:tmp, n]`. For an attribute with many values each op is one value;
  for one with a single value a `:create` replaces it. An attribute that no view declares goes to
  hosigraph as it is, after the check for a misspelt name (`check: false` skips it).

  **The rules of the attributes hold**: unique values, `final`, record types (not changed once
  set), keyword values, tuple rules and the rules the adapter derives (`hosi/uq-…`). Of the
  `hosi/` attributes only `hosi/owner` can be given, and only to a new record.

  **Not applied**: what belongs to views and to deletes. Nothing is added by `hosi_where` or
  `hosi_owner`, and removing facts follows no `on_delete` and leaves no tombstone.

  Answers `{:ok, %{n => id}}` (the ids of the `[:tmp, n]`), or

    * `{:error, {:stale, id}}` – there is no record `id`
    * `{:error, :stale}` – a record changed meanwhile; read again
    * `{:error, {:unique, attribute_or_rule_name}}`
    * `{:error, {:rejected, message}}` – hosigraph refused it

  A value of the wrong type raises `ArgumentError`; changing a `final` attribute or a record type
  raises `Ecto.Hosi.Error`, as `Repo.update` does. `timeout:` is how long the write is waited for.
  """

  alias Ecto.Hosi.{Backend, Change, Check, Model, RepoApi, Store}

  @doc false
  def write(repo, ops, opts \\ []) do
    meta = repo |> RepoApi.meta() |> Backend.with_timeout(opts)
    ops = Enum.map(ops, &op!(meta, &1, opts))
    # the targets, in the order they first appear
    targets = ops |> Enum.map(&elem(&1, 1)) |> Enum.uniq()
    by_target = Enum.group_by(ops, &elem(&1, 1))
    ids = Enum.filter(targets, &is_integer/1)
    entities = Map.new(Backend.read(meta, ids))

    case Enum.find(ids, &(entities[&1] == %{})) do
      nil ->
        targets
        |> Enum.reduce_while({:ok, []}, fn target, {:ok, acc} ->
          case plan(meta, target, entities[target], by_target[target]) do
            {:ok, part} -> {:cont, {:ok, [part | acc]}}
            {:error, _} = error -> {:halt, error}
          end
        end)
        |> case do
          {:ok, parts} -> send_parts(meta, Enum.reverse(parts))
          error -> error
        end

      id ->
        {:error, {:stale, id}}
    end
  end

  defp send_parts(meta, parts) do
    ops = Enum.flat_map(parts, &elem(&1, 0))
    conds = Enum.flat_map(parts, &elem(&1, 1))

    if ops == [] do
      {:ok, %{}}
    else
      opts = if conds == [], do: [], else: [when: conds]

      case Backend.write(meta, ops, opts) do
        {:ok, report} -> {:ok, Backend.temp_ids(report)}
        {:error, {_, :conflict, _}} -> {:error, :stale}
        {:error, {_, :"write-rejected", msg}} -> {:error, {:rejected, msg}}
        {:error, reason} -> raise Ecto.Hosi.Error, reason: reason
      end
    end
  end

  # `{:ok, {ops, conds}}` for one record
  defp plan(meta, [:tmp, _] = target, _no_entity, ops) do
    if Enum.any?(ops, &match?({:delete, _, _, _}, &1)) do
      raise ArgumentError, "Repo.write_facts: #{inspect(target)} is new: there is nothing to delete"
    end

    facts =
      for {attr, values} <- touched(meta, %{}, ops), into: %{} do
        {attr, shape(meta, attr, values)}
      end

    facts = Change.add_derived(meta, facts)

    with :ok <- free(meta, facts, nil, Change.entity(meta, facts)) do
      {:ok, {Change.create_ops(meta, target, facts), []}}
    end
  end

  defp plan(meta, id, entity, ops) do
    changes =
      for {attr, values} <- touched(meta, entity, ops),
          Enum.sort(values) != Enum.sort(Map.get(entity, attr, [])),
          do: {attr, shape(meta, attr, values)}

    if changes == [] do
      {:ok, {[], []}}
    else
      {write, conds} = Change.update(meta, id, entity, changes)

      with :ok <- free(meta, Change.sets(meta, changes, entity), id, entity) do
        {:ok, {write, Change.existence(meta, nil, id, entity) ++ conds}}
      end
    end
  end

  # `%{attribute => values}` of the attributes the ops name, once they are made
  defp touched(meta, entity, ops) do
    Enum.reduce(ops, %{}, fn {kind, _, attr, value}, acc ->
      current = Map.get_lazy(acc, attr, fn -> Map.get(entity, attr, []) end)

      new =
        case {kind, meta.model.attrs[attr]} do
          {:create, %{card: :many}} -> Enum.uniq(current ++ [value])
          {:create, _} -> [value]
          {:delete, _} -> List.delete(current, value)
        end

      Map.put(acc, attr, new)
    end)
  end

  defp shape(meta, attr, values) do
    case {meta.model.attrs[attr], values} do
      {%{card: :many}, values} -> values
      {_, []} -> nil
      {_, [value]} -> value
    end
  end

  defp free(meta, values, self_id, entity) do
    case Ecto.Adapters.Hosi.__taken__(meta, values, self_id, entity) do
      [] ->
        :ok

      [attr | _] ->
        rule = Enum.find(meta.model.rules, &(&1.attr == attr))
        {:error, {:unique, if(rule, do: rule.name, else: attr)}}
    end
  end

  # `{kind, target, attribute, value as the adapter holds it}`
  defp op!(meta, [kind, target, attr, value], opts)
       when kind in [:create, :delete] and is_atom(attr) do
    unless is_integer(target) or match?([:tmp, n] when is_integer(n), target) do
      raise ArgumentError,
            "Repo.write_facts: an op is about an id or [:tmp, n], got #{inspect(target)}"
    end

    attribute!(meta, kind, target, attr, opts)
    {kind, target, attr, value!(meta.model.attrs[attr], attr, value)}
  end

  defp op!(_meta, other, _opts) do
    raise ArgumentError,
          "Repo.write_facts: an op is [:create | :delete, id or [:tmp, n], attribute, value], " <>
            "got #{inspect(other)}"
  end

  defp attribute!(meta, kind, target, attr, opts) do
    name = Atom.to_string(attr)

    cond do
      attr == Model.owner() ->
        unless kind == :create and match?([:tmp, _], target) do
          raise ArgumentError,
                "Repo.write_facts: hosi/owner is the maker of a record: it is given when the " <>
                  "record is made, and does not change (ecto_hosi's limit)"
        end

      String.starts_with?(name, ["hosi/", "db/", "db."]) ->
        raise ArgumentError,
              "Repo.write_facts: :#{name} is a name of the adapter's or Datalevin's own " <>
                "(ecto_hosi's limit)"

      not Map.has_key?(meta.model.attrs, attr) ->
        if Keyword.get(opts, :check, true), do: Check.known!(meta, attr, "Repo.write_facts")

      true ->
        :ok
    end
  end

  defp value!(_spec, attr, nil) do
    raise ArgumentError, "Repo.write_facts: nil is not a fact (#{Model.kw(attr)}): delete the value"
  end

  defp value!(nil, _attr, value), do: value
  defp value!(%{type: :ref}, _attr, [:tmp, n] = tmp) when is_integer(n), do: tmp
  defp value!(%{type: :ref} = spec, attr, [:ref, to]), do: value!(spec, attr, to)

  defp value!(spec, attr, value) do
    with {:ok, cast} <- Ecto.Type.cast(spec.ecto, value),
         {:ok, dumped} <- Ecto.Type.adapter_dump(Ecto.Adapters.Hosi, spec.ecto, cast) do
      # a list that is kept as one value becomes its text
      if spec.card == :one,
        do: Store.normalize(%{attrs: %{attr => spec}}, attr, dumped),
        else: dumped
    else
      _ ->
        raise ArgumentError,
              "Repo.write_facts: #{inspect(value)} is not a value of #{Model.kw(attr)} " <>
                "(#{inspect(spec.ecto)})"
    end
  end
end
