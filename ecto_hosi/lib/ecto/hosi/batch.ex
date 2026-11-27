defmodule Ecto.Hosi.Batch do
  @moduledoc false
  # `Repo.write_batch/2`: inserts, updates and deletes of several rows in ONE hosigraph write,
  # all of it or none. New rows are named `[:tmp, n]` inside the batch, so a child can point at a
  # parent that does not exist yet (`refs: [post_id: :post]`).

  alias Ecto.Hosi.{Attr, Backend}

  def run(repo, steps, _opts) do
    meta = Ecto.Adapter.lookup_meta(repo.get_dynamic_repo())
    adapter = repo.__adapter__()
    steps = steps |> Enum.with_index(1) |> Enum.map(fn {s, n} -> normalize(s, n) end)
    by_name = Map.new(steps, &{&1.name, &1})

    try do
      compiled = Enum.map(steps, &compile(&1, meta, adapter, by_name))
      ops = Enum.flat_map(compiled, & &1.ops)
      conds = Enum.flat_map(compiled, & &1.conds)

      if ops == [] do
        {:ok, %{}}
      else
        opts = if conds == [], do: [], else: [when: conds]

        case Backend.write(meta, ops, opts) do
          {:ok, report} ->
            {:ok, Map.new(compiled, &{&1.name, &1.finish.(report)})}

          {:error, {_, :conflict, _}} ->
            {:error, :batch, :stale}

          {:error, {_, :"write-rejected", msg}} ->
            {:error, :batch, {:rejected, msg}}

          {:error, reason} ->
            raise Ecto.Hosi.Error, reason: reason
        end
      end
    catch
      {:batch_error, name, what} -> {:error, name, what}
    end
  end

  defp normalize({kind, {name, subject}}, n), do: normalize({kind, {name, subject, []}}, n)

  defp normalize({kind, {name, subject, opts}}, n) when kind in [:insert, :update, :delete],
    do: %{kind: kind, name: name, subject: subject, opts: opts, n: n}

  defp normalize(other, _n) do
    raise ArgumentError,
          "write_batch steps look like `insert: {name, changeset}` (also :update, :delete), got: #{inspect(other)}"
  end

  ## ------------------------------------------------------------ insert

  defp compile(%{kind: :insert} = step, meta, adapter, by_name) do
    cs = changeset!(step)
    schema = cs.data.__struct__
    source = schema.__schema__(:source)
    refs = Keyword.get(step.opts, :refs, [])
    struct = autogenerate(schema, :autogenerate, Ecto.Changeset.apply_changes(cs))
    tmp = [:tmp, step.n]

    for {_field, name} <- refs do
      case by_name[name] do
        %{kind: :insert, n: n} when n < step.n ->
          :ok

        _ ->
          raise ArgumentError,
                "refs: #{inspect(name)} must be an insert step earlier in the batch"
      end
    end

    facts =
      for field <- schema.__schema__(:fields), field != :id do
        attr = Attr.attr(source, field)

        case {Keyword.get(refs, field), Map.get(struct, field)} do
          {nil, nil} -> nil
          {nil, v} -> {attr, Backend.wrap(meta, attr, dump(adapter, schema, field, v))}
          {name, _} -> {attr, [:ref, [:tmp, by_name[name].n]]}
        end
      end
      |> Enum.reject(&is_nil/1)

    # a `tuple: true` rule needs every field: a ref to a row made earlier in this batch counts as set
    Ecto.Hosi.Derived.check_new!(meta, source, facts)

    # only values that are known now can be checked for uniqueness
    known = for {attr, v} <- facts, not is_list(v), do: {attr, v}
    derived = Ecto.Hosi.Derived.extend(meta, source, nil, known) -- known
    facts = facts ++ derived
    known = known ++ derived

    case Ecto.Adapters.Hosi.__check_unique__(meta, known, nil) do
      [] -> :ok
      [{:unique, cname} | _] -> throw({:batch_error, step.name, constraint_error(cs, cname)})
    end

    %{
      name: step.name,
      ops: [
        [:create, tmp, Attr.marker(source), true] | for({a, v} <- facts, do: [:create, tmp, a, v])
      ],
      conds: [],
      finish: fn report ->
        made = fn field, n -> load(adapter, schema, field, Backend.temp_id(report, n)) end

        struct =
          Enum.reduce(refs, struct, fn {field, name}, acc ->
            Map.put(acc, field, made.(field, by_name[name].n))
          end)

        %{struct | id: made.(:id, step.n)} |> Ecto.put_meta(state: :loaded)
      end
    }
  end

  ## ------------------------------------------------------------ update

  defp compile(%{kind: :update} = step, meta, adapter, _by_name) do
    cs = changeset!(step)
    schema = cs.data.__struct__
    source = schema.__schema__(:source)
    id = dump(adapter, schema, :id, cs.data.id)
    Ecto.Hosi.Cascade.append_only!(meta, source)

    is_integer(id) ||
      raise ArgumentError, "update step #{inspect(step.name)} needs a loaded row with an id"

    changes = cs.changes |> Map.take(schema.__schema__(:fields)) |> Map.delete(:id)
    changes = if changes == %{}, do: changes, else: add_autoupdate(schema, changes)

    {ops, conds, sets} =
      Enum.reduce(changes, {[], [[:holds, id, Attr.marker(source), true]], []}, fn
        {field, nil}, {ops, conds, sets} ->
          attr = Attr.attr(source, field)

          case Map.get(cs.data, field) do
            nil ->
              {ops, conds, sets}

            old ->
              d = dump(adapter, schema, field, old)

              {[[:delete, id, attr, Backend.wrap(meta, attr, d)] | ops],
               [[:holds, id, attr, d] | conds], sets}
          end

        {field, v}, {ops, conds, sets} ->
          attr = Attr.attr(source, field)
          d = dump(adapter, schema, field, v)
          {[[:create, id, attr, Backend.wrap(meta, attr, d)] | ops], conds, [{attr, d} | sets]}
      end)

    # the derived attributes of the unique rules this change moves
    nil_attrs = for {field, nil} <- changes, do: {Attr.attr(source, field), nil}
    entity = fn -> meta.backend.get_entities(meta.conn, [id]) |> Backend.ok!() |> hd() end
    changed = Enum.reverse(sets) ++ nil_attrs
    derived = Ecto.Hosi.Derived.extend(meta, source, id, changed, entity) -- changed

    {dops, dconds} =
      if derived == [], do: {[], []}, else: Ecto.Hosi.Update.set_ops(meta, id, entity.(), derived)

    sets = sets ++ for({a, v} <- derived, not is_nil(v), do: {a, v})
    ops = Enum.reverse(dops, ops)
    conds = conds ++ dconds

    case Ecto.Adapters.Hosi.__check_unique__(meta, sets, id) do
      [] -> :ok
      [{:unique, cname} | _] -> throw({:batch_error, step.name, constraint_error(cs, cname)})
    end

    %{
      name: step.name,
      ops: Enum.reverse(ops),
      conds: conds,
      finish: fn _report ->
        cs.data |> struct(changes) |> Ecto.put_meta(state: :loaded)
      end
    }
  end

  ## ------------------------------------------------------------ delete

  defp compile(%{kind: :delete} = step, meta, adapter, _by_name) do
    %{__struct__: schema, id: app_id} = struct = step.subject
    id = dump(adapter, schema, :id, app_id)
    source = schema.__schema__(:source)
    marker = Attr.marker(source)
    Ecto.Hosi.Cascade.append_only!(meta, source)

    entity =
      case Backend.get_entities(meta, [id]) |> Backend.ok!() do
        [e] -> e
        _ -> %{}
      end

    unless Map.has_key?(entity, marker), do: throw({:batch_error, step.name, :stale})

    {ops, conds} =
      case Ecto.Hosi.Cascade.plan(meta, source, [{id, entity}]) do
        {:ok, ops, conds} -> {ops, conds}
        {:restrict, name} -> throw({:batch_error, step.name, {:foreign_key, name}})
      end

    %{
      name: step.name,
      ops: ops,
      conds: conds,
      finish: fn _report -> Ecto.put_meta(struct, state: :deleted) end
    }
  end

  ## ------------------------------------------------------------ helpers

  defp changeset!(%{subject: %Ecto.Changeset{valid?: false} = cs, name: name}),
    do: throw({:batch_error, name, cs})

  defp changeset!(%{subject: %Ecto.Changeset{} = cs}), do: cs
  defp changeset!(%{subject: %{__struct__: _} = struct}), do: Ecto.Changeset.change(struct)

  defp dump(adapter, schema, field, value) do
    type = schema.__schema__(:type, field)

    case Ecto.Type.adapter_dump(adapter, type, value) do
      {:ok, dumped} ->
        dumped

      _ ->
        raise ArgumentError, "cannot dump #{inspect(value)} as #{inspect(type)} (field #{field})"
    end
  end

  defp load(adapter, schema, field, value) do
    type = schema.__schema__(:type, field)

    case Ecto.Type.adapter_load(adapter, type, value) do
      {:ok, loaded} ->
        loaded

      _ ->
        raise ArgumentError, "cannot load #{inspect(value)} as #{inspect(type)} (field #{field})"
    end
  end

  defp autogenerate(schema, :autogenerate, struct) do
    Enum.reduce(schema.__schema__(:autogenerate), struct, fn {fields, {m, f, a}}, acc ->
      Enum.reduce(fields, acc, fn field, acc ->
        if Map.get(acc, field) == nil, do: Map.put(acc, field, apply(m, f, a)), else: acc
      end)
    end)
  end

  defp add_autoupdate(schema, changes) do
    Enum.reduce(schema.__schema__(:autoupdate), changes, fn {fields, {m, f, a}}, acc ->
      Enum.reduce(fields, acc, &Map.put_new(&2, &1, apply(m, f, a)))
    end)
  end

  # the changeset, with the error `unique_constraint/3` declared for this constraint name
  defp constraint_error(cs, cname) do
    case Enum.find(cs.constraints, &(&1.type == :unique and &1.constraint == cname)) do
      nil ->
        raise Ecto.Hosi.Error,
          message:
            "unique constraint #{cname} was violated, but the changeset does not declare it " <>
              "(add unique_constraint/3)"

      c ->
        Ecto.Changeset.add_error(cs, c.field, c.error_message,
          constraint: :unique,
          constraint_name: cname
        )
    end
  end
end
