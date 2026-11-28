defmodule Ecto.Hosi.Batch do
  @moduledoc false
  # `Repo.write_batch/2`: inserts, updates and deletes of several records in ONE hosigraph write,
  # all of it or none.
  #
  #   * New records are named `[:tmp, n]` inside the batch, so a child can point at a parent that
  #     does not exist yet (`refs: [post_id: :post]`).
  #   * A later update can give a new record more facts, through any view (`record: :post`). The
  #     facts of a new record are put together here and written once.
  #   * Every step on a record that exists is worked out from what it holds before the batch. So
  #     one step cannot see another's change, and two steps cannot change one record (refused).

  alias Ecto.Hosi.{Backend, Blame, Change, Delete, Model, Store}

  @step_options %{insert: [:refs], update: [:record], delete: []}

  @doc """
  `{:ok, %{name => struct}}` or `{:error, name, what}`. `static: true` (an `Ecto.Multi` compiled to
  a batch) answers `:not_static` where a batch cannot do it, instead of raising.
  """
  def run(repo, steps, opts) do
    meta = repo.get_dynamic_repo() |> Ecto.Adapter.lookup_meta() |> Backend.with_timeout(opts)
    adapter = repo.__adapter__()
    steps = steps |> Enum.with_index(1) |> Enum.map(fn {s, n} -> normalize(s, n) end)
    names = Enum.map(steps, & &1.name)

    with [twice | _] <- names -- Enum.uniq(names) do
      raise ArgumentError,
            "write_batch: two steps are named #{inspect(twice)}; each step needs its own name, " <>
              "or its result would be lost"
    end

    env = %{meta: meta, adapter: adapter, by_name: Map.new(steps, &{&1.name, &1})}

    try do
      # `new` is `%{n => record}` of the records the batch makes, by the number of the insert step
      {compiled, new} = Enum.map_reduce(steps, %{}, &compile(&1, &2, env))
      compiled = make_new(meta, compiled, new)

      case overlap(compiled) do
        nil -> write(meta, compiled)
        found -> if opts[:static], do: :not_static, else: overlap!(found)
      end
    catch
      {:batch_error, name, what} -> {:error, name, what}
    end
  end

  defp write(meta, compiled) do
    # two deletes may reach one record: what both say is said once
    ops = compiled |> Enum.flat_map(& &1.ops) |> Enum.uniq()
    conds = compiled |> Enum.flat_map(& &1.conds) |> Enum.uniq()

    if ops == [] do
      {:ok, results(compiled, %{})}
    else
      opts = if conds == [], do: [], else: [when: conds]

      case Backend.write(meta, ops, opts) do
        {:ok, report} ->
          {:ok, results(compiled, Backend.temp_ids(report))}

        {:error, {_, :conflict, msg}} ->
          {:error, Blame.conflict(meta, compiled, msg), :stale}

        {:error, {_, :"write-rejected", msg}} ->
          {:error, Blame.rejected(meta, compiled, msg), {:rejected, msg}}

        {:error, reason} ->
          raise Ecto.Hosi.Error, reason: reason
      end
    end
  end

  defp results(compiled, temps), do: Map.new(compiled, &{&1.name, &1.finish.(temps)})

  defp normalize({kind, {name, subject}}, n), do: normalize({kind, {name, subject, []}}, n)

  defp normalize({kind, {name, subject, opts}}, n) when kind in [:insert, :update, :delete] do
    case Keyword.keys(opts) -- @step_options[kind] do
      [] ->
        %{kind: kind, name: name, subject: subject, opts: opts, n: n}

      unknown ->
        raise ArgumentError,
              "write_batch: step #{inspect(name)}: a#{if kind == :delete, do: "", else: "n"} " <>
                "#{kind} step takes #{inspect(@step_options[kind])}, not #{inspect(unknown)}"
    end
  end

  defp normalize(other, _n) do
    raise ArgumentError,
          "write_batch steps look like `insert: {name, changeset}` (also :update, :delete), got: #{inspect(other)}"
  end

  # the earlier insert step that `refs:` or `record:` names
  defp insert_step!(name, step, env, option) do
    case env.by_name[name] do
      %{kind: :insert, n: n} = found when n < step.n ->
        found

      _ ->
        raise ArgumentError,
              "write_batch: #{option} #{inspect(name)} (step #{inspect(step.name)}) must name an " <>
                "insert step earlier in the batch"
    end
  end

  ## ------------------------------------------------------------ insert

  defp compile(%{kind: :insert} = step, new, env) do
    %{meta: meta, adapter: adapter} = env
    cs = changeset!(step)
    schema = cs.data.__struct__
    view = Model.view(meta.model, schema)
    struct = autogenerate(schema, Ecto.Changeset.apply_changes(cs))
    refs = refs!(step, env, view)

    values =
      for field <- schema.__schema__(:fields), field != :id do
        attr = view.attr_of[field]
        value = value(env, schema, field, attr, Map.get(struct, field))

        case refs do
          %{^field => {:one, [target]}} -> {attr, [:tmp, target]}
          %{^field => {:many, targets}} -> {attr, List.wrap(value) ++ Enum.map(targets, &[:tmp, &1])}
          _ -> {attr, value}
        end
      end

    facts = Change.base(meta, view, values)

    record = %{
      facts: facts,
      # which step each attribute comes from
      by: Map.new(facts, fn {attr, _} -> {attr, step.n} end),
      steps: %{step.n => %{name: step.name, view: view, changeset: cs}}
    }

    finish = fn temps ->
      id = fn n -> load(adapter, schema, :id, Backend.temp_id(temps, n)) end

      struct =
        Enum.reduce(refs, struct, fn {field, {card, targets}}, acc ->
          made = fn n -> load(adapter, schema, field, Backend.temp_id(temps, n), card) end

          case card do
            :one -> Map.put(acc, field, made.(hd(targets)))
            :many -> Map.update!(acc, field, &(List.wrap(&1) ++ Enum.map(targets, made)))
          end
        end)

      %{struct | id: id.(step.n)} |> Ecto.put_meta(state: :loaded)
    end

    {%{name: step.name, kind: :insert, n: step.n, ops: [], conds: [], finish: finish},
     Map.put(new, step.n, record)}
  end

  ## ------------------------------------------------------------ update

  defp compile(%{kind: :update} = step, new, env) do
    case Keyword.fetch(step.opts, :record) do
      {:ok, name} -> add_to_new(step, new, env, insert_step!(name, step, env, "record:").n)
      :error -> {change(step, env), new}
    end
  end

  ## ------------------------------------------------------------ delete

  defp compile(%{kind: :delete} = step, new, env) do
    %{meta: meta} = env
    %{__struct__: schema, id: app_id} = struct = step.subject
    view = Model.view(meta.model, schema)
    id = id!(env, schema, app_id, step)
    entity = Backend.read_one(meta, id)

    unless Model.member?(view, entity), do: throw({:batch_error, step.name, :stale})

    {ops, conds} =
      case Delete.plan(meta, view, [{id, entity}]) do
        {:ok, ops, conds} -> {ops, conds}
        {:restrict, name} -> throw({:batch_error, step.name, {:foreign_key, name}})
        {:refused, info} -> throw({:batch_error, step.name, {:refused, info}})
      end

    finish = fn _temps -> Ecto.put_meta(struct, state: :deleted) end
    {%{name: step.name, kind: :delete, n: step.n, ops: ops, conds: conds, finish: finish}, new}
  end

  # an update of a record that exists
  defp change(step, env) do
    %{meta: meta} = env
    cs = changeset!(step)
    schema = cs.data.__struct__
    view = Model.view(meta.model, schema)
    id = id!(env, schema, cs.data.id, step)
    changes = changes(cs, schema)
    values = values(env, view, schema, changes)
    entity = Backend.read_one(meta, id)

    unless Change.reachable?(view, entity), do: throw({:batch_error, step.name, :stale})

    {ops, conds} = Change.update(meta, id, entity, values)
    sets = Change.sets(meta, values, entity)
    conds = Change.existence(meta, view, id, entity) ++ conds

    case Ecto.Adapters.Hosi.__check_unique__(meta, view, sets, id, entity) do
      [] -> :ok
      [{:unique, cname} | _] -> throw({:batch_error, step.name, constraint_error(cs, cname)})
    end

    finish = fn _temps -> cs.data |> struct(changes) |> Ecto.put_meta(state: :loaded) end
    %{name: step.name, kind: :update, n: step.n, ops: ops, conds: conds, finish: finish}
  end

  # `update: {name, changeset, record: :step}`: more facts for a record the batch makes
  defp add_to_new(step, new, env, n) do
    %{meta: meta, adapter: adapter} = env
    cs = changeset!(step)
    schema = cs.data.__struct__
    view = Model.view(meta.model, schema)

    unless is_nil(cs.data.id) do
      raise ArgumentError,
            "write_batch: step #{inspect(step.name)} has record:, so it is about a record the " <>
              "batch makes, but its changeset has the id #{inspect(cs.data.id)}. Use a new struct: " <>
              "Ecto.Changeset.change(%#{inspect(schema)}{}, ..)"
    end

    changes = changes(cs, schema)
    values = values(env, view, schema, changes)
    record = Map.fetch!(new, n)

    unless Change.reachable?(view, Change.entity(meta, record.facts)) do
      raise ArgumentError,
            "write_batch: step #{inspect(step.name)}: #{inspect(view.module)} is a view of a " <>
              "record type, and the record of step n.#{n} is not of that type " <>
              "(an update cannot change what a record is; ecto_hosi's limit)"
    end

    record = %{
      record
      | facts: Change.amend(meta, record.facts, values),
        by: Enum.reduce(values, record.by, fn {attr, _}, by -> Map.put(by, attr, step.n) end),
        steps: Map.put(record.steps, step.n, %{name: step.name, view: view, changeset: cs})
    }

    finish = fn temps ->
      id = load(adapter, schema, :id, Backend.temp_id(temps, n))
      %{struct(cs.data, changes) | id: id} |> Ecto.put_meta(state: :loaded)
    end

    {%{name: step.name, kind: :record, n: step.n, ops: [], conds: [], finish: finish},
     Map.put(new, n, record)}
  end

  ## ------------------------------------------------------------ the new records

  # each new record, whole: its ops (each with the step it comes from) and its unique values
  defp make_new(meta, compiled, new) do
    ops =
      new
      |> Enum.sort()
      |> Enum.reduce(%{}, fn {n, record}, acc ->
        facts = Change.add_derived(meta, record.facts)
        by = Enum.reduce(Map.keys(facts), record.by, &Map.put_new(&2, &1, derived_from(meta, record, &1, n)))
        entity = Change.entity(meta, facts)

        for {step_n, step} <- Enum.sort(record.steps), reduce: acc do
          acc ->
            own = Map.filter(facts, fn {attr, _} -> by[attr] == step_n end)

            case Ecto.Adapters.Hosi.__check_unique__(meta, step.view, own, nil, entity) do
              [] ->
                :ok

              [{:unique, cname} | _] ->
                throw({:batch_error, step.name, constraint_error(step.changeset, cname)})
            end

            Map.put(acc, step_n, Change.create_ops(meta, [:tmp, n], own))
        end
      end)

    Enum.map(compiled, fn
      %{kind: kind, n: n} = step when kind in [:insert, :record] -> %{step | ops: Map.fetch!(ops, n)}
      step -> step
    end)
  end

  # the step that a derived value comes from: the last that gave one of the rule's attributes
  defp derived_from(meta, record, attr, n) do
    case Enum.find(meta.model.rules, &(&1.attr == attr)) do
      nil ->
        n

      rule ->
        members = rule.attrs ++ for(w <- rule.where, do: elem(w, 1))
        members |> Enum.map(&Map.get(record.by, &1)) |> Enum.reject(&is_nil/1) |> Enum.max(fn -> n end)
    end
  end

  ## ------------------------------------------------------------ one step for one record

  # `{id, step, step}` where two steps change one record and one of them is an update, or nil
  defp overlap(compiled) do
    compiled
    |> Enum.reduce_while(%{}, fn step, seen ->
      ids = for [_, id | _] <- step.ops, is_integer(id), uniq: true, do: id

      case Enum.find_value(ids, &clash(seen, &1, step)) do
        nil -> {:cont, Enum.reduce(ids, seen, &Map.put(&2, &1, step))}
        found -> {:halt, {:clash, found}}
      end
    end)
    |> case do
      {:clash, found} -> found
      _ -> nil
    end
  end

  defp clash(seen, id, step) do
    case seen do
      %{^id => other} when other.kind == :update or step.kind == :update -> {id, other.name, step.name}
      _ -> nil
    end
  end

  defp overlap!({id, one, other}) do
    raise ArgumentError,
          "write_batch: the steps #{inspect(one)} and #{inspect(other)} both change record #{id}. " <>
            "Every step is worked out from the records as they are before the batch, so one cannot " <>
            "see the other's change: put the changes in one changeset, or write them one after the " <>
            "other (ecto_hosi's limit)"
  end

  ## ------------------------------------------------------------ helpers

  defp changeset!(%{subject: %Ecto.Changeset{valid?: false} = cs, name: name}),
    do: throw({:batch_error, name, cs})

  defp changeset!(%{subject: %Ecto.Changeset{} = cs}), do: cs
  defp changeset!(%{subject: %{__struct__: _} = struct}), do: Ecto.Changeset.change(struct)

  # `%{field => {card, [n]}}` of `refs:`
  defp refs!(step, env, view) do
    Map.new(Keyword.get(step.opts, :refs, []), fn {field, names} ->
      attr = view.attr_of[field]
      spec = attr && env.meta.model.attrs[attr]

      unless spec && spec.type == :ref do
        raise ArgumentError,
              "write_batch: refs: #{inspect(field)} (step #{inspect(step.name)}) is not a " <>
                "reference field of #{inspect(view.module)} (a belongs_to key, or hosi_ref)"
      end

      targets = for name <- List.wrap(names), do: insert_step!(name, step, env, "refs:").n

      if spec.card == :one and length(targets) != 1 do
        raise ArgumentError,
              "write_batch: refs: #{inspect(field)} (step #{inspect(step.name)}) holds one record"
      end

      {field, {spec.card, targets}}
    end)
  end

  # the changes of an update, `autoupdate` included
  defp changes(cs, schema) do
    case cs.changes |> Map.take(schema.__schema__(:fields)) |> Map.delete(:id) do
      changes when changes == %{} -> changes
      changes -> add_autoupdate(schema, changes)
    end
  end

  defp values(env, view, schema, changes) do
    for {field, v} <- changes do
      attr = view.attr_of[field]
      {attr, value(env, schema, field, attr, v)}
    end
  end

  # a field's value as the adapter holds it
  defp value(env, schema, field, attr, value),
    do: Store.normalize(env.meta.model, attr, dump(env.adapter, schema, field, value))

  defp id!(env, schema, app_id, step) do
    id = dump(env.adapter, schema, :id, app_id)

    is_integer(id) ||
      raise ArgumentError,
            "write_batch: #{step.kind} step #{inspect(step.name)} needs a loaded struct with an id"

    id
  end

  defp dump(adapter, schema, field, value) do
    type = schema.__schema__(:type, field)

    case Ecto.Type.adapter_dump(adapter, type, value) do
      {:ok, dumped} ->
        dumped

      _ ->
        raise ArgumentError, "cannot dump #{inspect(value)} as #{inspect(type)} (field #{field})"
    end
  end

  # (a field of many values: one of them)
  defp load(adapter, schema, field, value, card \\ :one) do
    type =
      case {card, schema.__schema__(:type, field)} do
        {:many, {:array, inner}} -> inner
        {_, type} -> type
      end

    case Ecto.Type.adapter_load(adapter, type, value) do
      {:ok, loaded} ->
        loaded

      _ ->
        raise ArgumentError, "cannot load #{inspect(value)} as #{inspect(type)} (field #{field})"
    end
  end

  defp autogenerate(schema, struct) do
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
