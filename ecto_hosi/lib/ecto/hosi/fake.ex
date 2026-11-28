defmodule Ecto.Hosi.Fake do
  @moduledoc """
  An in-memory stand-in for `Hosi`, with the same functions and the same answers as documented in
  hosigraph-ex (`{:ok, %{temps: [[n, id]]}}`, `{:error, {:app, :conflict, _}}`, `[:ref, id]` values,
  `when:` conditions, `[:rules, text]` query inputs). It exists so the adapter can be tested
  without libhosi or Datalevin.

  It refuses what hosigraph refuses of the calls the adapter and a test make: an empty write, a
  reserved or erased id, a write of a tuple, a value of the wrong type for `:long` and `:uuid`,
  a taken unique value, and the same wrong `highest`, `highest_in` and `erase` calls. For the other
  types it is **stricter**: hosigraph takes a number for a `:string` attribute, say, and this does
  not. It is not a model of every rule: where this file and hosigraph differ, hosigraph is right.
  """
  use GenServer

  def start_link(opts \\ []) do
    {open, gen_opts} = Keyword.pop(opts, :open, %{})
    GenServer.start_link(__MODULE__, open, gen_opts)
  end

  def stop(conn), do: GenServer.stop(conn)

  def write(conn, ops, opts \\ []),
    do: GenServer.call(conn, {:write, ops, Keyword.get(opts, :when, [])}, wait(opts))

  def get_entities(conn, ids), do: GenServer.call(conn, {:get_entities, ids})
  def query_many(conn, queries), do: GenServer.call(conn, {:query_many, queries})

  def highest_in(conn, attr, keys, limit, opts \\ []),
    do: GenServer.call(conn, {:highest_in, attr, keys, limit, opts})

  def highest(conn, attr, limit, opts \\ []),
    do: GenServer.call(conn, {:highest, attr, limit, opts})

  def history(conn, id), do: GenServer.call(conn, {:history, id})
  def erase(conn, erasures, opts \\ []),
    do: GenServer.call(conn, {:erase, erasures}, wait(opts))

  @doc "Nothing is on a disk here: the report of a hosigraph with nothing left to clean."
  def clean(conn, opts \\ []), do: GenServer.call(conn, :clean, wait(opts))

  defp wait(opts), do: Keyword.get(opts, :timeout, 5_000)

  @doc "The opening arguments and all facts, for assertions in tests."
  def dump(conn), do: GenServer.call(conn, :dump)

  @impl true
  def init(open),
    do:
      {:ok,
       %{
         open: open,
         schema: Map.get(open, :schema, %{}),
         facts: %{},
         events: [],
         next_id: 1000,
         erased: MapSet.new()
       }}

  @impl true
  def handle_call({:write, ops, conds}, _from, st) do
    with :ok <- check_ids(conds),
         :ok <- check_conditions(conds, st.facts),
         :ok <- check_batch(ops, st),
         {:ok, acc} <- apply_ops(ops, st),
         :ok <- check_unique(acc.facts, st.schema) do
      temps = for {n, id} <- Enum.sort(acc.tmp), do: [n, id]

      st = %{
        st
        | facts: acc.facts,
          events: st.events ++ Enum.reverse(acc.events),
          next_id: acc.next_id
      }

      {:reply, {:ok, %{temps: temps}}, st}
    else
      {:error, _} = e -> {:reply, e, st}
    end
  end

  def handle_call({:get_entities, ids}, _from, st),
    do: {:reply, {:ok, Enum.map(ids, &entity(st.facts, &1))}, st}

  def handle_call({:query_many, queries}, _from, st) do
    facts = (for {{id, a}, vals} <- st.facts, v <- vals, do: {id, a, v}) ++ tuple_facts(st)
    results = Enum.map(queries, &Ecto.Hosi.Fake.Datalog.run(&1, facts))

    case Enum.find(results, &match?({:error, _}, &1)) do
      nil -> {:reply, {:ok, Enum.map(results, fn {:ok, rows} -> rows end)}, st}
      {:error, msg} -> {:reply, {:error, {:app, :"bad-argument", msg}}, st}
    end
  end

  def handle_call({:highest, attr, limit, opts}, _from, st) do
    case check_highest(st.schema, attr, limit, opts[:after]) do
      :ok ->
        places = for {{id, ^attr}, vals} <- st.facts, v <- vals, do: %{val: v, id: id}
        {:reply, {:ok, page(places, limit, opts[:after])}, st}

      error ->
        {:reply, error, st}
    end
  end

  def handle_call({:highest_in, attr, keys, limit, opts}, _from, st) do
    case check_highest_in(st.schema, attr, keys, limit, opts[:after]) do
      {:ok, ka, oa} ->
        places =
          for {{id, ^ka}, [k | _]} <- st.facts,
              k in keys,
              v <- Enum.take(Map.get(st.facts, {id, oa}, []), 1),
              do: %{key: k, val: v, id: id}

        {:reply, {:ok, page(places, limit, opts[:after])}, st}

      error ->
        {:reply, error, st}
    end
  end

  def handle_call({:history, id}, _from, st),
    do: {:reply, {:ok, Enum.filter(st.events, &(&1.id == id))}, st}

  # `[:ids, ids]` and `[:"owned-by", attributes, owners]` (whatever ever had one of the owners as a
  # value of one of the attributes); the events of what is erased go with it
  def handle_call({:erase, erasures}, _from, st) do
    named = for [:ids, ids] <- erasures, id <- ids, do: id

    owned =
      for [:"owned-by", attrs, owners] <- erasures,
          %{kind: :create, attr: attr, val: owner, id: id} <- st.events,
          attr in attrs,
          owner in owners,
          uniq: true,
          do: id

    case Enum.find(named, &(&1 <= 1)) do
      nil ->
        ids = MapSet.new(named ++ owned)
        facts = Map.reject(st.facts, fn {{id, _}, _} -> id in ids end)
        events = Enum.reject(st.events, &(&1.id in ids))
        report = %{erased: MapSet.size(ids), owned: Enum.sort(owned)}
        erased = MapSet.union(st.erased, ids)
        {:reply, {:ok, report}, %{st | facts: facts, events: events, erased: erased}}

      id ->
        {:reply, bad("erase: #{id} is not an entity id"), st}
    end
  end

  def handle_call(:clean, _from, st) do
    report = %{rewritten: [], skipped: [], "records-left": 0, "khana-cleaned": false, "khana-rebuilt": false}
    {:reply, {:ok, report}, st}
  end

  def handle_call(:dump, _from, st), do: {:reply, %{open: st.open, facts: st.facts}, st}

  ## ---- what is refused before anything is looked at

  # (ids up to 1 are hosigraph's own)
  defp check_ids(conds) do
    case Enum.find(conds, fn [_, id | _] -> is_integer(id) and id <= 1 end) do
      nil -> :ok
      [_, id | _] -> bad("writeIf: #{id} is not an entity id")
    end
  end

  defp check_batch([], _st), do: bad("write: no operations")

  defp check_batch(ops, st) do
    Enum.find_value(ops, :ok, fn
      [_, ref, attr, _] ->
        cond do
          match?(%{tuple: _}, st.schema[attr]) ->
            bad("write: #{attr}: a tuple is made by the khana from its parts, and is not written")

          is_integer(ref) and ref <= 1 ->
            bad("write: #{ref} is not a valid entity id")

          ref in st.erased ->
            bad("write: #{ref} was erased")

          true ->
            nil
        end

      _ ->
        nil
    end)
  end

  defp check_highest(schema, attr, limit, cursor) do
    spec = schema[attr]

    cond do
      limit < 1 -> bad("highest: the count is #{limit}, and has to be 1 or more")
      spec == nil -> bad("highest: #{attr} is not in the schema")
      Map.has_key?(spec, :tuple) -> bad("highest: #{attr} is a tuple")
      Map.get(spec, :type) in [nil, :any] -> bad("highest: #{attr} has no type")
      cursor && not fits?(spec.type, cursor.val) -> bad("highest: the cursor's value does not fit #{attr}")
      true -> :ok
    end
  end

  defp check_highest_in(schema, attr, keys, limit, cursor) do
    with %{tuple: [ka, oa]} <- schema[attr],
         %{type: key_type} <- schema[ka],
         %{type: :long} <- schema[oa] do
      cond do
        limit < 1 -> bad("highestIn: the count is #{limit}, and has to be 1 or more")
        not Enum.all?(keys, &fits?(key_type, &1)) -> bad("highestIn: a key does not fit #{ka}")
        cursor && not is_integer(cursor.val) -> bad("highestIn: the cursor's value is not a long")
        true -> {:ok, ka, oa}
      end
    else
      _ -> bad("highestIn: #{attr} is not a tuple of two typed attributes whose order is a long")
    end
  end

  defp fits?(:string, v), do: is_binary(v)
  defp fits?(type, v) when type in [:long, :ref], do: is_integer(v)
  defp fits?(:double, v), do: is_float(v)
  defp fits?(:boolean, v), do: is_boolean(v)
  defp fits?(:keyword, v), do: is_atom(v) and not is_nil(v) and not is_boolean(v)
  defp fits?(:uuid, v), do: match?({:uuid, _}, v)
  defp fits?(_, _), do: false

  # the largest value first and, among equal values, the largest id
  defp page(places, limit, cursor) do
    places = Enum.sort_by(places, &{&1.val, &1.id}, :desc)

    places =
      case cursor do
        nil -> places
        %{val: v, id: i} -> Enum.filter(places, &({&1.val, &1.id} < {v, i}))
      end

    Enum.take(places, limit)
  end

  ## ---- conditions

  defp check_conditions(conds, facts) do
    failed =
      Enum.reject(conds, fn
        [:holds, id, attr, v] -> v in vals(facts, id, attr)
        [:lacks, id, attr, v] -> v not in vals(facts, id, attr)
        [:unset, id, attr] -> vals(facts, id, attr) == []
      end)

    if failed == [],
      do: :ok,
      # the failed conditions as EDN, as hosigraph says them
      else: {:error, {:app, :conflict, Hosi.Edn.encode(failed)}}
  end

  ## ---- ops (all or nothing: a copy is changed and kept only if every op is good)

  defp apply_ops(ops, st) do
    seq = st.events |> List.last(%{seq: 0}) |> Map.fetch!(:seq)
    acc = %{facts: st.facts, events: [], tmp: %{}, next_id: st.next_id, seq: seq}

    Enum.reduce_while(ops, {:ok, acc}, fn op, {:ok, acc} ->
      case apply_op(op, acc, st.schema) do
        {:ok, acc} -> {:cont, {:ok, acc}}
        {:error, _} = e -> {:halt, e}
      end
    end)
  end

  defp apply_op([kind, ref, attr, val], acc, schema) when kind in [:create, :delete] do
    {id, acc} = resolve(acc, ref)
    {val, acc} = resolve_ref_value(acc, val)

    with {:ok, value} <- check_value(Map.get(schema, attr, %{}), attr, val) do
      key = {id, attr}
      current = Map.get(acc.facts, key, [])
      card = get_in(schema, [attr, :card]) || :one

      new =
        case {kind, card} do
          {:create, :one} -> [value]
          {:create, :many} -> Enum.uniq(current ++ [value])
          {:delete, _} -> List.delete(current, value)
        end

      facts = if new == [], do: Map.delete(acc.facts, key), else: Map.put(acc.facts, key, new)
      seq = acc.seq + 1
      event = %{seq: seq, kind: kind, id: id, attr: attr, val: value}
      {:ok, %{acc | facts: facts, events: [event | acc.events], seq: seq}}
    end
  end

  defp apply_op(other, _acc, _schema), do: bad("not an op: " <> inspect(other))

  defp resolve(acc, [:tmp, n]) do
    case acc.tmp do
      %{^n => id} -> {id, acc}
      _ -> {acc.next_id, %{acc | tmp: Map.put(acc.tmp, n, acc.next_id), next_id: acc.next_id + 1}}
    end
  end

  defp resolve(acc, id) when is_integer(id), do: {id, acc}

  defp resolve_ref_value(acc, [:ref, [:tmp, _] = t]) do
    {id, acc} = resolve(acc, t)
    {[:ref, id], acc}
  end

  defp resolve_ref_value(acc, val), do: {val, acc}

  # `[:ref, id]` is how an op names another entity; only `:ref` attributes take it, and they take nothing else.
  defp check_value(%{type: :ref}, _attr, [:ref, id]) when is_integer(id), do: {:ok, id}

  defp check_value(%{type: :ref}, attr, v),
    do: rejected("#{attr} is a ref, write [:ref, id] (got #{inspect(v)})")

  defp check_value(_spec, attr, [:ref, _]), do: rejected("#{attr} is not a ref attribute")
  defp check_value(%{type: :long}, _a, v) when is_integer(v), do: {:ok, v}
  defp check_value(%{type: :string}, _a, v) when is_binary(v), do: {:ok, v}
  defp check_value(%{type: :boolean}, _a, v) when is_boolean(v), do: {:ok, v}
  defp check_value(%{type: :double}, _a, v) when is_float(v), do: {:ok, v}
  defp check_value(%{type: :keyword}, _a, v) when is_atom(v) and not is_nil(v) and not is_boolean(v), do: {:ok, v}

  # kept in lower case, as hosigraph answers it
  defp check_value(%{type: :uuid}, a, {:uuid, text} = v) when is_binary(text) do
    if text =~ ~r/\A[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}\z/,
      do: {:ok, {:uuid, String.downcase(text)}},
      else: rejected("#{a} is uuid, got #{inspect(v)}")
  end

  defp check_value(_spec, a, {:uuid, _} = v),
    do: rejected("#{a} is not a uuid attribute (got #{inspect(v)})")
  defp check_value(%{type: t}, a, v), do: rejected("#{a} is #{t}, got #{inspect(v)}")
  defp check_value(_no_type, _a, v), do: {:ok, v}

  defp bad(msg), do: {:error, {:app, :"bad-argument", msg}}
  defp rejected(msg), do: {:error, {:app, :"write-rejected", msg}}

  ## ---- unique: a value one entity holds may not be held by another

  # What the khana keeps for a tuple attribute: `[a b]` for each entity that has one of the parts
  # (`nil` for a part it lacks, which is a value there, not SQL's NULL).
  defp tuples(facts, schema, attr) do
    %{tuple: parts} = Map.fetch!(schema, attr)

    ids = for {{id, a}, vals} <- facts, a in parts, vals != [], uniq: true, do: id
    for id <- Enum.sort(ids), do: {id, attr, Enum.map(parts, &(facts |> vals(id, &1) |> List.first()))}
  end

  defp tuple_facts(st) do
    for {attr, %{tuple: _}} <- st.schema, fact <- tuples(st.facts, st.schema, attr), do: fact
  end

  defp check_unique(facts, schema) do
    with :ok <- check_unique_values(facts, schema), do: check_unique_tuples(facts, schema)
  end

  defp check_unique_tuples(facts, schema) do
    Enum.find_value(for({attr, %{tuple: _, unique: _}} <- schema, do: attr), :ok, fn attr ->
      facts
      |> tuples(schema, attr)
      |> Enum.group_by(&elem(&1, 2), &elem(&1, 0))
      |> Enum.find_value(fn
        {v, [first, second | _]} -> {:error, unique_error(attr, v, first, second)}
        _ -> nil
      end)
    end)
  end

  defp unique_error(attr, v, first, second) do
    # the words of Datalevin 1.2.10, as hosigraph passes them on
    {:app, :"write-rejected",
     "dtlv: Cannot add #datalevin/Datom [#{second} :#{attr} #{Hosi.Edn.encode(v)}] " <>
       "because of unique constraint: #{first}"}
  end

  defp check_unique_values(facts, schema) do
    uniques = for {attr, %{unique: _} = spec} <- schema, not Map.has_key?(spec, :tuple), do: attr

    dup =
      Enum.find_value(uniques, fn attr ->
        facts
        |> Enum.flat_map(fn {{id, a}, vals} ->
          if a == attr, do: Enum.map(vals, &{&1, id}), else: []
        end)
        |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
        |> Enum.find_value(fn {v, ids} ->
          case Enum.sort(Enum.uniq(ids)) do
            [first, second | _] -> {attr, v, first, second}
            _ -> nil
          end
        end)
      end)

    case dup do
      nil ->
        :ok

      {attr, v, first, second} ->
        {:error, unique_error(attr, v, first, second)}
    end
  end

  defp vals(facts, id, attr), do: Map.get(facts, {id, attr}, [])

  defp entity(facts, id) do
    for {{^id, a}, vals} <- facts, vals != [], into: %{}, do: {a, vals}
  end
end

defmodule Ecto.Hosi.Fake.Datalog do
  @moduledoc false
  # A small Datalog evaluator over `{e, attr, v}` facts: patterns, predicates, `not-join`, `or-join`,
  # rule calls (recursive ones to a fixed point), and the inputs scalar / coll / rules.

  @special ["not-join", "or-join", "or", "and"]

  def run(%{q: text} = req, facts) do
    inputs = Map.get(req, :inputs, [])

    with {:ok, form} <- Hosi.Edn.decode(text) do
      sec = sections(form)
      {bindings, rules_text} = bind_inputs(Map.get(sec, :in, []), inputs)
      rules = parse_rules(rules_text)
      names = MapSet.new(rules, &elem(&1, 0))
      rels = fixpoint(rules, %{facts: facts, rels: %{}, names: names})
      ctx = %{facts: facts, rels: rels, names: names}

      find = for {:symbol, v} <- Map.get(sec, :find, []), do: v

      rows =
        Map.get(sec, :where, [])
        |> eval_clauses(bindings, ctx)
        |> Enum.map(fn b -> Enum.map(find, &Map.fetch!(b, &1)) end)
        |> Enum.uniq()
        |> Enum.sort()

      {:ok, rows}
    end
  rescue
    e -> {:error, Exception.message(e)}
  end

  defp sections(form) do
    form
    |> Enum.reduce({nil, %{}}, fn
      k, {_, acc} when k in [:find, :in, :where] -> {k, Map.put(acc, k, [])}
      x, {k, acc} -> {k, Map.update!(acc, k, &(&1 ++ [x]))}
    end)
    |> elem(1)
  end

  defp bind_inputs(ins, inputs) do
    specs = Enum.reject(ins, &(&1 == {:symbol, "$"}))

    if length(specs) != length(inputs),
      do:
        raise(
          "the :in has #{length(specs)} inputs but #{length(inputs)} were given: #{inspect(ins)}"
        )

    specs
    |> Enum.zip(inputs)
    |> Enum.reduce({[%{}], nil}, fn
      {{:symbol, "%"}, [:rules, text]}, {bs, _} when is_binary(text) ->
        {bs, text}

      {{:symbol, "?" <> _ = name}, [:scalar, v]}, {bs, r} ->
        {Enum.map(bs, &Map.put(&1, name, v)), r}

      {[{:symbol, name}, {:symbol, "..."}], [:coll, vs]}, {bs, r} when is_list(vs) ->
        {for(b <- bs, v <- vs, do: Map.put(b, name, v)), r}

      {spec, input}, _ ->
        raise "cannot bind #{inspect(spec)} to #{inspect(input)}"
    end)
  end

  defp parse_rules(nil), do: []

  defp parse_rules(text) do
    {:ok, rules} = Hosi.Edn.decode(text)

    for [[{:symbol, name} | args] | body] <- rules do
      {name, for({:symbol, a} <- args, do: a), body}
    end
  end

  defp fixpoint(rules, ctx) do
    rels =
      Enum.reduce(rules, ctx.rels, fn {name, args, body}, acc ->
        tuples =
          for b <- eval_clauses(body, [%{}], %{ctx | rels: acc}),
              do: Enum.map(args, &Map.fetch!(b, &1))

        Map.update(acc, name, MapSet.new(tuples), &MapSet.union(&1, MapSet.new(tuples)))
      end)

    if rels == ctx.rels, do: rels, else: fixpoint(rules, %{ctx | rels: rels})
  end

  defp eval_clauses(clauses, bindings, ctx) do
    Enum.reduce(clauses, bindings, fn clause, bs ->
      Enum.flat_map(bs, &eval_clause(clause, &1, ctx))
    end)
  end

  # [(op a b)]
  defp eval_clause([[{:symbol, op} | args]], b, _ctx) do
    [x, y] = Enum.map(args, &value(&1, b))
    if predicate(op, x, y), do: [b], else: []
  end

  defp eval_clause([{:symbol, "not-join"}, vars, clauses | more], b, ctx) do
    inner = Map.take(b, names(vars))
    if eval_clauses([clauses | more], [inner], ctx) == [], do: [b], else: []
  end

  defp eval_clause([{:symbol, "or-join"}, vars | branches], b, ctx) do
    join = names(vars)

    for branch <- branches,
        r <- eval_clauses(branch_clauses(branch), [Map.take(b, join)], ctx),
        uniq: true,
        do: Map.merge(b, Map.take(r, join))
  end

  # `(or ..)`: every branch binds the same variables
  defp eval_clause([{:symbol, "or"} | branches], b, ctx) do
    for branch <- branches,
        r <- eval_clauses(branch_clauses(branch), [b], ctx),
        uniq: true,
        do: r
  end

  # a clause that starts with a symbol: [?e :a ?v] (a variable first) or (rule a b)
  defp eval_clause([{:symbol, name} | args] = clause, b, ctx) when name not in @special do
    if String.starts_with?(name, ["?", "_"]) do
      pattern(clause, b, ctx)
    else
      unless MapSet.member?(ctx.names, name), do: raise("unknown rule #{name} (no rules input?)")

      for tuple <- Map.get(ctx.rels, name, MapSet.new()),
          length(tuple) == length(args),
          b2 <- unify_all(args, tuple, b),
          do: b2
    end
  end

  # [123 :a ?v]  -- an entity id first
  defp eval_clause([_e, attr, _v] = clause, b, ctx) when is_atom(attr),
    do: pattern(clause, b, ctx)

  defp pattern([e, attr, v], b, ctx) do
    for {id, a, val} <- ctx.facts,
        a == attr,
        b1 <- unify(e, id, b),
        b2 <- unify(v, val, b1),
        do: b2
  end

  defp branch_clauses([{:symbol, "and"} | clauses]), do: clauses
  defp branch_clauses(clause), do: [clause]

  defp unify_all(terms, values, b) do
    Enum.zip(terms, values)
    |> Enum.reduce([b], fn {t, v}, bs -> Enum.flat_map(bs, &unify(t, v, &1)) end)
  end

  defp unify({:symbol, "_"}, _v, b), do: [b]

  defp unify({:symbol, name}, v, b) do
    case b do
      %{^name => ^v} -> [b]
      %{^name => _} -> []
      _ -> [Map.put(b, name, v)]
    end
  end

  defp unify(lit, v, b), do: if(lit == v, do: [b], else: [])

  defp value({:symbol, name}, b), do: Map.fetch!(b, name)
  defp value(lit, _b), do: lit

  defp names(vars), do: for({:symbol, n} <- vars, do: n)

  defp predicate(op, x, y) when op in ["=", "=="], do: x == y
  defp predicate(op, x, y) when op in ["not=", "!="], do: x != y
  defp predicate("<", x, y), do: x < y
  defp predicate("<=", x, y), do: x <= y
  defp predicate(">", x, y), do: x > y
  defp predicate(">=", x, y), do: x >= y
end
