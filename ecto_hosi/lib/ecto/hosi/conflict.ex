defmodule Ecto.Hosi.Conflict do
  @moduledoc false
  # `on_conflict:` for `insert` and `insert_all`. What Ecto hands the adapter:
  #
  #     {:raise, [], []}                      the default
  #     {:nothing, [], targets}               on_conflict: :nothing
  #     {[field, ..], [], targets}            {:replace, ..}, :replace_all, :replace_all_except
  #     {%Ecto.Query{}, params, targets}      [set: .., inc: ..] or an update query
  #
  # `targets` is the `conflict_target:` as attributes: `[:email]`, `[:a, :b]`. A row conflicts
  # with an existing record when all the target attributes are equal (a nil never conflicts, as in
  # SQL).
  #
  # A unique rule holds across every record, whichever view wrote it, so the record that holds a
  # unique value may be one the view does not show (a tombstone keeps its ids). `:nothing` then
  # skips the row; an action that would change that record is refused.
  #
  # Looking the existing record up and writing are two steps: with the target on a unique rule
  # hosigraph's own `unique` is the guard against a race; without it two concurrent inserts of the
  # same target can both succeed.

  alias Ecto.Hosi.{Backend, Change, Model, Reader, Store, Update}

  def check!({:raise, _, _}), do: :ok
  def check!({:nothing, _, targets}), do: targets!(targets)
  def check!({fields, _, targets}) when is_list(fields), do: targets!(targets)
  def check!({%Ecto.Query{} = q, _, targets}) do
    targets!(targets)

    if q.wheres != [] do
      raise ArgumentError,
            "Ecto.Adapters.Hosi does not support a where in an on_conflict query (ecto_hosi's limit)"
    end
  end

  def check!(other) do
    raise ArgumentError,
          "Ecto.Adapters.Hosi cannot handle on_conflict: #{inspect(other)} (ecto_hosi's limit)"
  end

  defp targets!(targets) when is_list(targets) do
    if Enum.all?(targets, &is_atom/1), do: :ok, else: targets!({:other, targets})
  end

  defp targets!({:unsafe_fragment, text}) do
    parse_fragment!(text)
    :ok
  end

  defp targets!(other) do
    raise ArgumentError,
          "Ecto.Adapters.Hosi supports conflict_target: [fields] and " <>
            "{:unsafe_fragment, \"(col, lower(col)) WHERE col IS NULL\"} (ecto_hosi's limit), " <>
            "got: #{inspect(other)}"
  end

  ## ---- `{:unsafe_fragment, "(a, lower(b)) WHERE c IS NULL AND d = 'x'"}`: an expression or
  # partial unique index. Columns and `lower(column)`; conditions `IS NULL`, `IS NOT NULL`, `= literal`.

  defp parse_fragment!(text) do
    with {:ok, inner, rest} <- paren(String.trim(text)),
         {:ok, items} <- items(inner),
         {:ok, conds} <- conds(rest) do
      {items, conds}
    else
      _ ->
        raise ArgumentError,
              "Ecto.Adapters.Hosi cannot read the conflict_target #{inspect(text)}: " <>
                "columns and lower(column), then WHERE column IS [NOT] NULL / column = literal " <>
                "(ecto_hosi's limit)"
    end
  end

  # "(a, lower(b)) WHERE ..." -> {:ok, "a, lower(b)", "WHERE ..."}
  defp paren("(" <> rest) do
    case scan(String.graphemes(rest), 1, []) do
      {inner, after_} -> {:ok, inner, String.trim(after_)}
      :error -> :error
    end
  end

  defp paren(_), do: :error

  defp scan([], _depth, _acc), do: :error
  defp scan([")" | rest], 1, acc), do: {acc |> Enum.reverse() |> Enum.join(), Enum.join(rest)}
  defp scan([")" | rest], d, acc), do: scan(rest, d - 1, [")" | acc])
  defp scan(["(" | rest], d, acc), do: scan(rest, d + 1, ["(" | acc])
  defp scan([g | rest], d, acc), do: scan(rest, d, [g | acc])

  defp items(text) do
    parts = text |> String.split(",") |> rejoin([])

    result =
      for p <- parts do
        p = String.trim(p)

        cond do
          m = Regex.run(~r/\Alower\(([\w\/.\-]+)\)\z/i, p) -> {:lower, String.to_atom(Enum.at(m, 1))}
          Regex.match?(~r/\A[\w\/.\-]+\z/, p) -> {:plain, String.to_atom(p)}
          true -> :error
        end
      end

    if result == [] or :error in result, do: :error, else: {:ok, result}
  end

  # commas inside lower(...) do not split (there are none today, but keep it honest)
  defp rejoin([], acc), do: Enum.reverse(acc)
  defp rejoin([a | rest], acc), do: rejoin(rest, [a | acc])

  defp conds(""), do: {:ok, []}

  defp conds(text) do
    case Regex.run(~r/\AWHERE\s+(.*)\z/is, text) do
      [_, body] ->
        result =
          body
          |> String.split(~r/\s+AND\s+/i)
          |> Enum.map(&cond1(String.trim(&1)))

        if :error in result, do: :error, else: {:ok, result}

      _ ->
        :error
    end
  end

  defp cond1(text) do
    cond do
      m = Regex.run(~r/\A([\w\/.\-]+)\s+IS\s+NOT\s+NULL\z/i, text) -> {:not_null, atom(m)}
      m = Regex.run(~r/\A([\w\/.\-]+)\s+IS\s+NULL\z/i, text) -> {:null, atom(m)}
      m = Regex.run(~r/\A([\w\/.\-]+)\s*=\s*'([^']*)'\z/, text) -> {:eq, atom(m), Enum.at(m, 2)}
      m = Regex.run(~r/\A([\w\/.\-]+)\s*=\s*(true|false)\z/i, text) -> {:eq, atom(m), String.downcase(Enum.at(m, 2)) == "true"}
      m = Regex.run(~r/\A([\w\/.\-]+)\s*=\s*(-?\d+)\z/, text) -> {:eq, atom(m), String.to_integer(Enum.at(m, 2))}
      true -> :error
    end
  end

  defp atom([_, col | _]), do: String.to_atom(col)

  def raise?({:raise, _, _}), do: true
  def raise?(_), do: false

  def nothing?({:nothing, _, _}), do: true
  def nothing?(_), do: false

  @doc "Without a target, `:nothing` means any unique violation (the caller checks those)."
  def targetless?({_, _, []}), do: true
  def targetless?(_), do: false

  @doc """
  The key of a row (`[{attribute, value}]`), or nil when it has none to match (a nil never
  conflicts, and a partial index only holds the records its WHERE lets in).
  """
  def key(_view, values, {_, _, targets}) when is_list(targets) and targets != [] do
    pairs = for attr <- targets, do: {attr, get(values, attr)}
    if Enum.any?(pairs, fn {_, v} -> is_nil(v) end), do: nil, else: {:plain, pairs}
  end

  def key(_view, values, {_, _, {:unsafe_fragment, text}}) do
    {items, conds} = parse_fragment!(text)
    items = for {kind, attr} <- items, do: {kind, attr, get(values, attr)}

    cond do
      Enum.any?(items, fn {_, _, v} -> is_nil(v) end) -> nil
      not Enum.all?(conds, &holds?(&1, values)) -> nil
      true -> {:frag, items, conds}
    end
  end

  def key(_view, _values, _on_conflict), do: nil

  defp get(values, attr) do
    case List.keyfind(values, attr, 0) do
      {_, v} -> v
      nil -> nil
    end
  end

  # does a row being inserted fall inside the partial index?
  defp holds?({:null, a}, values), do: is_nil(get(values, a))
  defp holds?({:not_null, a}, values), do: not is_nil(get(values, a))
  defp holds?({:eq, a, v}, values), do: get(values, a) == v

  @doc """
  The existing record a row collides with: `{:conflict, id}` when the view shows it,
  `{:elsewhere, id}` when a unique value is held by a record the view does not show, or `:none`.
  """
  def find(meta, view, values, on_conflict) do
    known!(view, on_conflict)

    case key(view, values, on_conflict) do
      nil ->
        :none

      {:plain, pairs} ->
        attrs = Enum.map(pairs, &elem(&1, 0))
        found(meta, view, lookup(meta, view, pairs, unique?(meta, attrs)))

      {:frag, items, conds} ->
        attrs = Enum.map(items, &elem(&1, 1))
        found(meta, view, lookup_frag(meta, view, items, conds, unique?(meta, attrs)))
    end
  end

  # A target that names no attribute of the view would never match, and the row would be written
  # beside the one it was to meet.
  defp known!(view, {_, _, targets}) do
    columns =
      case targets do
        {:unsafe_fragment, text} ->
          {items, conds} = parse_fragment!(text)
          Enum.map(items, &elem(&1, 1)) ++ Enum.map(conds, &elem(&1, 1))

        list ->
          list
      end

    # (`:id` is hosigraph's own: a new record's never meets another)
    case Enum.reject(columns, &(&1 == :id or Map.has_key?(view.field_of, &1))) do
      [] ->
        :ok

      unknown ->
        raise ArgumentError,
              "Ecto.Adapters.Hosi: conflict_target names #{Enum.map_join(unknown, ", ", &Model.kw/1)}, " <>
                "which #{inspect(view.module)} does not have. A column is the attribute of a field " <>
                "(its source): #{view.field_of |> Map.keys() |> Enum.sort() |> Enum.map_join(", ", &Model.kw/1)}"
    end
  end

  # whether the attributes of a target are held once: a unique attribute, or a rule over them
  defp unique?(meta, [attr]) do
    match?(%{unique: true}, meta.model.attrs[attr]) or rule_over?(meta, [attr])
  end

  defp unique?(meta, attrs), do: rule_over?(meta, attrs)

  defp rule_over?(meta, attrs),
    do: Enum.any?(meta.model.rules, &(Enum.sort(&1.attrs) == Enum.sort(attrs)))

  defp found(_meta, _view, []), do: :none

  defp found(meta, view, ids) do
    records = Backend.read(meta, Enum.sort(ids))

    case Enum.find(records, fn {_, entity} -> Model.member?(view, entity) end) do
      {id, _} -> {:conflict, id}
      nil -> {:elsewhere, hd(Enum.sort(ids))}
    end
  end

  @doc "The refusal of an action on a record that the view does not show."
  def elsewhere!(view, id) do
    raise Ecto.Hosi.Error,
      reason: {:elsewhere, id},
      message:
        "a unique value of this insert is held by record #{id}, which #{inspect(view.module)} " <>
          "does not show, so on_conflict will not change it. (ecto_hosi's limit)"
  end

  # the ids that have these values. A unique target is looked for among all records; another only
  # among those the view shows.
  defp lookup(meta, view, pairs, unique?) do
    vars = for {_, i} <- Enum.with_index(pairs, 1), do: "?v#{i}"
    clauses = for {{attr, _}, var} <- Enum.zip(pairs, vars), do: "[?e #{Model.kw(attr)} #{var}]"
    member = if unique?, do: [], else: Reader.member_clauses(meta, view, "?e")

    req = %{
      q: "[:find ?e :in $ #{Enum.join(vars, " ")} :where #{Enum.join(clauses ++ member, " ")}]",
      inputs: for({a, v} <- pairs, do: [:scalar, Store.out(meta.model, a, v)])
    }

    meta |> Backend.query(req) |> Backend.ok!() |> Enum.map(fn [id] -> id end)
  end

  # An expression / partial index. Datalog narrows by the plain columns and the WHERE; a
  # `lower(column)` is compared here, on the entities that came back.
  defp lookup_frag(meta, view, items, conds, unique?) do
    plain = for {:plain, a, v} <- items, do: {a, v}
    lowers = for {:lower, a, v} <- items, do: {a, String.downcase(v)}

    {cond_clauses, cond_inputs} =
      conds
      |> Enum.with_index(1)
      |> Enum.map(fn
        {{:null, a}, _} -> {"(not-join [?e] [?e #{Model.kw(a)} _])", []}
        {{:not_null, a}, _} -> {"[?e #{Model.kw(a)} _]", []}
        {{:eq, a, v}, i} -> {"[?e #{Model.kw(a)} ?c#{i}]", [{"?c#{i}", Store.out(meta.model, a, v)}]}
      end)
      |> Enum.unzip()

    cond_inputs = List.flatten(cond_inputs)
    pvars = for {_, i} <- Enum.with_index(plain, 1), do: "?v#{i}"
    pclauses = for {{a, _}, var} <- Enum.zip(plain, pvars), do: "[?e #{Model.kw(a)} #{var}]"
    lclauses = for {a, _} <- lowers, do: "[?e #{Model.kw(a)} _]"
    in_vars = pvars ++ Enum.map(cond_inputs, &elem(&1, 0))
    member = if unique?, do: [], else: Reader.member_clauses(meta, view, "?e")

    # (a not-join needs a clause before it that binds ?e)
    clauses = pclauses ++ lclauses ++ member ++ cond_clauses

    req = %{
      q: "[:find ?e :in $ #{Enum.join(in_vars, " ")} :where #{Enum.join(clauses, " ")}]",
      inputs:
        for({a, v} <- plain, do: [:scalar, Store.out(meta.model, a, v)]) ++
          for({_, v} <- cond_inputs, do: [:scalar, v])
    }

    ids = meta |> Backend.query(req) |> Backend.ok!() |> Enum.map(fn [id] -> id end)

    if lowers == [] do
      ids
    else
      for {id, entity} <- Backend.read(meta, ids),
          Enum.all?(lowers, fn {a, low} ->
            case Backend.first(entity, a) do
              v when is_binary(v) -> String.downcase(v) == low
              _ -> false
            end
          end),
          do: id
    end
  end

  @doc "`{ops, conds}` that apply the conflict action to the existing record `id`."
  def ops(_meta, _view, _id, _entity, _values, {:nothing, _, _}), do: {[], []}

  def ops(meta, view, id, entity, values, {attrs, _, _}) when is_list(attrs) do
    changes = for attr <- attrs, do: {attr, get(values, attr)}
    {ops, conds} = Change.update(meta, id, entity, changes)
    {ops, Change.existence(meta, view, id, entity) ++ conds}
  end

  # The query's `^` indexes continue after the insert's own parameters, but `params` holds only
  # the query's: pad the front so an index finds its value.
  def ops(meta, view, id, entity, _values, {%Ecto.Query{} = q, params, _}) do
    padded = List.duplicate(nil, offset(q.updates)) ++ params
    Update.ops(meta, view, id, entity, q.updates, padded)
  end

  defp offset(updates) do
    case pins(updates, []) do
      [] -> 0
      ixs -> Enum.min(ixs)
    end
  end

  defp pins({:^, _, [ix]}, acc) when is_integer(ix), do: [ix | acc]
  defp pins(%{expr: e}, acc), do: pins(e, acc)
  defp pins(list, acc) when is_list(list), do: Enum.reduce(list, acc, &pins/2)
  defp pins({a, b}, acc), do: pins(b, pins(a, acc))
  defp pins(_, acc), do: acc
end
