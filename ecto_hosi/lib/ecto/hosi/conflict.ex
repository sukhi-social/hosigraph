defmodule Ecto.Hosi.Conflict do
  @moduledoc false
  # `on_conflict:` for `insert` and `insert_all`. What Ecto hands the adapter:
  #
  #     {:raise, [], []}                      the default
  #     {:nothing, [], targets}               on_conflict: :nothing
  #     {[field, ..], [], targets}            {:replace, ..}, :replace_all, :replace_all_except
  #     {%Ecto.Query{}, params, targets}      [set: .., inc: ..] or an update query
  #
  # `targets` is the `conflict_target:` as field names: `[:email]`, `[:a, :b]`. A row conflicts
  # with an existing row when all the target fields are equal (a nil never conflicts, as in SQL).
  #
  # Looking the existing row up and writing are two steps: with the target on a `hosi_unique`
  # field hosigraph's own `unique` is the guard against a race; without it two concurrent inserts
  # of the same target can both succeed.

  alias Ecto.Hosi.{Attr, Backend, Update}

  def check!({:raise, _, _}), do: :ok
  def check!({:nothing, _, targets}), do: targets!(targets)
  def check!({fields, _, targets}) when is_list(fields), do: targets!(targets)
  def check!({%Ecto.Query{} = q, _, targets}) do
    targets!(targets)

    if q.wheres != [] do
      raise ArgumentError, "Ecto.Adapters.Hosi does not support a where in an on_conflict query"
    end
  end

  def check!(other) do
    raise ArgumentError, "Ecto.Adapters.Hosi cannot handle on_conflict: #{inspect(other)}"
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
            "{:unsafe_fragment, \"(col, lower(col)) WHERE col IS NULL\"}, got: #{inspect(other)}"
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
                "columns and lower(column), then WHERE column IS [NOT] NULL / column = literal"
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
          m = Regex.run(~r/\Alower\((\w+)\)\z/i, p) -> {:lower, String.to_atom(Enum.at(m, 1))}
          Regex.match?(~r/\A\w+\z/, p) -> {:plain, String.to_atom(p)}
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
      m = Regex.run(~r/\A(\w+)\s+IS\s+NOT\s+NULL\z/i, text) -> {:not_null, atom(m)}
      m = Regex.run(~r/\A(\w+)\s+IS\s+NULL\z/i, text) -> {:null, atom(m)}
      m = Regex.run(~r/\A(\w+)\s*=\s*'([^']*)'\z/, text) -> {:eq, atom(m), Enum.at(m, 2)}
      m = Regex.run(~r/\A(\w+)\s*=\s*(true|false)\z/i, text) -> {:eq, atom(m), String.downcase(Enum.at(m, 2)) == "true"}
      m = Regex.run(~r/\A(\w+)\s*=\s*(-?\d+)\z/, text) -> {:eq, atom(m), String.to_integer(Enum.at(m, 2))}
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
  The key of a row, or nil when it has none to match (a NULL never conflicts, and a partial
  index only holds the rows its WHERE lets in).
  """
  def key(source, attrs, {_, _, targets}) when is_list(targets) and targets != [] do
    pairs = for f <- targets, do: {Attr.attr(source, f), Keyword.get(attrs, Attr.attr(source, f))}
    if Enum.any?(pairs, fn {_, v} -> is_nil(v) end), do: nil, else: {:plain, pairs}
  end

  def key(source, attrs, {_, _, {:unsafe_fragment, text}}) do
    {items, conds} = parse_fragment!(text)

    items =
      for {kind, f} <- items do
        {kind, Attr.attr(source, f), Keyword.get(attrs, Attr.attr(source, f))}
      end

    conds = for c <- conds, do: cond_attr(source, c)

    cond do
      Enum.any?(items, fn {_, _, v} -> is_nil(v) end) -> nil
      not Enum.all?(conds, &holds?(&1, attrs)) -> nil
      true -> {:frag, items, conds}
    end
  end

  def key(_source, _attrs, _on_conflict), do: nil

  defp cond_attr(source, {:null, f}), do: {:null, Attr.attr(source, f)}
  defp cond_attr(source, {:not_null, f}), do: {:not_null, Attr.attr(source, f)}
  defp cond_attr(source, {:eq, f, v}), do: {:eq, Attr.attr(source, f), v}

  # does a row being inserted (attr => value) fall inside the partial index?
  defp holds?({:null, a}, attrs), do: is_nil(Keyword.get(attrs, a))
  defp holds?({:not_null, a}, attrs), do: not is_nil(Keyword.get(attrs, a))
  defp holds?({:eq, a, v}, attrs), do: Keyword.get(attrs, a) == v

  @doc "`{:conflict, id}` of the existing row this one collides with, or `:none`."
  def find(meta, source, attrs, on_conflict) do
    case key(source, attrs, on_conflict) do
      nil -> :none
      {:plain, pairs} -> lookup(meta, source, pairs)
      {:frag, items, conds} -> lookup_frag(meta, source, items, conds)
    end
  end

  defp lookup(meta, source, pairs) do
    vars = for {_, i} <- Enum.with_index(pairs, 1), do: "?v#{i}"

    clauses =
      for {{attr, _}, var} <- Enum.zip(pairs, vars), do: "[?e #{Attr.kw(attr)} #{var}]"

    req = %{
      q:
        "[:find ?e :in $ #{Enum.join(vars, " ")} :where [?e #{Attr.kw(Attr.marker(source))} _] " <>
          "#{Enum.join(clauses, " ")}]",
      inputs: for({a, v} <- pairs, do: [:scalar, Ecto.Hosi.Kw.to_store(Backend.keywords(meta), a, v)])
    }

    case meta |> Backend.query(req) |> Backend.ok!() |> Enum.map(fn [id] -> id end) do
      [] -> :none
      ids -> {:conflict, Enum.min(ids)}
    end
  end

  # An expression / partial index. Datalog narrows by the plain columns and the WHERE; a
  # `lower(column)` is compared here, on the entities that came back.
  defp lookup_frag(meta, source, items, conds) do
    kws = Backend.keywords(meta)
    plain = for {:plain, a, v} <- items, do: {a, v}
    lowers = for {:lower, a, v} <- items, do: {a, String.downcase(v)}

    {cond_clauses, cond_inputs} =
      conds
      |> Enum.with_index(1)
      |> Enum.map(fn
        {{:null, a}, _} -> {"(not-join [?e] [?e #{Attr.kw(a)} _])", []}
        {{:not_null, a}, _} -> {"[?e #{Attr.kw(a)} _]", []}
        {{:eq, a, v}, i} -> {"[?e #{Attr.kw(a)} ?c#{i}]", [{"?c#{i}", Ecto.Hosi.Kw.to_store(kws, a, v)}]}
      end)
      |> Enum.unzip()

    cond_inputs = List.flatten(cond_inputs)
    pvars = for {_, i} <- Enum.with_index(plain, 1), do: "?v#{i}"
    pclauses = for {{a, _}, var} <- Enum.zip(plain, pvars), do: "[?e #{Attr.kw(a)} #{var}]"
    lclauses = for {a, _} <- lowers, do: "[?e #{Attr.kw(a)} _]"
    in_vars = pvars ++ Enum.map(cond_inputs, &elem(&1, 0))

    req = %{
      q:
        "[:find ?e :in $ #{Enum.join(in_vars, " ")} :where [?e #{Attr.kw(Attr.marker(source))} _] " <>
          Enum.join(pclauses ++ lclauses ++ cond_clauses, " ") <> "]",
      inputs:
        for({a, v} <- plain, do: [:scalar, Ecto.Hosi.Kw.to_store(kws, a, v)]) ++
          for({_, v} <- cond_inputs, do: [:scalar, v])
    }

    ids = meta |> Backend.query(req) |> Backend.ok!() |> Enum.map(fn [id] -> id end)

    matching =
      if lowers == [] or ids == [] do
        ids
      else
        entities = meta |> Backend.get_entities(ids) |> Backend.ok!() |> then(&Enum.zip(ids, &1))

        for {id, entity} <- entities,
            Enum.all?(lowers, fn {a, low} ->
              case Backend.first(entity, a) do
                v when is_binary(v) -> String.downcase(v) == low
                _ -> false
              end
            end),
            do: id
      end

    case matching do
      [] -> :none
      ids -> {:conflict, Enum.min(ids)}
    end
  end

  @doc "`{ops, conds}` that apply the conflict action to the existing row `id`."
  def ops(_meta, _source, _id, _entity, _attrs, {:nothing, _, _}), do: {[], []}

  def ops(meta, source, id, entity, attrs, {fields, _, _}) when is_list(fields) do
    values = for f <- fields, do: {Attr.attr(source, f), Keyword.get(attrs, Attr.attr(source, f))}
    values = Ecto.Hosi.Derived.extend(meta, source, id, values, fn -> entity end)
    {ops, conds} = Update.set_ops(meta, id, entity, values)
    {ops, [[:holds, id, Attr.marker(source), true] | conds]}
  end

  # The query's `^` indexes continue after the insert's own parameters, but `params` holds only
  # the query's: pad the front so an index finds its value.
  def ops(meta, source, id, entity, _attrs, {%Ecto.Query{} = q, params, _}) do
    padded = List.duplicate(nil, offset(q.updates)) ++ params
    Update.ops(meta, source, id, entity, q.updates, padded)
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
