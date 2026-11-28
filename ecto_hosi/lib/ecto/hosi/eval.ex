defmodule Ecto.Hosi.Eval do
  @moduledoc false
  # Evaluates an Ecto query expression over rows read from hosigraph, in Elixir, with SQL's
  # meaning (NULL propagates; and/or/not are three-valued). Used by `Ecto.Hosi.Hybrid` to
  # re-check the whole `where` of a query whose candidates Datalog fetched, and to compute the
  # parts Datalog does not: fragments, like, aggregates, coalesce.
  #
  # An env is `%{binding_index => row | nil}`; a row is `%{id:, entity:}` (nil: a left join that
  # found nothing). Values are in the adapter's form (already dumped), as are the query's params.

  alias Ecto.Hosi.{Backend, Types}

  @cmp [:==, :!=, :<, :<=, :>, :>=]
  @aggs [:count, :sum, :min, :max, :avg]

  def aggregate?({agg, _, args}) when agg in @aggs and is_list(args), do: true
  def aggregate?({:filter, _, [e, _]}), do: aggregate?(e)
  def aggregate?({_, _, args}) when is_list(args), do: Enum.any?(args, &aggregate?/1)
  def aggregate?(list) when is_list(list), do: Enum.any?(list, &aggregate?/1)
  def aggregate?({a, b}), do: aggregate?(a) or aggregate?(b)
  def aggregate?(_), do: false

  ## ------------------------------------------------------------ one row

  def eval({{:., _, [{:&, _, [ix]}, f]}, _, []}, env, ctx), do: field(env, ix, f, ctx)
  def eval({:^, _, [ix]}, _env, ctx), do: Enum.at(ctx.params, ix)
  def eval({:^, _, [ix, len]}, _env, ctx), do: Enum.slice(ctx.params, ix, len)
  def eval(%Ecto.Query.Tagged{value: v}, env, ctx), do: eval(v, env, ctx)
  def eval({:type, _, [e, _]}, env, ctx), do: eval(e, env, ctx)
  def eval({:selected_as, _, [e, _]}, env, ctx), do: eval(e, env, ctx)

  def eval({:and, _, [a, b]}, env, ctx) do
    case eval(a, env, ctx) do
      false -> false
      x -> and3(x, eval(b, env, ctx))
    end
  end

  def eval({:or, _, [a, b]}, env, ctx) do
    case eval(a, env, ctx) do
      true -> true
      x -> or3(x, eval(b, env, ctx))
    end
  end

  def eval({:not, _, [a]}, env, ctx) do
    case eval(a, env, ctx) do
      nil -> nil
      v -> not v
    end
  end

  def eval({:is_nil, _, [a]}, env, ctx), do: is_nil(eval(a, env, ctx))

  def eval({op, _, [a, b]}, env, ctx) when op in @cmp do
    compare(op, eval(a, env, ctx), eval(b, env, ctx))
  end

  def eval({:in, _, [a, b]}, env, ctx) do
    case {eval(a, env, ctx), eval(b, env, ctx)} do
      {nil, _} -> nil
      {x, list} -> x in (list |> Types.unpack_list() |> List.wrap())
    end
  end

  def eval({:like, _, [a, b]}, env, ctx), do: like(eval(a, env, ctx), eval(b, env, ctx), false)
  def eval({:ilike, _, [a, b]}, env, ctx), do: like(eval(a, env, ctx), eval(b, env, ctx), true)

  def eval({:coalesce, _, args}, env, ctx),
    do: Enum.find_value(args, fn a -> eval(a, env, ctx) end)

  def eval({:fragment, _, parts}, env, ctx), do: fragment(parts, env, ctx)

  def eval(list, env, ctx) when is_list(list), do: Enum.map(list, &eval(&1, env, ctx))

  def eval({agg, _, args} = e, _env, _ctx) when agg in @aggs and is_list(args) do
    raise ArgumentError,
          "Ecto.Adapters.Hosi: the aggregate #{Macro.to_string(e)} is not valid here " <>
            "(ecto_hosi's limit)"
  end

  def eval(v, _env, _ctx)
      when is_number(v) or is_binary(v) or is_boolean(v) or is_nil(v) or is_atom(v),
      do: v

  def eval(other, _env, _ctx) do
    raise ArgumentError,
          "Ecto.Adapters.Hosi cannot evaluate #{Macro.to_string(other)} (ecto_hosi's limit); " <>
            "use Repo.datalog/1 for it"
  end

  ## ------------------------------------------------------------ a group of rows

  @doc "An expression over a group: aggregates collapse the group; the rest is read from its first row."
  def eval_group(expr, envs, ctx) do
    if aggregate?(expr) do
      expr |> subst(envs, ctx) |> eval(List.first(envs) || %{}, ctx)
    else
      eval(expr, List.first(envs) || %{}, ctx)
    end
  end

  defp subst({:filter, _, [e, cond]}, envs, ctx) do
    kept = Enum.filter(envs, &(eval(cond, &1, ctx) == true))
    subst(e, kept, ctx)
  end

  defp subst({agg, _, args} = e, envs, ctx) when agg in @aggs and is_list(args),
    do: aggregate(e, envs, ctx)

  defp subst({a, m, args}, envs, ctx) when is_list(args),
    do: {a, m, Enum.map(args, &subst(&1, envs, ctx))}

  defp subst(list, envs, ctx) when is_list(list), do: Enum.map(list, &subst(&1, envs, ctx))
  defp subst(other, _envs, _ctx), do: other

  defp aggregate({:count, _, []}, envs, _ctx), do: length(envs)
  defp aggregate({:count, _, [:*]}, envs, _ctx), do: length(envs)

  defp aggregate({:count, _, [e]}, envs, ctx),
    do: envs |> values(e, ctx) |> length()

  defp aggregate({:count, _, [e, :distinct]}, envs, ctx),
    do: envs |> values(e, ctx) |> Enum.uniq() |> length()

  defp aggregate({:sum, _, [e]}, envs, ctx), do: reduce(values(envs, e, ctx), &Kernel.+/2)
  defp aggregate({:min, _, [e]}, envs, ctx), do: reduce(values(envs, e, ctx), &min/2)
  defp aggregate({:max, _, [e]}, envs, ctx), do: reduce(values(envs, e, ctx), &max/2)

  defp aggregate({:avg, _, [e]}, envs, ctx) do
    case values(envs, e, ctx) do
      [] -> nil
      vs -> Enum.sum(vs) / length(vs)
    end
  end

  defp values(envs, e, ctx), do: envs |> Enum.map(&eval(e, &1, ctx)) |> Enum.reject(&is_nil/1)

  defp reduce([], _f), do: nil
  defp reduce(vs, f), do: Enum.reduce(vs, f)

  ## ------------------------------------------------------------ parts

  defp field(env, ix, f, ctx) do
    case Map.get(env, ix) do
      nil ->
        nil

      %{id: id} when f == :id ->
        id

      %{entity: entity} ->
        case ctx.meta.model.attrs[f] do
          %{card: :many} -> entity |> Map.get(f, []) |> Enum.sort()
          _ -> Backend.first(entity, f)
        end
    end
  end

  defp and3(true, true), do: true
  defp and3(_, false), do: false
  defp and3(_, _), do: nil

  defp or3(false, false), do: false
  defp or3(_, true), do: true
  defp or3(_, _), do: nil

  defp compare(_op, nil, _), do: nil
  defp compare(_op, _, nil), do: nil
  defp compare(:==, a, b), do: a == b
  defp compare(:!=, a, b), do: a != b
  defp compare(:<, a, b), do: a < b
  defp compare(:<=, a, b), do: a <= b
  defp compare(:>, a, b), do: a > b
  defp compare(:>=, a, b), do: a >= b

  defp like(nil, _, _), do: nil
  defp like(_, nil, _), do: nil

  defp like(s, pattern, ci?) when is_binary(s) and is_binary(pattern) do
    re =
      pattern
      |> String.graphemes()
      |> Enum.map_join(fn
        "%" -> ".*"
        "_" -> "."
        g -> Regex.escape(g)
      end)

    Regex.match?(Regex.compile!("\\A" <> re <> "\\z", if(ci?, do: "isu", else: "su")), s)
  end

  ## ------------------------------------------------------------ fragments
  #
  # The few the app uses, by their text. Anything else raises and says so.

  defp fragment(parts, env, ctx) do
    {text, args} =
      Enum.reduce(parts, {"", []}, fn
        {:raw, s}, {t, a} -> {t <> s, a}
        {:expr, e}, {t, a} -> {t <> "?", a ++ [eval(e, env, ctx)]}
      end)

    run_fragment(String.trim(text), args, ctx)
  end

  defp run_fragment("lower(?)", [v], _), do: if(is_binary(v), do: String.downcase(v))
  defp run_fragment("upper(?)", [v], _), do: if(is_binary(v), do: String.upcase(v))
  defp run_fragment("length(?)", [v], _), do: if(is_binary(v), do: String.length(v))
  defp run_fragment("? LIKE ?", [a, b], _), do: like(a, b, false)
  defp run_fragment("? ILIKE ?", [a, b], _), do: like(a, b, true)

  defp run_fragment("?->>'" <> key, [v], _), do: json_text(v, String.trim_trailing(key, "'"))

  defp run_fragment("CAST(?->>'" <> rest, [v], _) do
    [key, _] = String.split(rest, "' AS ", parts: 2)
    json_text(v, key)
  end

  defp run_fragment(text, _args, _ctx), do: unsupported_fragment(text)

  defp unsupported_fragment(text) do
    raise ArgumentError,
          "Ecto.Adapters.Hosi cannot evaluate the fragment #{inspect(text)} " <>
            "(ecto_hosi's limit); use Repo.datalog/1 for it"
  end

  # A map is kept as JSON text; `->>` reads one key as text.
  defp json_text(nil, _key), do: nil

  defp json_text(text, key) when is_binary(text) do
    case JSON.decode(text) do
      {:ok, %{} = map} ->
        case Map.get(map, key) do
          nil -> nil
          v when is_binary(v) -> v
          v -> JSON.encode!(v)
        end

      _ ->
        nil
    end
  end
end
