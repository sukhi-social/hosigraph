defmodule Ecto.Hosi.Derived do
  @moduledoc false
  # Unique rules hosigraph's per-attribute `unique` cannot say: a combination of attributes, a
  # lower-cased value, a partial index. Each such rule (`hosi_unique [:a, :b], opts`) gets an
  # attribute of its own, `:"hosi/uq-<name>"`, that holds the JSON of the key's values while a
  # record is in the rule and is absent while it is not (a nil member, or the `where` not holding).
  # hosigraph's `unique: :value` on that attribute is then the guard, race included.
  #
  # A rule is over attributes: it holds for every record that has them, whichever view wrote it.
  #
  # `changes/4` is the one place that keeps the attribute right: whatever writes a record hands it
  # the values it is about to set, and gets back the derived values to set with them.
  #
  # A plain combination can instead be hosigraph's own tuple (`hosi_unique [:a, :b], tuple: true`,
  # `native: true` here): the khana keeps `[a b]` itself, so nothing is derived or written, and
  # `changes/4` only checks that a record that has one of the members has them all (a tuple's nil
  # is a value, not SQL's NULL).

  @doc "The rules a view declares: `[%{attr, name, native, attrs, lower, where}]`."
  def specs(module, view) do
    for {fields, opts} <- module.__hosi__(:uniques) do
      to_attr = fn field ->
        case view.attr_of do
          %{^field => attr} ->
            attr

          _ ->
            raise ArgumentError,
                  "#{inspect(module)}: hosi_unique names #{inspect(field)}, which is not a field of the view"
        end
      end

      case Keyword.keys(opts) -- [:name, :lower, :where, :tuple] do
        [] -> :ok
        unknown -> raise ArgumentError, "#{inspect(module)}: hosi_unique: unknown #{inspect(unknown)}"
      end

      attrs = Enum.map(fields, to_attr)
      name = Keyword.get(opts, :name) || "#{view.source}_#{Enum.join(attrs, "_")}_index"
      where = Keyword.get(opts, :where, [])
      native = Keyword.get(opts, :tuple, false)
      if native, do: check_native!(module, fields, opts)

      %{
        attr: String.to_atom("hosi/uq-" <> String.replace(name, "/", ".")),
        name: name,
        native: native,
        attrs: attrs,
        lower: opts |> Keyword.get(:lower, []) |> Enum.map(to_attr),
        where:
          for(f <- Keyword.get(where, :null, []), do: {:null, to_attr.(f)}) ++
            for(f <- Keyword.get(where, :not_null, []), do: {:not_null, to_attr.(f)}) ++
            for({f, v} <- Keyword.get(where, :eq, []), do: {:eq, to_attr.(f), v})
      }
    end
  end

  defp check_native!(module, fields, opts) do
    if length(fields) < 2 or Keyword.has_key?(opts, :lower) or Keyword.has_key?(opts, :where) do
      raise ArgumentError,
            "#{inspect(module)}: hosi_unique #{inspect(fields)}, tuple: true is for a plain combination " <>
              "of two or more fields (no :lower, no :where): a tuple cannot say those"
    end
  end

  @doc "The rules a write of these attributes touches."
  def touched(model, attrs) do
    attrs = MapSet.new(attrs)

    Enum.filter(model.rules, fn rule ->
      Enum.any?(rule.attrs ++ where_attrs(rule), &MapSet.member?(attrs, &1))
    end)
  end

  @doc """
  The derived values to set along with `changed` (`%{attribute => value | nil}`, the values a write
  is about to set; `nil` removes) in a record that holds `entity` now (`%{}` for a new one):
  `[{attribute, value | nil}]`. Raises for a record left with only part of a tuple rule.
  """
  def changes(model, changed, entity) do
    case touched(model, Map.keys(changed)) do
      [] ->
        []

      rules ->
        state = fn attr ->
          if Map.has_key?(changed, attr), do: changed[attr], else: first(entity, attr)
        end

        {native, derived} = Enum.split_with(rules, & &1.native)
        Enum.each(native, &check_members!(&1, state))

        Enum.flat_map(derived, fn rule ->
          new = key(rule, state)
          old = first(entity, rule.attr)

          cond do
            new == old -> []
            true -> [{rule.attr, new}]
          end
        end)
    end
  end

  defp first(entity, attr) do
    case Map.get(entity, attr) do
      [value | _] -> value
      _ -> nil
    end
  end

  # A tuple's nil is a value: two records `(1, nil)` would be the same pair. SQL's NULLs are
  # distinct, so a rule that must keep that cannot be a tuple: say so instead of refusing later.
  defp check_members!(rule, state) do
    missing = Enum.filter(rule.attrs, &is_nil(state.(&1)))

    if missing != [] and length(missing) != length(rule.attrs) do
      raise ArgumentError,
            "#{rule.name}: hosi_unique over #{inspect(rule.attrs)}, tuple: true needs every field set, " <>
              "but #{inspect(missing)} would be nil (a tuple's nil is a value, SQL's NULL is not: " <>
              "hosigraph's limit)"
    end
  end

  defp where_attrs(rule), do: for(w <- rule.where, do: elem(w, 1))

  # the key's JSON while the record is in the rule, nil while it is not
  defp key(rule, state) do
    values = Enum.map(rule.attrs, state)

    cond do
      Enum.any?(values, &is_nil/1) -> nil
      not Enum.all?(rule.where, &holds?(&1, state)) -> nil
      Enum.any?(values, &unknown?/1) -> unknown!(rule)
      true -> values |> Enum.zip(rule.attrs) |> Enum.map(&fold(&1, rule.lower)) |> JSON.encode!()
    end
  end

  # a reference to a record that the same write makes: its id is not known yet
  defp unknown?([:ref, [:tmp, _]]), do: true
  defp unknown?([:tmp, _]), do: true
  defp unknown?(_), do: false

  defp unknown!(rule) do
    raise ArgumentError,
          "#{rule.name}: this unique rule covers a reference to a record made in the same write, " <>
            "which the adapter cannot check. Write that record first, or make the rule a tuple " <>
            "(tuple: true), which hosigraph checks itself (ecto_hosi's limit)"
  end

  defp fold({v, attr}, lower) when is_binary(v), do: if(attr in lower, do: String.downcase(v), else: v)
  defp fold({v, _}, _), do: v

  defp holds?({:null, attr}, state), do: is_nil(state.(attr))
  defp holds?({:not_null, attr}, state), do: not is_nil(state.(attr))
  defp holds?({:eq, attr, v}, state), do: state.(attr) == v
end
