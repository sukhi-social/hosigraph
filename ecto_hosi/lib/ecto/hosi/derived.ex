defmodule Ecto.Hosi.Derived do
  @moduledoc false
  # Unique rules hosigraph's per-attribute `unique` cannot say: a combination of fields, a
  # lower-cased value, a partial index. Each such rule (`hosi_unique [:a, :b], opts`) gets an
  # attribute of its own, `:"source/uq-<name>"`, that holds the JSON of the key's values while the
  # row is in the rule and is absent while it is not (a nil member, or the `where` not holding).
  # hosigraph's `unique: :value` on that attribute is then the guard, race included.
  #
  # `extend/5` is the one place that keeps the attribute right: whatever writes a row hands it the
  # attribute values it is about to write, and gets them back with the derived ones added (or
  # `{attr, nil}`, to retract one the row no longer has).
  #
  # A plain combination can instead be hosigraph's own tuple (`hosi_unique [:a, :b], tuple: true`,
  # `native: true` here): the khana keeps `[a b]` itself, so nothing is derived or written, and
  # `extend/5` only checks that no field is nil (a tuple's nil is a value, not SQL's NULL).

  alias Ecto.Hosi.{Attr, Backend}

  @doc "The rules of a schema module: `[%{attr, name, fields, attrs, lower, where}]`."
  def specs(mod) do
    source = mod.__schema__(:source)

    if function_exported?(mod, :__hosi__, 1) do
      for {fields, opts} <- mod.__hosi__(:uniques) do
        name = Keyword.get(opts, :name) || default_name(source, fields)
        where = Keyword.get(opts, :where, [])
        native = Keyword.get(opts, :tuple, false)
        if native, do: check_native!(mod, fields, opts)

        %{
          attr: Attr.attr(source, "uq-" <> name),
          name: name,
          native: native,
          fields: fields,
          attrs: Enum.map(fields, &Attr.attr(source, &1)),
          lower: opts |> Keyword.get(:lower, []) |> Enum.map(&Attr.attr(source, &1)),
          where:
            for(f <- Keyword.get(where, :null, []), do: {:null, Attr.attr(source, f)}) ++
              for(f <- Keyword.get(where, :not_null, []), do: {:not_null, Attr.attr(source, f)}) ++
              for({f, v} <- Keyword.get(where, :eq, []), do: {:eq, Attr.attr(source, f), v})
        }
      end
    else
      []
    end
  end

  defp default_name(source, [first | _]), do: "#{source}_#{first}_index"

  defp check_native!(mod, fields, opts) do
    if length(fields) < 2 or Keyword.has_key?(opts, :lower) or Keyword.has_key?(opts, :where) do
      raise ArgumentError,
            "#{inspect(mod)}: hosi_unique #{inspect(fields)}, tuple: true is for a plain combination " <>
              "of two or more fields (no :lower, no :where): a tuple cannot say those"
    end
  end

  @doc "`%{source => [spec]}` for these schema modules; only sources that have a rule."
  def build(schemas) do
    for mod <- schemas, specs = specs(mod), specs != [], into: %{} do
      {mod.__schema__(:source), specs}
    end
  end

  @doc "`%{derived attr => constraint name}`."
  def names(derived), do: for({_, specs} <- derived, s <- specs, into: %{}, do: {s.attr, s.name})

  @doc """
  `attrs` (`[{attr, value | nil}]`, the values a write is about to set; `nil` retracts) with the
  derived attributes of `source` that the write changes. `id` is the row being changed, or `nil` for
  a new row; `read_entity` reads that row when a rule needs what it holds now.
  """
  def extend(meta, source, id, attrs, read_entity \\ fn -> %{} end) do
    case Map.get(Map.get(meta, :derived, %{}), source) do
      nil ->
        attrs

      specs ->
        changed = Map.new(attrs)
        {native, specs} = Enum.split_with(specs, & &1.native)
        if id, do: Enum.each(native, &check_members!(&1, id, changed))
        touched = if id, do: Enum.filter(specs, &touches?(&1, changed)), else: specs

        if touched == [] do
          attrs
        else
          entity = if id, do: read_entity.(), else: %{}
          state = fn attr -> if Map.has_key?(changed, attr), do: changed[attr], else: Backend.first(entity, attr) end

          attrs ++
            Enum.flat_map(touched, fn spec ->
              new = key(spec, state)
              old = Backend.first(entity, spec.attr)

              cond do
                new == old -> []
                is_nil(new) -> [{spec.attr, nil}]
                true -> [{spec.attr, new}]
              end
            end)
        end
    end
  end

  # A tuple's nil is a value: two rows `(1, nil)` would be the same pair. SQL's NULLs are distinct,
  # so a rule that must keep that cannot be a tuple: say so instead of refusing rows later.
  defp check_members!(spec, id, changed) do
    missing =
      if id,
        do: for(a <- spec.attrs, Map.has_key?(changed, a), is_nil(changed[a]), do: a),
        else: for(a <- spec.attrs, is_nil(Map.get(changed, a)), do: a)

    if missing != [] do
      raise ArgumentError,
            "#{spec.name}: hosi_unique #{inspect(spec.fields)}, tuple: true needs every field set, " <>
              "but #{inspect(missing)} would be nil (a tuple's nil is a value, SQL's NULL is not)"
    end
  end

  @doc """
  A new row's attributes (`[{attr, value}]`, a value that is not known yet written as `[:ref, ..]`)
  checked against the `tuple: true` rules of `source`: every field of such a rule must be set.
  `extend/5` checks an update (a field set to nil); only the caller knows a new row is complete.
  """
  def check_new!(meta, source, attrs) do
    changed = Map.new(attrs)

    meta
    |> Map.get(:derived, %{})
    |> Map.get(source, [])
    |> Enum.filter(& &1.native)
    |> Enum.each(&check_members!(&1, nil, changed))
  end

  defp touches?(spec, changed) do
    Enum.any?(spec.attrs ++ where_attrs(spec), &Map.has_key?(changed, &1))
  end

  defp where_attrs(spec), do: for(w <- spec.where, do: elem(w, 1))

  # the key's JSON while the row is in the rule, nil while it is not
  defp key(spec, state) do
    values = Enum.map(spec.attrs, state)

    cond do
      Enum.any?(values, &is_nil/1) -> nil
      not Enum.all?(spec.where, &holds?(&1, state)) -> nil
      true -> values |> Enum.zip(spec.attrs) |> Enum.map(&fold(&1, spec.lower)) |> JSON.encode!()
    end
  end

  defp fold({v, attr}, lower) when is_binary(v), do: if(attr in lower, do: String.downcase(v), else: v)
  defp fold({v, _}, _), do: v

  defp holds?({:null, attr}, state), do: is_nil(state.(attr))
  defp holds?({:not_null, attr}, state), do: not is_nil(state.(attr))
  defp holds?({:eq, attr, v}, state), do: state.(attr) == v
end
