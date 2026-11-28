defmodule Ecto.Hosi.Model do
  @moduledoc false
  # What the Repo's views and vocabularies declare, put together once when the Repo starts:
  # the attributes and their rules, the views, and the rules over several attributes. Everything
  # that reads or writes asks this. A Repo whose declarations do not agree does not start.
  #
  #   attrs  %{attribute => spec}
  #   views  %{module => view}
  #   rules  the combined, lower-cased and partial unique rules (`Ecto.Hosi.Derived`)
  #   schema the `schema:` map hosigraph is opened with

  alias Ecto.Hosi.Types

  defstruct attrs: %{}, views: %{}, rules: [], vocabularies: [], schema: %{}

  @owner :"hosi/owner"

  # an attribute name that EDN reads as one keyword
  @part "[A-Za-z_][A-Za-z0-9_.*+!?$%&=<>\\-]*"
  @name Regex.compile!("\\A#{@part}(/#{@part})?\\z")

  @spec_keys [:unique, :final, :file, :record_type, :ref, :keyword, :on_delete, :fk_name]

  @doc "The attribute that says who made a record."
  def owner, do: @owner

  @doc "EDN text of an attribute, as a query writes it."
  def kw(attr) when is_atom(attr), do: ":" <> Atom.to_string(attr)

  @doc "The model of these schema modules (the Repo's `:schemas`). Raises when it cannot be."
  def build(modules) do
    modules = Enum.uniq(modules)
    Enum.each(modules, &view!/1)

    views = Map.new(modules, &{&1, view_of(&1)})

    vocabularies =
      modules |> Enum.flat_map(& &1.__hosi__(:vocabulary)) |> Enum.map(&elem(&1, 1)) |> Enum.uniq()

    check_prefixes!(vocabularies, views)

    declarations =
      Enum.flat_map(vocabularies, &vocabulary_declarations/1) ++
        Enum.flat_map(modules, &declarations(&1, views[&1])) ++ [owner_declaration()]

    attrs = declarations |> Enum.group_by(& &1.attr) |> Map.new(&merge/1)
    rules = rules(modules, views, attrs)
    attrs = Enum.reduce(rules, attrs, &add_rule(&2, &1))
    attrs = tuples(modules, views, attrs)

    views = Map.new(views, fn {mod, view} -> {mod, finish_view(mod, view, attrs)} end)
    check_tombstones!(views)
    check_final!(attrs)

    %__MODULE__{
      attrs: attrs,
      views: views,
      rules: rules,
      vocabularies: vocabularies,
      schema: Map.new(attrs, fn {attr, spec} -> {attr, store_spec(spec)} end)
    }
  end

  ## ------------------------------------------------------------ questions

  @doc "The view of a schema module, or `ArgumentError` for one the Repo does not have."
  def view(%__MODULE__{views: views}, schema) do
    case views do
      %{^schema => view} ->
        view

      _ ->
        raise ArgumentError,
              "Ecto.Adapters.Hosi will not use #{inspect(schema)}: it is not in the Repo's " <>
                ":schemas, so its types and rules (unique values, delete rules) are not known. " <>
                "Add it to :schemas. (ecto_hosi's limit, not hosigraph's)"
    end
  end

  @doc "Does a record (`%{attribute => values}`) show up in the view?"
  def member?(view, entity) do
    Enum.all?(view.where, fn
      {attr, :present} -> Map.get(entity, attr, []) != []
      {attr, {:eq, value}} -> value in Map.get(entity, attr, [])
    end)
  end

  @doc "`[{attribute, value}]` of the record types a record has, in order."
  def record_types(%__MODULE__{attrs: attrs}, entity) do
    for {attr, values} <- Enum.sort(entity),
        match?(%{record_type: true}, Map.get(attrs, attr)),
        value <- values,
        do: {attr, value}
  end

  @doc "The views that say a record has all of these types (so a delete may go through them)."
  def views_of(%__MODULE__{views: views}, types) do
    for {_, view} <- Enum.sort(views), types != [], Enum.all?(types, &(&1 in view.record_types)), do: view
  end

  @doc "The tombstone a delete of a record with these types leaves, or nil."
  def tombstone(model, types) do
    model |> views_of(types) |> Enum.find_value(& &1.tombstone)
  end

  @doc "Whether a record is already a tombstone: it holds what some tombstone sets."
  def tombstoned?(%__MODULE__{views: views} = model, entity) do
    types = record_types(model, entity)

    Enum.any?(views, fn {_, view} ->
      case view.tombstone do
        nil ->
          false

        tombstone ->
          marks = for {attr, v} <- tombstone.set, record_type?(model, attr), do: {attr, v}
          marks != [] and Enum.all?(marks, &(&1 in types))
      end
    end)
  end

  def record_type?(%__MODULE__{attrs: attrs}, attr),
    do: match?(%{record_type: true}, Map.get(attrs, attr))

  @doc "The attributes that the adapter or hosigraph makes from others: never a view's own."
  def made?(%__MODULE__{attrs: attrs}, attr),
    do: match?(%{made: made} when made != nil, Map.get(attrs, attr))

  @doc "The attributes that refer to records and say what a delete does."
  def delete_rules(%__MODULE__{attrs: attrs}) do
    for {attr, %{on_delete: action} = spec} <- Enum.sort(attrs),
        action in [:cascade, :nilify, :restrict],
        do: {attr, spec}
  end

  @doc "The constraint name of a unique rule of one attribute, for a write through `view`."
  def unique_name(%__MODULE__{rules: rules}, view, attr) do
    case Enum.find(rules, &(&1.attr == attr)) do
      nil -> "#{view.source}_#{attr}_index"
      rule -> rule.name
    end
  end

  ## ------------------------------------------------------------ views

  defp view!(module) do
    Code.ensure_compiled!(module)

    unless function_exported?(module, :__schema__, 1) do
      raise ArgumentError, "#{inspect(module)} in :schemas is not an Ecto schema"
    end

    unless function_exported?(module, :__hosi__, 1) do
      raise ArgumentError,
            "#{inspect(module)}: every schema of the Repo is a view of facts. Add " <>
              "`use Ecto.Hosi.View` and say which facts make a record show up (hosi_where) and " <>
              "who owns the records (hosi_owner)"
    end

    unless module.__schema__(:primary_key) == [:id] and
             Ecto.Type.type(module.__schema__(:type, :id)) == :id do
      raise ArgumentError,
            "#{inspect(module)}: Ecto.Adapters.Hosi needs an integer primary key named :id " <>
              "(it is the hosigraph entity id: hosigraph's limit)"
    end
  end

  # the fields and their attributes; the rest of a view needs the attributes (`finish_view`)
  defp view_of(module) do
    fields =
      for field <- module.__schema__(:fields), field != :id do
        {field, module.__schema__(:field_source, field)}
      end

    for {_field, attr} <- fields, do: check_name!(module, attr)

    case fields |> Enum.map(&elem(&1, 1)) |> then(&(&1 -- Enum.uniq(&1))) do
      [] -> :ok
      [twice | _] -> raise ArgumentError, "#{inspect(module)}: two fields are the attribute #{kw(twice)}"
    end

    %{
      module: module,
      source: module.__schema__(:source),
      attr_of: Map.new(fields),
      field_of: Map.new(fields, fn {field, attr} -> {attr, field} end),
      types: Map.new(fields, fn {field, attr} -> {attr, module.__schema__(:type, field)} end)
    }
  end

  defp check_name!(module, attr) do
    text = Atom.to_string(attr)

    cond do
      not (text =~ @name) ->
        raise ArgumentError,
              "#{inspect(module)}: #{inspect(attr)} cannot be an attribute name: letters, digits " <>
                "and _ . * + ! ? $ % & = < > -, with at most one / (hosigraph's limit)"

      String.starts_with?(text, "db/") or String.starts_with?(text, "db.") ->
        raise ArgumentError, "#{inspect(module)}: db/ names are Datalevin's own (#{kw(attr)})"

      String.starts_with?(text, "hosi/") and attr != @owner ->
        raise ArgumentError,
              "#{inspect(module)}: hosi/ names are for the facts the adapter writes; a view can " <>
                "read only hosi/owner (got #{kw(attr)})"

      true ->
        :ok
    end
  end

  # the attribute of a field named by a macro of the view
  defp attr!(module, view, field, macro) do
    case view.attr_of do
      %{^field => attr} ->
        attr

      _ ->
        raise ArgumentError,
              "#{inspect(module)}: #{macro} names #{inspect(field)}, which is not a field of the view"
    end
  end

  defp finish_view(module, view, attrs) do
    where =
      for {field, condition} <- module.__hosi__(:where) do
        attr = attr!(module, view, field, "hosi_where")

        case condition do
          :present ->
            {attr, :present}

          {:eq, value} ->
            stored = dump!(module, view.types[attr], attrs[attr], field, value)
            check_default!(module, attrs[attr], field, value)
            {attr, {:eq, stored}}
        end
      end

    if where == [] do
      raise ArgumentError,
            "#{inspect(module)}: say which facts make a record show up in this view: " <>
              "hosi_where kind: \"note\" (a field that holds a value) or hosi_where :field " <>
              "(a field that has to be there)"
    end

    record_types =
      for {attr, {:eq, v}} <- where, attrs[attr].record_type, do: {attr, v}

    view
    |> Map.put(:where, where)
    |> Map.put(:record_types, Enum.sort(record_types))
    |> Map.put(:owner, view_owner(module, view, attrs))
    |> then(&Map.put(&1, :tombstone, view_tombstone(module, &1, attrs)))
  end

  # A new struct says what hosi_where writes: the field's default is that value.
  defp check_default!(_module, %{card: :many}, _field, _value), do: :ok

  defp check_default!(module, _spec, field, value) do
    type = module.__schema__(:type, field)
    default = Map.get(module.__struct__(), field)

    unless default != nil and Ecto.Type.cast(type, default) == Ecto.Type.cast(type, value) do
      raise ArgumentError,
            "#{inspect(module)}: hosi_where #{field}: #{inspect(value)} is written for every " <>
              "record made through this view, so give the field the same default: " <>
              "field #{inspect(field)}, #{inspect(type)}, default: #{inspect(value)}"
    end
  end

  defp view_owner(module, view, attrs) do
    case module.__hosi__(:owner) do
      nil ->
        raise ArgumentError,
              "#{inspect(module)}: say who owns the records made through this view, so that " <>
                "erasing an account takes what is its own: hosi_owner :field (the maker's id), " <>
                "or hosi_owner :nobody"

      :nobody ->
        :nobody

      field ->
        attr = attr!(module, view, field, "hosi_owner")

        unless attrs[attr].enc == :integer and attrs[attr].card == :one do
          raise ArgumentError,
                "#{inspect(module)}: hosi_owner #{inspect(field)} needs a field that holds the " <>
                  "id of one record (a reference or an integer)"
        end

        attr
    end
  end

  defp view_tombstone(module, view, attrs) do
    case module.__hosi__(:tombstone) do
      nil ->
        nil

      opts ->
        case Keyword.keys(opts) -- [:keep, :set, :mark] do
          [] -> :ok
          unknown -> raise ArgumentError, "#{inspect(module)}: hosi_tombstone: unknown #{inspect(unknown)}"
        end

        if view.record_types == [] do
          raise ArgumentError,
                "#{inspect(module)}: hosi_tombstone needs a view of a record type: a field with " <>
                  "record_type (hosi_record_type, or in the vocabulary) that hosi_where gives a value"
        end

        keep =
          for field <- List.wrap(opts[:keep]),
              into: MapSet.new(),
              do: attr!(module, view, field, "hosi_tombstone keep:")

        set =
          for {field, value} <- opts[:set] || [] do
            attr = attr!(module, view, field, "hosi_tombstone set:")
            {attr, dump!(module, view.types[attr], attrs[attr], field, value)}
          end

        leaves? =
          Enum.any?(set, fn {attr, v} ->
            Enum.any?(view.record_types, fn {a, was} -> a == attr and was != v end)
          end)

        unless leaves? do
          raise ArgumentError,
                "#{inspect(module)}: hosi_tombstone set: must change the record type that " <>
                  "hosi_where asks for (#{Enum.map_join(view.record_types, ", ", fn {a, v} -> "#{kw(a)} #{inspect(v)}" end)}), " <>
                  "or the tombstone would still show up in the view"
        end

        mark =
          with field when field != nil <- opts[:mark] do
            attr = attr!(module, view, field, "hosi_tombstone mark:")

            unless attrs[attr].enc == :datetime and attrs[attr].card == :one do
              raise ArgumentError,
                    "#{inspect(module)}: hosi_tombstone mark: #{inspect(field)} needs a datetime field"
            end

            {attr, view.types[attr]}
          end

        %{keep: keep, set: Enum.sort(set), mark: mark}
    end
  end

  # the value as the adapter holds it (a datetime as microseconds, a keyword as its string)
  defp dump!(module, type, spec, field, value) do
    type = if spec.card == :many, do: elem(type, 1), else: type

    with {:ok, cast} <- Ecto.Type.cast(type, value),
         {:ok, dumped} <- Ecto.Type.adapter_dump(Ecto.Adapters.Hosi, type, cast) do
      if spec.keyword && not Map.has_key?(spec.keyword, dumped) do
        raise ArgumentError,
              "#{inspect(module)}: #{inspect(value)} is not one of the values of #{kw(spec.attr)} " <>
                inspect(Map.keys(spec.keyword))
      end

      dumped
    else
      _ ->
        raise ArgumentError,
              "#{inspect(module)}: #{inspect(value)} is not a value of the field #{inspect(field)}"
    end
  end

  defp check_tombstones!(views) do
    views
    |> Map.values()
    |> Enum.filter(& &1.tombstone)
    |> Enum.group_by(& &1.record_types)
    |> Enum.each(fn {types, group} ->
      if group |> Enum.map(& &1.tombstone) |> Enum.uniq() |> length() > 1 do
        raise ArgumentError,
              "two views declare different tombstones for #{inspect(types)}: " <>
                "#{Enum.map_join(group, " and ", &inspect(&1.module))}. A tombstone belongs to " <>
                "the record type: declare it in one of them"
      end
    end)
  end

  ## ------------------------------------------------------------ declarations of attributes

  defp declaration(attr, by, enc, card, ecto, rules) do
    Map.merge(
      %{attr: attr, by: by, enc: enc, card: card, ecto: ecto},
      Map.merge(Map.new(@spec_keys, &{&1, nil}), Map.new(rules))
    )
  end

  defp owner_declaration do
    declaration(@owner, "the adapter", :integer, :one, :id, ref: true, final: true)
  end

  defp vocabulary_declarations(vocabulary) do
    for {name, %{attr: attr, type: type, opts: opts}} <- vocabulary.__hosi_vocabulary__().attributes do
      by = "#{inspect(vocabulary)}.#{name}"
      card = if opts[:many], do: :many, else: :one

      {ecto, extra} =
        case type do
          :ref -> {:id, [ref: true, on_delete: opts[:on_delete], fk_name: opts[:name]]}
          :keyword -> {:string, [keyword: keyword_values!(by, :string, opts[:values])]}
          type -> {type, []}
        end

      rules =
        [
          unique: opts[:unique] || nil,
          final: opts[:final] || nil,
          file: opts[:file] || nil,
          record_type: opts[:record_type] || nil
        ] ++ extra

      declaration(attr, by, encoding!(by, ecto), card, ecto, rules)
    end
  end

  defp declarations(module, view) do
    h = &module.__hosi__/1
    many = field_set(module, view, h.(:many), "hosi_many")
    final = field_set(module, view, h.(:final), "hosi_final")
    file = field_set(module, view, h.(:file), "hosi_file")
    kind = field_set(module, view, h.(:record_type), "hosi_record_type")
    unique = field_set(module, view, h.(:unique), "hosi_unique")

    keywords =
      Map.new(h.(:keywords), fn {field, values} ->
        {attr!(module, view, field, "hosi_keyword"), values}
      end)

    belongs =
      for name <- module.__schema__(:associations),
          %Ecto.Association.BelongsTo{owner_key: key} <- [module.__schema__(:association, name)],
          into: MapSet.new(),
          do: module.__schema__(:field_source, key)

    refs =
      Map.new(h.(:refs), fn {field, opts} ->
        attr = attr!(module, view, field, "hosi_ref")

        case Keyword.keys(opts) -- [:on_delete, :name] do
          [] ->
            :ok

          unknown ->
            raise ArgumentError,
                  "#{inspect(module)}: hosi_ref #{inspect(field)}: unknown #{inspect(unknown)} " <>
                    "(a rule holds for whatever the reference points at: there is no to:)"
        end

        if opts[:on_delete] not in [nil, :cascade, :nilify, :restrict, :keep] do
          raise ArgumentError,
                "#{inspect(module)}: hosi_ref #{inspect(field)}: on_delete: is :cascade, " <>
                  ":nilify, :restrict or :keep"
        end

        {attr, opts}
      end)

    from_vocabulary =
      Map.new(h.(:vocabulary), fn {field, vocabulary, name} ->
        {view.attr_of[field], vocabulary.__hosi_vocabulary__().attributes[name]}
      end)

    for {field, attr} <- view.attr_of do
      type = view.types[attr]
      by = "#{inspect(module)}.#{field}"
      vocab = from_vocabulary[attr]
      many? = MapSet.member?(many, attr) or (vocab != nil and vocab.opts[:many] == true)

      {ecto, card} =
        case {many?, type} do
          {false, type} -> {type, :one}
          {true, {:array, inner}} -> {inner, :many}
          {true, _} -> raise ArgumentError, "#{by}: many values need a {:array, type} field"
        end

      ref? = MapSet.member?(belongs, attr) or Map.has_key?(refs, attr)
      enc = encoding!(by, ecto)

      if ref? and enc != :integer do
        raise ArgumentError, "#{by}: a reference needs a field that holds an id"
      end

      keyword =
        case keywords do
          %{^attr => values} -> keyword_values!(by, ecto, values)
          _ -> nil
        end

      ref_opts = Map.get(refs, attr, [])

      declaration(attr, by, enc, card, ecto,
        unique: MapSet.member?(unique, attr) || nil,
        final: MapSet.member?(final, attr) || nil,
        file: MapSet.member?(file, attr) || nil,
        record_type: MapSet.member?(kind, attr) || nil,
        ref: ref? || nil,
        keyword: keyword,
        on_delete: ref_opts[:on_delete],
        fk_name: ref_opts[:name]
      )
    end
  end

  defp field_set(module, view, fields, macro),
    do: MapSet.new(fields, &attr!(module, view, &1, macro))

  # what a keyword may look like in EDN without being quoted
  @keyword ~r/\A[A-Za-z][A-Za-z0-9_.\-]*\z/

  defp keyword_values!(by, type, values) do
    unless type == :string do
      raise ArgumentError,
            "#{by}: a keyword needs a :string field (Elixir keeps seeing the string, " <>
              "hosigraph keeps the keyword)"
    end

    unless is_list(values) and values != [] and
             Enum.all?(values, &(is_binary(&1) and &1 =~ @keyword)) and values == Enum.uniq(values) do
      raise ArgumentError,
            "#{by}: a keyword takes a list of distinct strings that are keyword names (letters, " <>
              "digits, _ - .; no / or spaces), got #{inspect(values)}"
    end

    Map.new(values, &{&1, String.to_atom(&1)})
  end

  # How the values of a type are kept: two declarations of one attribute must keep them alike.
  defp encoding!(by, type) do
    case Ecto.Type.type(type) do
      {:array, inner} -> {:array, encoding!(by, inner)}
      t when t in [:id, :integer] -> :integer
      t when t in [:time, :time_usec] -> :time
      t when t in [:naive_datetime, :naive_datetime_usec, :utc_datetime, :utc_datetime_usec] -> :datetime
      {:map, _} -> :map
      t when t in [:string, :boolean, :float, :decimal, :date, :map, :binary, :uuid] -> t
      other ->
        raise ArgumentError,
              "#{by}: Ecto.Adapters.Hosi does not support the Ecto type #{inspect(type)} " <>
                "(primitive #{inspect(other)}) yet (ecto_hosi's limit)"
    end
  end

  defp merge({attr, [first | rest] = declarations}) do
    spec =
      Enum.reduce(rest, first, fn next, acc ->
        if next.enc != acc.enc or next.card != acc.card do
          conflict!(attr, declarations, "#{say(acc)} and #{say(next)}")
        end

        Enum.reduce(@spec_keys, acc, fn key, acc ->
          case {Map.fetch!(acc, key), Map.fetch!(next, key)} do
            {_, nil} -> acc
            {nil, value} -> Map.put(acc, key, value)
            {same, same} -> acc
            {one, other} -> conflict!(attr, declarations, "#{key} #{inspect(one)} and #{inspect(other)}")
          end
        end)
      end)

    hosi_type =
      cond do
        spec.ref -> :ref
        spec.keyword -> :keyword
        true -> Types.hosi_type(spec.enc)
      end

    spec =
      spec
      |> Map.delete(:by)
      |> Map.merge(%{type: hosi_type, made: nil, tuple: nil})
      |> Map.update!(:unique, &(&1 == true))
      |> Map.update!(:final, &(&1 == true))
      |> Map.update!(:file, &(&1 == true))
      |> Map.update!(:record_type, &(&1 == true))
      |> Map.update!(:ref, &(&1 == true))

    if spec.record_type and spec.card == :many do
      conflict!(attr, declarations, "a record type holds one value")
    end

    {attr, spec}
  end

  defp say(declaration) do
    many = if declaration.card == :many, do: "many ", else: ""
    "#{many}#{inspect(declaration.enc)}"
  end

  defp conflict!(attr, declarations, what) do
    raise ArgumentError,
          "the attribute #{kw(attr)} is declared in two ways: #{what} " <>
            "(#{Enum.map_join(declarations, ", ", & &1.by)}). A name has one definition: declare " <>
            "it once, in a vocabulary, or give the two meanings two names"
  end

  defp check_prefixes!(vocabularies, views) do
    vocabularies
    |> Enum.group_by(& &1.__hosi_vocabulary__().prefix)
    |> Enum.each(fn
      {_, [_]} ->
        :ok

      {prefix, several} ->
        raise ArgumentError,
              "two vocabularies have the prefix #{inspect(prefix)}: #{Enum.map_join(several, ", ", &inspect/1)}"
    end)

    for vocabulary <- vocabularies,
        %{prefix: prefix, attributes: attributes} = vocabulary.__hosi_vocabulary__(),
        own = MapSet.new(attributes, fn {_, a} -> a.attr end),
        {module, view} <- views,
        {field, attr} <- view.attr_of,
        String.starts_with?(Atom.to_string(attr), prefix <> "/"),
        not MapSet.member?(own, attr) do
      raise ArgumentError,
            "#{inspect(module)}.#{field}: #{inspect(vocabulary)} owns the prefix #{prefix}/ and " <>
              "has no attribute #{kw(attr)}. Declare it there, or use another name"
    end
  end

  defp check_final!(attrs) do
    for {attr, %{final: true, on_delete: :nilify}} <- attrs do
      raise ArgumentError,
            "#{kw(attr)} is final and on_delete: :nilify: a delete could not remove it"
    end
  end

  ## ------------------------------------------------------------ rules over several attributes

  defp rules(modules, views, attrs) do
    modules
    |> Enum.flat_map(&Ecto.Hosi.Derived.specs(&1, views[&1]))
    |> Enum.group_by(& &1.attr)
    |> Enum.sort()
    |> Enum.map(fn {attr, same} ->
      case Enum.uniq(same) do
        [rule] ->
          Enum.each(rule.attrs, fn a ->
            if attrs[a].card == :many do
              raise ArgumentError, "#{rule.name}: a unique rule over a field with many values is not supported"
            end
          end)

          rule

        _ ->
          raise ArgumentError,
                "two different unique rules are named #{kw(attr)}: give one of them another name:"
      end
    end)
  end

  defp add_rule(attrs, rule) do
    spec =
      if rule.native,
        do: %{type: nil, enc: nil, card: :one, tuple: rule.attrs, made: :tuple},
        else: %{type: :string, enc: :string, card: :one, tuple: nil, made: :derived}

    Map.put(attrs, rule.attr, made_spec(rule.attr, Map.put(spec, :unique, true)))
  end

  defp tuples(modules, views, attrs) do
    for module <- modules, {key, order} <- module.__hosi__(:tuples), reduce: attrs do
      attrs ->
        view = views[module]
        key_attr = attr!(module, view, key, "hosi_tuple")
        order_attr = attr!(module, view, order, "hosi_tuple")

        unless attrs[order_attr].type == :long and attrs[order_attr].card == :one and
                 attrs[key_attr].card == :one do
          raise ArgumentError,
                "#{inspect(module)}: hosi_tuple #{inspect(key)}, #{inspect(order)}: the order " <>
                  "must be an integer, a date or a datetime, and neither has many values"
        end

        attr = tuple_attr(key_attr, order_attr)

        Map.put(
          attrs,
          attr,
          made_spec(attr, %{type: nil, enc: nil, card: :one, tuple: [key_attr, order_attr], made: :index})
        )
    end
  end

  @doc "The attribute of the `(key, order)` index: `:\"hosi/account_id+ap.published\"`."
  def tuple_attr(key, order) do
    flat = &(&1 |> Atom.to_string() |> String.replace("/", "."))
    String.to_atom("hosi/#{flat.(key)}+#{flat.(order)}")
  end

  defp made_spec(attr, fields) do
    Map.merge(
      %{
        attr: attr,
        ecto: nil,
        unique: false,
        final: false,
        file: false,
        record_type: false,
        ref: false,
        keyword: nil,
        on_delete: nil,
        fk_name: nil
      },
      fields
    )
  end

  defp store_spec(%{tuple: nil} = spec) do
    %{type: spec.type}
    |> put_if(spec.card == :many, :card, :many)
    |> put_if(spec.unique, :unique, :value)
  end

  defp store_spec(%{tuple: parts} = spec),
    do: put_if(%{tuple: parts}, spec.unique, :unique, :value)

  defp put_if(map, true, key, value), do: Map.put(map, key, value)
  defp put_if(map, false, _key, _value), do: map

  ## ------------------------------------------------------------ warnings

  @doc """
  The references that say nothing about a delete of what they point at, and so are left pointing
  at nothing: `[{attribute, [view]}]`. A parent's `has_many .., on_delete:` counts as saying it,
  and a `belongs_to` of a parent that is not one of the Repo's views is left alone.
  """
  def undeclared(%__MODULE__{attrs: attrs, views: views}) do
    for {attr, %{ref: true, on_delete: nil}} <- Enum.sort(attrs), attr != @owner do
      holders = for {mod, view} <- Enum.sort(views), Map.has_key?(view.field_of, attr), do: mod

      unsaid = Enum.filter(holders, &unsaid?(&1, views[&1].field_of[attr], views))
      if holders == [] or unsaid != [], do: {attr, unsaid}
    end
    |> Enum.reject(&is_nil/1)
  end

  # does the view `module` leave open what a delete does to its reference `field`?
  defp unsaid?(module, field, views) do
    parents =
      for name <- module.__schema__(:associations),
          %Ecto.Association.BelongsTo{owner_key: ^field, related: parent} <-
            [module.__schema__(:association, name)],
          do: parent

    case Enum.filter(parents, &Map.has_key?(views, &1)) do
      # a reference to any record (hosi_ref, a vocabulary): nothing else can say it
      [] -> parents == []
      listed -> not Enum.all?(listed, &said_by?(&1, module, field))
    end
  end

  defp said_by?(parent, module, field) do
    Enum.any?(parent.__schema__(:associations), fn name ->
      match?(
        %Ecto.Association.Has{related: ^module, related_key: ^field, on_delete: on_delete}
        when on_delete != :nothing,
        parent.__schema__(:association, name)
      )
    end)
  end
end
