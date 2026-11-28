defmodule Ecto.Hosi.View do
  @moduledoc """
  An Ecto schema as a **view of facts**.

      defmodule MyApp.Note do
        use Ecto.Schema
        use Ecto.Hosi.View

        hosi_where type: "note"                 # a record is a note when ap/type is :note
        hosi_owner :attributed_to_id            # its maker: erased with that account
        hosi_tombstone keep: [:attributed_to_id],
                       set: [type: "tombstone", former_type: "note"],
                       mark: :deleted_at

        schema "note" do                        # Ecto needs a name; it adds nothing to attributes
          hosi_attributes MyApp.ActivityPub,
            type: :type, former_type: :former_type, uri: :uri,
            attributed_to_id: :attributed_to, deleted_at: :deleted

          field :content, :string               # the attribute `content`
        end
      end

  hosigraph keeps **facts**, `[record attribute value]`. It has no tables. A view says which
  facts make a record show up (`hosi_where/1`), and which attributes its fields read and write:

    * a field is the attribute of the same name, or the one `source:` names
      (`field :path, :string, source: :"s3/path"`), or an attribute of an `Ecto.Hosi.Vocabulary`
      (`hosi_attributes/2`);
    * `:id` is the internal integer of the record, which hosigraph makes;
    * one record can show up in several views.

  The name in `schema "note"` is used only for Ecto's default constraint names
  (`note_content_index`).

  Every view says which facts make a record show up (`hosi_where/1`) and who owns the records it
  makes (`hosi_owner/1`). The Repo does not start without both.

  ## Rules belong to attributes

  `hosi_unique`, `hosi_keyword`, `hosi_ref`, `hosi_final`, `hosi_many`, `hosi_file` and
  `hosi_record_type` give an attribute a rule, and the rule holds **for every record that has the
  attribute**, whichever view wrote it. A rule is declared once, in a view or in a vocabulary;
  other views of the attribute need not repeat it. The Repo does not start if two declarations of
  one attribute disagree.
  """

  @macros [
    hosi_where: 1,
    hosi_owner: 1,
    hosi_tombstone: 1,
    hosi_unique: 1,
    hosi_unique: 2,
    hosi_tuple: 2,
    hosi_ref: 1,
    hosi_ref: 2,
    hosi_keyword: 2,
    hosi_final: 1,
    hosi_many: 1,
    hosi_file: 1,
    hosi_record_type: 1,
    hosi_attributes: 2
  ]

  @accumulated [
    :hosi_where,
    :hosi_unique,
    :hosi_uniques,
    :hosi_tuple,
    :hosi_ref,
    :hosi_keyword,
    :hosi_final,
    :hosi_many,
    :hosi_file,
    :hosi_record_type,
    :hosi_vocabulary
  ]

  defmacro __using__(_opts) do
    quote do
      import Ecto.Hosi.View, only: unquote(@macros)

      for name <- unquote(@accumulated) do
        Module.register_attribute(__MODULE__, name, accumulate: true)
      end

      Module.register_attribute(__MODULE__, :hosi_owner, accumulate: true)
      Module.register_attribute(__MODULE__, :hosi_tombstone, accumulate: true)
      @before_compile Ecto.Hosi.View
    end
  end

  @doc """
  Which facts make a record show up in this view. All of them must hold:

      hosi_where type: "note"          # the field `type` holds "note"
      hosi_where :pinned_at            # the record has a value for `pinned_at`
      hosi_where [:from_id, :to_id]    # ...has both

  A record created through the view gets the facts that are given with a value (the field needs
  the same `default:`, so that a new struct says so too; `hosi_attributes/2` sets it), and must be
  given a value for each field that only has to be there.

  Declare it before the `schema` block.
  """
  defmacro hosi_where(conditions) do
    quote do
      for condition <- List.wrap(unquote(conditions)) do
        Module.put_attribute(__MODULE__, :hosi_where, Ecto.Hosi.View.__where__(__MODULE__, condition))
      end
    end
  end

  @doc false
  def __where__(_module, field) when is_atom(field) and not is_nil(field), do: {field, :present}

  def __where__(_module, {field, value}) when is_atom(field) and not is_nil(value),
    do: {field, {:eq, value}}

  def __where__(module, other) do
    raise ArgumentError,
          "#{inspect(module)}: hosi_where takes field: value, or a field that must be there; " <>
            "got #{inspect(other)}"
  end

  @doc """
  Who owns the records made through this view: their **maker**.

      hosi_owner :author_id     # the field that holds the maker's id
      hosi_owner :nobody        # nobody does (an account itself, say)

  A record made through the view gets the fact `hosi/owner`, a reference to the maker. It is set
  at creation, does not change, and is kept in a tombstone. `Repo.erase_account/2` erases by it.
  """
  defmacro hosi_owner(field) do
    quote do
      Module.put_attribute(__MODULE__, :hosi_owner, unquote(field))
    end
  end

  @doc """
  Make `Repo.delete` leave a **tombstone** instead of removing the record.

      hosi_tombstone keep: [:uri], set: [type: "tombstone", former_type: "note"], mark: :deleted_at

    * `set:` – the facts the tombstone gets. At least one of them must change a record type that
      `hosi_where/1` asks for: that is how the record leaves the view
    * `keep:` – fields whose facts stay. Final attributes, file attributes and `hosi/owner` stay
      anyway; everything else is removed
    * `mark:` – a datetime field that gets the time of the delete

  It belongs to the record type: a delete through any view of that type leaves this tombstone.
  The removed content stays in the history until the record is erased.
  """
  defmacro hosi_tombstone(opts) do
    quote do
      Module.put_attribute(__MODULE__, :hosi_tombstone, unquote(opts))
    end
  end

  @doc """
  A unique rule. A single field with no options is hosigraph's own rule on that attribute:

      hosi_unique :email

  A list of fields, or options, is a rule over those attributes, as a combined or partial index:

      hosi_unique [:account_id, :note_id]
      hosi_unique :username, where: [null: [:domain]], name: "accounts_username_index"
      hosi_unique :email, lower: [:email], where: [null: [:domain], not_null: [:email]]

    * `:name` – the constraint name `unique_constraint/3` expects
      (default: `<schema name>_<attributes>_index`, as Ecto's own default)
    * `:lower` – fields compared lower-cased
    * `:where` – the rule holds only for records where `null: [fields]` have no value,
      `not_null: [fields]` have one, and `eq: [field: value]` are equal
    * `:tuple` – `true` keeps the combination as a hosigraph tuple with `unique`, so hosigraph
      itself holds it, direct writes included. Only for a plain combination of two or more fields,
      and a record that has one of them must have all of them (a tuple's nil is a value)

  A nil never conflicts (except as above). Rules that are not a tuple or a single field are kept
  by the adapter in a fact of its own (`hosi/uq-…`): a **direct** write passes them by. Use
  `Repo.write_facts/2` for writes that should keep them.
  """
  defmacro hosi_unique(fields, opts \\ []) do
    quote do
      case {unquote(fields), unquote(opts)} do
        {f, []} when is_atom(f) -> Module.put_attribute(__MODULE__, :hosi_unique, f)
        {f, o} -> Module.put_attribute(__MODULE__, :hosi_uniques, {List.wrap(f), o})
      end
    end
  end

  @doc """
  A `(key, order)` index that `Repo.highest_in/4` walks: the newest records of some keys.

      hosi_tuple :account_id, :published_at

  `order` must be an integer, a date or a datetime. hosigraph keeps the index itself.
  """
  defmacro hosi_tuple(key, order) do
    quote do
      Module.put_attribute(__MODULE__, :hosi_tuple, {unquote(key), unquote(order)})
    end
  end

  @doc """
  The field is a **reference** to a record, any record. (A `belongs_to` key is one already.)

      hosi_ref :note_id, on_delete: :cascade

    * `:on_delete` – what deleting the record it points at does to the record that points:
      `:cascade` (delete it too, leaving its tombstone if it has one), `:nilify` (remove the
      reference), `:restrict` (refuse the delete; `foreign_key_constraint/3` turns that into a
      changeset error), or `:keep` (leave it pointing at what is gone, or at the tombstone)
    * `:name` – the constraint name (default: `<schema name>_<attribute>_fkey`)

  hosigraph has no foreign keys: the adapter does this on every delete, in the same atomic write.
  """
  defmacro hosi_ref(field, opts \\ []) do
    quote do
      Module.put_attribute(__MODULE__, :hosi_ref, {unquote(field), unquote(opts)})
    end
  end

  @doc """
  Keep a string field as a keyword in hosigraph: Elixir sees `"public"`, the store holds
  `:public`, so Datalog reads `[?n :visibility :public]`.

      hosi_keyword :visibility, ["public", "followers", "direct"]

  The values are a closed list. Another string raises `ArgumentError` before anything is written.
  """
  defmacro hosi_keyword(field, values) do
    quote do
      Module.put_attribute(__MODULE__, :hosi_keyword, {unquote(field), unquote(values)})
    end
  end

  @doc """
  The attributes of these fields are **final**: once set, a value is not changed or removed
  (erase still removes it). The adapter refuses it; a direct write is not refused.
  """
  defmacro hosi_final(fields) do
    quote do
      for field <- List.wrap(unquote(fields)), do: Module.put_attribute(__MODULE__, :hosi_final, field)
    end
  end

  @doc """
  These `{:array, type}` fields hold **many values**: one fact for each, so that Datalog and
  `where "x" in v.tags` find a record by one of them. It is a set: order and duplicates are not
  kept (read back sorted). Without this, a list is one JSON text.
  """
  defmacro hosi_many(fields) do
    quote do
      for field <- List.wrap(unquote(fields)), do: Module.put_attribute(__MODULE__, :hosi_many, field)
    end
  end

  @doc """
  These fields name a **file** outside the database (an S3 path). A record that has one is not
  removed outright: it needs a tombstone, which keeps the name, and `Repo.erase_account/2` hands
  the names to your code before it erases.
  """
  defmacro hosi_file(fields) do
    quote do
      for field <- List.wrap(unquote(fields)), do: Module.put_attribute(__MODULE__, :hosi_file, field)
    end
  end

  @doc """
  The field says **what a record is** (as `ap/type` does). An update cannot change it once set,
  and a record that has it is deleted only through a view whose `hosi_where/1` asks for its value.
  """
  defmacro hosi_record_type(field) do
    quote do
      Module.put_attribute(__MODULE__, :hosi_record_type, unquote(field))
    end
  end

  @doc """
  Fields that are attributes of a vocabulary, inside the `schema` block:

      hosi_attributes MyApp.ActivityPub, type: :type, uri: :uri, attributed_to_id: :attributed_to

  Each pair is `field: attribute`. The field gets the vocabulary's type. A pair can add Ecto field
  options: `author_id: {:attributed_to, type: MyApp.Id}` (a reference with an id type of your
  own), `visibility: {:visibility, default: "public"}`.
  """
  defmacro hosi_attributes(vocabulary, pairs) do
    quote do
      for {field, target} <- unquote(pairs) do
        {type, opts} =
          Ecto.Hosi.View.__attribute_field__(__MODULE__, unquote(vocabulary), field, target)

        Ecto.Schema.__field__(__MODULE__, field, type, opts)
      end
    end
  end

  @doc false
  def __attribute_field__(module, vocabulary, field, target) do
    {name, given} =
      case target do
        {name, given} -> {name, given}
        name -> {name, []}
      end

    Code.ensure_compiled!(vocabulary)

    unless function_exported?(vocabulary, :__hosi_vocabulary__, 0) do
      raise ArgumentError, "#{inspect(module)}: #{inspect(vocabulary)} is not an Ecto.Hosi.Vocabulary"
    end

    attributes = vocabulary.__hosi_vocabulary__().attributes

    %{attr: attr, type: type, opts: opts} =
      Keyword.get(attributes, name) ||
        raise ArgumentError,
              "#{inspect(module)}: #{inspect(vocabulary)} has no attribute #{inspect(name)}; " <>
                "it has #{inspect(Keyword.keys(attributes))}"

    {own_type, given} = Keyword.pop(given, :type)

    ecto_type =
      case {own_type, type} do
        {nil, :ref} -> :id
        {nil, :keyword} -> :string
        {nil, type} -> type
        {own, :ref} -> own
        {own, _} ->
          raise ArgumentError,
                "#{inspect(module)}: only a reference can take type: #{inspect(own)} (#{field}); " <>
                  "the vocabulary gives the type of an attribute"
      end

    ecto_type = if opts[:many], do: {:array, ecto_type}, else: ecto_type

    Module.put_attribute(module, :hosi_vocabulary, {field, vocabulary, name})

    # a value that hosi_where asks for is the field's default, so a new struct says it too
    where = module |> Module.get_attribute(:hosi_where) |> List.keyfind(field, 0)

    defaults =
      case where do
        {_, {:eq, value}} -> if opts[:many], do: [], else: [default: value]
        _ -> []
      end

    {ecto_type, defaults |> Keyword.merge(given) |> Keyword.put(:source, attr)}
  end

  defmacro __before_compile__(env) do
    get = &(env.module |> Module.get_attribute(&1) |> Enum.reverse())

    once = fn name ->
      case get.(name) do
        [] ->
          nil

        [value] ->
          value

        _ ->
          raise ArgumentError, "#{inspect(env.module)}: #{name} is declared twice"
      end
    end

    declared = %{
      view: true,
      where: get.(:hosi_where),
      owner: once.(:hosi_owner),
      tombstone: once.(:hosi_tombstone),
      unique: Enum.uniq(get.(:hosi_unique)),
      uniques: get.(:hosi_uniques),
      tuples: get.(:hosi_tuple),
      refs: get.(:hosi_ref),
      keywords: get.(:hosi_keyword),
      final: get.(:hosi_final),
      many: get.(:hosi_many),
      file: get.(:hosi_file),
      record_type: get.(:hosi_record_type),
      vocabulary: get.(:hosi_vocabulary)
    }

    for {key, value} <- declared do
      quote do
        @doc false
        def __hosi__(unquote(key)), do: unquote(Macro.escape(value))
      end
    end
  end
end
