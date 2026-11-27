defmodule Ecto.Hosi.Schema do
  @moduledoc """
  Optional helper for Ecto schemas stored in hosigraph.

      defmodule MyApp.User do
        use Ecto.Schema
        use Ecto.Hosi.Schema

        hosi_unique :email       # hosigraph refuses a second user with this email

        schema "user" do
          field :email, :string
        end
      end

  Without `hosi_unique` the schema works the same, just with no uniqueness in the store.
  `build/1` turns a list of schemas into the `schema:` map hosigraph is opened with.
  """

  alias Ecto.Hosi.{Attr, Types}

  defmacro __using__(_opts) do
    quote do
      import Ecto.Hosi.Schema,
        only: [hosi_unique: 1, hosi_unique: 2, hosi_tuple: 2, hosi_ref: 2, hosi_keyword: 2, hosi_append_only: 0]
      Module.register_attribute(__MODULE__, :hosi_unique, accumulate: true)
      Module.register_attribute(__MODULE__, :hosi_uniques, accumulate: true)
      Module.register_attribute(__MODULE__, :hosi_tuple, accumulate: true)
      Module.register_attribute(__MODULE__, :hosi_ref, accumulate: true)
      Module.register_attribute(__MODULE__, :hosi_keyword, accumulate: true)
      Module.register_attribute(__MODULE__, :hosi_append_only, accumulate: false)
      @before_compile Ecto.Hosi.Schema
    end
  end

  @doc """
  Declare a unique rule. A bare field is unique on its own, and hosigraph refuses a second row:

      hosi_unique :email

  A list of two or more fields is unique as a combination (like a composite unique index), and
  options say the rest of what an index can say:

      hosi_unique [:account_id, :note_id]
      hosi_unique :username, where: [null: [:domain]], name: "accounts_username_index"
      hosi_unique :email, lower: [:email], where: [null: [:domain], not_null: [:email]]

    * `:name` – the constraint name `unique_constraint/3` expects (default `source_firstfield_index`)
    * `:lower` – fields compared lower-cased (`lower(email)`)
    * `:where` – the rule holds only for rows where `null: [fields]` are nil, `not_null: [fields]` are
      not, and `eq: [field: value]` are equal (a partial index)
    * `:tuple` – `true` keeps the combination as a hosigraph tuple (a Datalevin composite tuple) with
      `unique`, instead of a JSON text in a derived attribute: `[?e :follow/uq-... [12 34]]` finds a
      pair by its value, and the khana keeps it up to date. Only for a plain combination of two or
      more fields (no `:lower`, no `:where`), and **every field must be set**: a tuple's nil is a
      value, not SQL's NULL, so two rows `(1, nil)` would collide. A write that leaves a field nil
      raises `ArgumentError`.

  A row with a nil in one of the fields is never in conflict (SQL's NULLs are distinct). The store
  keeps a derived attribute for each such rule, so the guard is hosigraph's own, race included.
  """
  defmacro hosi_unique(fields, opts \\ []) do
    quote do
      case {unquote(fields), unquote(opts)} do
        {f, []} when is_atom(f) ->
          Module.put_attribute(__MODULE__, :hosi_unique, [f])

        {f, o} ->
          Module.put_attribute(__MODULE__, :hosi_uniques, {List.wrap(f), o})
      end
    end
  end

  @doc """
  Declare a `(key, order)` index that `Repo.highest_in/5` walks: the newest rows of some keys.

      hosi_tuple :account_id, :created_at     # the newest notes of some authors

  `key` has a type (a `belongs_to` key, an integer, a string); `order` must be an integer or a
  datetime (it is stored as a long). hosigraph keeps the index itself: it is never written.
  """
  defmacro hosi_tuple(key, order) do
    quote do
      Module.put_attribute(__MODULE__, :hosi_tuple, {unquote(key), unquote(order)})
    end
  end

  @doc """
  Declare what deleting the row a foreign key points at does to this row (`ON DELETE`):

      hosi_ref :account_id, to: "accounts", on_delete: :cascade

    * `:to` – the parent's table name, or its schema module
    * `:on_delete` – `:cascade` (delete this row too), `:nilify` (set the key to nil), or
      `:restrict` / `:none` (refuse the delete; `foreign_key_constraint/3` turns it into an error)
    * `:name` – the constraint name (default `source_field_fkey`)

  hosigraph has no foreign keys, so the adapter does this on every delete (`Repo.delete`,
  `delete_all`, a `write_batch` delete step) in the same atomic write as the delete itself.
  """
  defmacro hosi_ref(field, opts) do
    quote do
      Module.put_attribute(__MODULE__, :hosi_ref, {unquote(field), unquote(opts)})
    end
  end

  @doc """
  Keep a string field as a keyword in hosigraph: Elixir still sees `"public"`, the store holds
  `:public`, so Datalog reads `[?n :note/visibility :public]`.

      hosi_keyword :visibility, ["public", "followers", "direct"]

  The values are a closed list: the strings that may be written (a changeset's `validate_inclusion`
  can say the same list). Another string raises `ArgumentError` before anything is written. A name
  is letters, digits, `_`, `-` and `.`, starting with a letter. The field stays a `:string` in Ecto.
  """
  defmacro hosi_keyword(field, values) do
    quote do
      Module.put_attribute(__MODULE__, :hosi_keyword, {unquote(field), unquote(values)})
    end
  end

  @doc "Rows of this schema are written once: the adapter refuses to update or delete them."
  defmacro hosi_append_only do
    quote do
      Module.put_attribute(__MODULE__, :hosi_append_only, true)
    end
  end

  defmacro __before_compile__(env) do
    uniques = env.module |> Module.get_attribute(:hosi_unique) |> List.flatten() |> Enum.uniq()
    tuples = env.module |> Module.get_attribute(:hosi_tuple) |> Enum.reverse()
    uniques_x = env.module |> Module.get_attribute(:hosi_uniques) |> Enum.reverse()
    refs = env.module |> Module.get_attribute(:hosi_ref) |> Enum.reverse()
    keywords = env.module |> Module.get_attribute(:hosi_keyword) |> Enum.reverse()
    append_only = Module.get_attribute(env.module, :hosi_append_only) == true

    quote do
      @doc false
      def __hosi__(:unique), do: unquote(uniques)
      def __hosi__(:tuples), do: unquote(Macro.escape(tuples))
      def __hosi__(:uniques), do: unquote(Macro.escape(uniques_x))
      def __hosi__(:refs), do: unquote(Macro.escape(refs))
      def __hosi__(:keywords), do: unquote(Macro.escape(keywords))
      def __hosi__(:append_only), do: unquote(append_only)
    end
  end

  @doc """
  The hosigraph schema for these Ecto schemas: `%{attr => %{type: .., unique: ..}}`.

  `belongs_to` foreign keys become `:ref` attributes; the primary key is the entity id and has no attribute.
  """
  def build(schemas) do
    Enum.reduce(schemas, %{}, fn mod, acc -> Map.merge(acc, build_one(mod)) end)
  end

  defp build_one(mod) do
    Code.ensure_compiled!(mod)
    source = mod.__schema__(:source)

    unless mod.__schema__(:primary_key) == [:id] and Ecto.Type.type(mod.__schema__(:type, :id)) == :id do
      raise ArgumentError,
            "#{inspect(mod)}: Ecto.Adapters.Hosi needs an integer primary key named :id " <>
              "(it is the hosigraph entity id)"
    end

    fks =
      for name <- mod.__schema__(:associations),
          %Ecto.Association.BelongsTo{owner_key: key} <- [mod.__schema__(:association, name)],
          do: key

    uniques = if function_exported?(mod, :__hosi__, 1), do: mod.__hosi__(:unique), else: []
    keywords = if function_exported?(mod, :__hosi__, 1), do: Keyword.keys(mod.__hosi__(:keywords)), else: []

    fields =
      for field <- mod.__schema__(:fields), field != :id, into: %{} do
        type =
          cond do
            field in fks -> :ref
            field in keywords -> :keyword
            true -> Types.hosi_type(mod.__schema__(:type, field))
          end

        spec = %{type: type}
        spec = if field in uniques, do: Map.put(spec, :unique, :value), else: spec
        {Attr.attr(source, field), spec}
      end

    tuples = if function_exported?(mod, :__hosi__, 1), do: mod.__hosi__(:tuples), else: []

    fields =
      for {key, order} <- tuples, into: fields do
        {Attr.tuple(source, key, order), %{tuple: [Attr.attr(source, key), Attr.attr(source, order)]}}
      end

    fields =
      for spec <- Ecto.Hosi.Derived.specs(mod), into: fields do
        # a rule that is a plain combination can be hosigraph's own tuple; the others are a JSON text
        if spec.native,
          do: {spec.attr, %{tuple: spec.attrs, unique: :value}},
          else: {spec.attr, %{type: :string, unique: :value}}
      end

    Map.put(fields, Attr.marker(source), %{type: :boolean})
  end
end
