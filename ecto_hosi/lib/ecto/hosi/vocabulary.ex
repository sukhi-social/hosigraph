defmodule Ecto.Hosi.Vocabulary do
  @moduledoc """
  Attributes that several views share, declared once.

      defmodule MyApp.ActivityPub do
        use Ecto.Hosi.Vocabulary, prefix: "ap"

        attribute :type, :keyword, values: ["note", "person", "tombstone"], record_type: true
        attribute :former_type, :keyword, values: ["note", "person"]
        attribute :uri, :string, unique: true, final: true
        attribute :attributed_to, :ref, on_delete: :keep, final: true
        attribute :alt_id, :string, unique: true, many: true
        attribute :deleted, :utc_datetime_usec
      end

  `attribute :former_type` is the attribute `ap/former-type`: the prefix, a slash, and the name
  with `-` for `_`. A view (`Ecto.Hosi.View`) gives its fields these attributes with
  `hosi_attributes/2`.

  The prefix belongs to the vocabulary: a view cannot name another attribute under it.

  ## Types

  Any Ecto type (`:string`, `:integer`, `:utc_datetime_usec`, `Ecto.UUID`, `{:array, :string}`
  kept as one JSON text, a type of your own), or:

    * `:keyword` – a string for Elixir, a keyword in hosigraph; needs `values:`, the closed list
    * `:ref` – a reference to any record (an integer id for Elixir)

  ## Rules

  The rules of an attribute hold for every record that has it, whichever view wrote it.

    * `unique: true` – no two records hold the same value (hosigraph's own rule)
    * `final: true` – once set, it is not changed or removed; erase still removes it
    * `many: true` – a record holds a set of values: order and duplicates are not kept
    * `record_type: true` – says what a record is (`ap/type`): an update cannot change it, and a
      delete goes through a view of that type
    * `file: true` – names a file outside the database: such a record is not removed outright,
      and its tombstone keeps the name
    * `on_delete:` (a `:ref`) – what deleting the record it points at does to this one:
      `:cascade`, `:nilify`, `:restrict` or `:keep`
    * `name:` (a `:ref`) – the constraint name of a `:restrict`
  """

  @rules [:values, :unique, :final, :many, :record_type, :file, :on_delete, :name]
  @on_delete [:cascade, :nilify, :restrict, :keep]

  defmacro __using__(opts) do
    quote do
      import Ecto.Hosi.Vocabulary, only: [attribute: 2, attribute: 3]
      Module.register_attribute(__MODULE__, :hosi_attribute, accumulate: true)
      @hosi_prefix Ecto.Hosi.Vocabulary.__prefix__(__MODULE__, unquote(opts))
      @before_compile Ecto.Hosi.Vocabulary
    end
  end

  @doc "Declare an attribute of the vocabulary. See the module for the types and the rules."
  defmacro attribute(name, type, opts \\ []) do
    quote do
      Ecto.Hosi.Vocabulary.__attribute__(__MODULE__, unquote(name), unquote(type), unquote(opts))
    end
  end

  @doc false
  def __prefix__(module, opts) do
    prefix = Keyword.get(opts, :prefix)

    unless is_binary(prefix) and prefix =~ ~r/\A[A-Za-z][A-Za-z0-9_.\-]*\z/ and
             prefix not in ["hosi", "db"] do
      raise ArgumentError,
            "#{inspect(module)}: use Ecto.Hosi.Vocabulary, prefix: \"ap\" – a short name of " <>
              "letters, digits, _ . -, not \"hosi\" or \"db\" (got #{inspect(prefix)})"
    end

    prefix
  end

  @doc false
  def __attribute__(module, name, type, opts) do
    here = "#{inspect(module)}: attribute #{inspect(name)}"

    unless is_atom(name) and Atom.to_string(name) =~ ~r/\A[a-z][a-z0-9_]*\z/ do
      raise ArgumentError, "#{here}: a name is lower-case letters, digits and _"
    end

    case Keyword.keys(opts) -- @rules do
      [] -> :ok
      unknown -> raise ArgumentError, "#{here}: unknown option(s) #{inspect(unknown)}"
    end

    if type == :keyword != Keyword.has_key?(opts, :values) do
      raise ArgumentError, "#{here}: values: goes with the type :keyword, and :keyword needs it"
    end

    if (Keyword.has_key?(opts, :on_delete) or Keyword.has_key?(opts, :name)) and type != :ref do
      raise ArgumentError, "#{here}: on_delete: and name: are for a :ref"
    end

    if Keyword.get(opts, :on_delete) not in [nil | @on_delete] do
      raise ArgumentError, "#{here}: on_delete: is one of #{inspect(@on_delete)}"
    end

    if List.keymember?(Module.get_attribute(module, :hosi_attribute), name, 0) do
      raise ArgumentError, "#{here} is declared twice"
    end

    Module.put_attribute(module, :hosi_attribute, {name, type, opts})
  end

  defmacro __before_compile__(env) do
    prefix = Module.get_attribute(env.module, :hosi_prefix)

    attributes =
      for {name, type, opts} <- env.module |> Module.get_attribute(:hosi_attribute) |> Enum.reverse() do
        attr = String.to_atom(prefix <> "/" <> String.replace(Atom.to_string(name), "_", "-"))
        {name, %{attr: attr, type: type, opts: opts}}
      end

    quote do
      @doc false
      def __hosi_vocabulary__,
        do: %{prefix: unquote(prefix), attributes: unquote(Macro.escape(attributes))}
    end
  end
end
