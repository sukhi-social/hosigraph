defmodule Ecto.Hosi.Attr do
  @moduledoc """
  How an Ecto schema is written down as hosigraph attributes.

  A schema with source `"user"` and a field `:name` becomes the attribute `:"user/name"`.
  Every row also carries a marker attribute `:"user/ecto-kind"` (always `true`), which is how
  "all users" is found: hosigraph has no tables, only attributes.
  """

  @kind "ecto-kind"

  @doc "The attribute of `field` in the schema whose source is `source`."
  def attr(source, field), do: String.to_atom("#{source}/#{field}")

  @doc "The `(key, order)` index attribute: `:\"note/account_id+created_at\"`."
  def tuple(source, key, order), do: String.to_atom("#{source}/#{key}+#{order}")

  @doc "The marker attribute that says an entity is a row of `source`."
  def marker(source), do: attr(source, @kind)

  @doc "`:\"user/email\"` -> `\"user_email_index\"`, the name Ecto's `unique_constraint/3` expects."
  def constraint_name(attr) do
    {source, field} = split(attr)
    "#{source}_#{field}_index"
  end

  def split(attr) do
    [source, field] = attr |> Atom.to_string() |> String.split("/", parts: 2)
    {source, field}
  end

  @doc "EDN keyword text for an attribute, e.g. `\":user/name\"`."
  def kw(attr) when is_atom(attr), do: ":" <> Atom.to_string(attr)
end
