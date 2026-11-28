defmodule Ecto.Hosi.Types do
  @moduledoc false
  # Ecto types <-> the value types hosigraph keeps (any/string/long/double/boolean/keyword/ref/uuid).
  # Times are kept as microseconds since the epoch (a long), so `highest/3` can order them.
  # Maps, and lists kept as one value, are JSON text; binaries are Base64 text: strings, because
  # hosigraph has no composite value type. They are stored and compared as the text, so they can be
  # read back and matched with `==`, but not queried inside (a list that you search by its items is a
  # field with many values, `hosi_many`). A UUID is hosigraph's own; between Ecto and the adapter
  # it is its text.
  # The JSON is Elixir's own `JSON` (1.18+).

  @epoch_date ~D[1970-01-01]
  @epoch_naive ~N[1970-01-01 00:00:00]

  @doc "The hosigraph type of an encoding (`Ecto.Hosi.Model`: how the values of a type are kept)."
  def hosi_type(encoding) do
    case encoding do
      e when e in [:integer, :date, :datetime] -> :long
      e when e in [:string, :decimal, :time, :map, :binary] -> :string
      {:array, _} -> :string
      :boolean -> :boolean
      :float -> :double
      :uuid -> :uuid
    end
  end

  @doc """
  A value no type was there to dump (a parameter of a schemaless query, a column the schema types
  more strictly than the caller does): times, dates, decimals, maps and lists in the form
  hosigraph keeps them. Anything else is returned as it is.
  """
  def raw_dump(%DateTime{} = dt), do: DateTime.to_unix(dt, :microsecond)
  def raw_dump(%NaiveDateTime{} = n), do: NaiveDateTime.diff(n, @epoch_naive, :microsecond)
  def raw_dump(%Date{} = d), do: Date.diff(d, @epoch_date)
  def raw_dump(%Time{} = t), do: Time.to_iso8601(t)
  def raw_dump(%Decimal{} = d), do: Decimal.to_string(d, :normal)
  def raw_dump(v) when is_map(v) and not is_struct(v), do: JSON.encode!(v)
  def raw_dump(v), do: v

  @doc "The parameters of a query with `raw_dump/1` applied (lists element by element)."
  def normalize_params(params) do
    Enum.map(params, fn
      list when is_list(list) -> Enum.map(list, &raw_dump/1)
      v -> raw_dump(v)
    end)
  end

  ## ---- the Ecto.Adapter loaders/dumpers callbacks

  def dumpers(p, type) when p in [:utc_datetime, :utc_datetime_usec], do: [type, ns(&dump_utc/1)]

  def dumpers(p, type) when p in [:naive_datetime, :naive_datetime_usec],
    do: [type, ns(&dump_naive/1)]

  def dumpers(:date, type), do: [type, ns(&dump_date/1)]
  def dumpers(p, type) when p in [:time, :time_usec], do: [type, ns(&dump_time/1)]
  def dumpers(:decimal, type), do: [type, ns(&dump_decimal/1)]
  def dumpers(:map, type), do: [type, ns(&dump_json/1)]
  # the items of a list are dumped one by one by Ecto; the list stays a list here, and is made into
  # what is kept (`Ecto.Hosi.Store.normalize/3`)
  def dumpers({:array, _}, type), do: [type]
  def dumpers(:binary, type), do: [type, ns(&dump_binary/1)]
  def dumpers(:uuid, type), do: [type, ns(&dump_uuid/1)]
  def dumpers(_primitive, type), do: [type]

  def loaders(p, type) when p in [:utc_datetime, :utc_datetime_usec], do: [ns(&load_utc/1), type]

  def loaders(p, type) when p in [:naive_datetime, :naive_datetime_usec],
    do: [ns(&load_naive/1), type]

  def loaders(:date, type), do: [ns(&load_date/1), type]
  def loaders(p, type) when p in [:time, :time_usec], do: [ns(&load_time/1), type]
  def loaders(:decimal, type), do: [ns(&load_decimal/1), type]
  def loaders(:float, type), do: [ns(&load_float/1), type]
  def loaders(:map, type), do: [ns(&load_json/1), type]
  def loaders({:array, :binary}, type), do: [ns(&load_binaries/1), type]
  def loaders({:array, _}, type), do: [ns(&load_list/1), type]
  def loaders(:uuid, type), do: [ns(&load_uuid/1), type]
  def loaders(_primitive, type), do: [type]

  @doc """
  A value as Ecto should see it, read straight from a fact, for a field of `type`. Binaries are
  kept as Base64 text, and a text is already a binary, so Ecto would not run a loader over it:
  decode it here.
  """
  def read(type, value) when is_binary(value) and not is_nil(type) do
    if Ecto.Type.type(type) == :binary do
      case Base.decode64(value) do
        {:ok, bin} -> bin
        :error -> value
      end
    else
      value
    end
  end

  def read(_type, value), do: value

  @doc """
  The right side of `x in ^list` as a list of values (a list kept as JSON text arrives as one).
  """
  def unpack_list(text) when is_binary(text) do
    case JSON.decode(text) do
      {:ok, list} when is_list(list) -> list
      _ -> [text]
    end
  end

  def unpack_list(vs), do: vs

  # Ecto hands `nil` ("no such fact") to these functions too; it stays `nil`.
  defp ns(fun),
    do: fn
      nil -> {:ok, nil}
      value -> fun.(value)
    end

  defp dump_utc(%DateTime{} = dt), do: {:ok, DateTime.to_unix(dt, :microsecond)}
  defp dump_utc(_), do: :error

  defp dump_naive(%NaiveDateTime{} = n),
    do: {:ok, NaiveDateTime.diff(n, @epoch_naive, :microsecond)}

  defp dump_naive(_), do: :error
  defp dump_date(%Date{} = d), do: {:ok, Date.diff(d, @epoch_date)}
  defp dump_date(_), do: :error
  defp dump_time(%Time{} = t), do: {:ok, Time.to_iso8601(t)}
  defp dump_time(_), do: :error
  defp dump_decimal(%Decimal{} = d), do: {:ok, Decimal.to_string(d, :normal)}
  defp dump_decimal(_), do: :error

  defp dump_json(value) when is_map(value) or is_list(value), do: {:ok, JSON.encode!(value)}
  defp dump_json(_), do: :error

  defp dump_binary(b) when is_binary(b), do: {:ok, Base.encode64(b)}
  defp dump_binary(_), do: :error

  defp dump_uuid(<<_::128>> = raw), do: Ecto.UUID.load(raw)
  defp dump_uuid(_), do: :error

  defp load_uuid(text) when is_binary(text), do: Ecto.UUID.dump(text)
  defp load_uuid(_), do: :error

  # a list kept as JSON text, or the many values of an attribute
  defp load_list(list) when is_list(list), do: {:ok, list}
  defp load_list(text), do: load_json(text)

  defp load_binaries(value) do
    with {:ok, items} when is_list(items) <- load_list(value) do
      {:ok,
       Enum.map(items, fn
         nil -> nil
         item ->
           case Base.decode64(item) do
             {:ok, binary} -> binary
             :error -> item
           end
       end)}
    else
      _ -> :error
    end
  end

  defp load_json(s) when is_binary(s) do
    case JSON.decode(s) do
      {:ok, value} -> {:ok, value}
      _ -> :error
    end
  end

  defp load_json(_), do: :error

  defp load_utc(n) when is_integer(n), do: {:ok, DateTime.from_unix!(n, :microsecond)}
  defp load_utc(_), do: :error

  defp load_naive(n) when is_integer(n),
    do: {:ok, NaiveDateTime.add(@epoch_naive, n, :microsecond)}

  defp load_naive(_), do: :error
  defp load_date(n) when is_integer(n), do: {:ok, Date.add(@epoch_date, n)}
  defp load_date(_), do: :error
  defp load_time(s) when is_binary(s), do: Time.from_iso8601(s)
  defp load_time(_), do: :error
  defp load_decimal(s) when is_binary(s), do: {:ok, Decimal.new(s)}
  defp load_decimal(%Decimal{} = d), do: {:ok, d}
  defp load_decimal(n) when is_integer(n), do: {:ok, Decimal.new(n)}
  defp load_decimal(n) when is_float(n), do: {:ok, Decimal.from_float(n)}
  defp load_decimal(_), do: :error
  defp load_float(n) when is_integer(n), do: {:ok, n * 1.0}
  defp load_float(n) when is_float(n), do: {:ok, n}
  defp load_float(_), do: :error
end
