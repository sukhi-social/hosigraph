defmodule Hosi.Edn do
  @moduledoc """
  The small piece of EDN that hosigraph's wire needs.

  Going out: `nil`, booleans, integers, floats, strings, atoms (as keywords), lists (as vectors),
  maps. Coming in, the same, and also lists `( )` and sets `#{ }`. A comma is a space.

    * a keyword `:person/name` is the atom `:"person/name"` (the names of a schema are the
      user's own, and few);
    * a vector or a list is a list; a set is a `MapSet`;
    * `##NaN`, `##Inf`, `##-Inf` are `{:float, :nan | :inf | :neg_inf}`, since the BEAM has none;
    * a symbol is `{:symbol, "name"}`. Tagged elements (`#inst ...`) are not read.
  """

  @doc "EDN text for a term. Raises `ArgumentError` for what EDN here cannot say."
  @spec encode(term) :: binary
  def encode(term), do: IO.iodata_to_binary(enc(term))

  defp enc(nil), do: "nil"
  defp enc(true), do: "true"
  defp enc(false), do: "false"
  defp enc(n) when is_integer(n), do: Integer.to_string(n)
  defp enc(f) when is_float(f), do: Float.to_string(f)
  defp enc(a) when is_atom(a), do: [?:, Atom.to_string(a)]
  defp enc(s) when is_binary(s) do
    unless String.valid?(s), do: raise(ArgumentError, "not UTF-8: #{inspect(s)}")
    [?", for(<<c::utf8 <- s>>, do: esc(c)), ?"]
  end
  defp enc(l) when is_list(l), do: [?[, l |> Enum.map(&enc/1) |> Enum.intersperse(?\s), ?]]
  defp enc(m) when is_map(m) and not is_struct(m) do
    [?{, m |> Enum.map(fn {k, v} -> [enc(k), ?\s, enc(v)] end) |> Enum.intersperse(", "), ?}]
  end
  defp enc(other), do: raise(ArgumentError, "cannot write as EDN: #{inspect(other)}")

  defp esc(?"), do: "\\\""
  defp esc(?\\), do: "\\\\"
  defp esc(?\n), do: "\\n"
  defp esc(?\r), do: "\\r"
  defp esc(?\t), do: "\\t"
  defp esc(c) when c < 0x20 or c == 0x7F,
    do: ["\\u", c |> Integer.to_string(16) |> String.downcase() |> String.pad_leading(4, "0")]
  defp esc(c), do: <<c::utf8>>

  @doc "Read exactly one form."
  @spec decode(binary) :: {:ok, term} | {:error, String.t()}
  def decode(text) when is_binary(text) do
    with {:ok, term, rest} <- form(skip(text)) do
      case skip(rest) do
        "" -> {:ok, term}
        more -> {:error, "unexpected trailing input: " <> String.slice(more, 0, 20)}
      end
    end
  end

  defp skip(<<c, rest::binary>>) when c in [?\s, ?\t, ?\n, ?\r, ?,], do: skip(rest)
  defp skip(<<?;, rest::binary>>) do
    case :binary.split(rest, "\n") do
      [_, more] -> skip(more)
      [_] -> ""
    end
  end
  defp skip(text), do: text

  defp form(""), do: {:error, "unexpected end of input"}
  defp form("nil" <> r = t), do: atom_end(t, r, nil)
  defp form("true" <> r = t), do: atom_end(t, r, true)
  defp form("false" <> r = t), do: atom_end(t, r, false)
  defp form("##NaN" <> r), do: {:ok, {:float, :nan}, r}
  defp form("##Inf" <> r), do: {:ok, {:float, :inf}, r}
  defp form("##-Inf" <> r), do: {:ok, {:float, :neg_inf}, r}
  defp form(<<?", r::binary>>), do: string(r, [])
  defp form(<<?[, r::binary>>), do: seq(skip(r), ?], [])
  defp form(<<?(, r::binary>>), do: seq(skip(r), ?), [])
  defp form("\#{" <> r) do
    with {:ok, xs, rest} <- seq(skip(r), ?}, []), do: {:ok, MapSet.new(xs), rest}
  end
  defp form(<<?{, r::binary>>) do
    with {:ok, xs, rest} <- seq(skip(r), ?}, []) do
      if rem(length(xs), 2) == 0,
        do: {:ok, xs |> Enum.chunk_every(2) |> Map.new(fn [k, v] -> {k, v} end), rest},
        else: {:error, "a map with an odd number of forms"}
    end
  end
  defp form(<<?:, r::binary>>) do
    {name, rest} = token(r)
    if name == "", do: {:error, "empty keyword"}, else: {:ok, String.to_atom(name), rest}
  end
  defp form(<<?#, _::binary>>), do: {:error, "tagged elements and dispatch forms are not read"}
  defp form(<<c, _::binary>> = t) when c in ?0..?9 or c in [?-, ?+] do
    {tok, rest} = token(t)
    case number(tok) do
      {:ok, n} -> {:ok, n, rest}
      :error -> {:ok, {:symbol, tok}, rest}
    end
  end
  defp form(t) do
    case token(t) do
      {"", <<c, _::binary>>} -> {:error, "unexpected #{<<c>>}"}
      {tok, rest} -> {:ok, {:symbol, tok}, rest}
    end
  end

  # `nil` and friends only when the word ends there (so `nilly` stays a symbol)
  defp atom_end(t, rest, value) do
    case token(t) do
      {tok, _} when tok in ["nil", "true", "false"] -> {:ok, value, rest}
      {tok, rest2} -> {:ok, {:symbol, tok}, rest2}
    end
  end

  @delims [?\s, ?\t, ?\n, ?\r, ?,, ?[, ?], ?(, ?), ?{, ?}, ?", ?;]

  defp token(t) do
    n = tlen(t, 0)
    {binary_part(t, 0, n), binary_part(t, n, byte_size(t) - n)}
  end

  defp tlen(<<c, r::binary>>, n) when c not in @delims, do: tlen(r, n + 1)
  defp tlen(_, n), do: n

  defp number(tok) do
    tok = String.trim_trailing(tok, "N")
    cond do
      Regex.match?(~r/\A[-+]?\d+\z/, tok) -> {:ok, String.to_integer(String.trim_leading(tok, "+"))}
      Regex.match?(~r/\A[-+]?\d+(\.\d+)?([eE][-+]?\d+)?M?\z/, tok) -> {:ok, tok |> String.trim_trailing("M") |> to_float()}
      true -> :error
    end
  end

  # Float.parse wants a digit after the dot and a dot before the exponent
  defp to_float(tok) do
    tok = if String.contains?(tok, "."), do: tok, else: String.replace(tok, ~r/([eE])/, ".0\\1", global: false)
    tok = if String.contains?(tok, "."), do: tok, else: tok <> ".0"
    {f, ""} = tok |> String.trim_leading("+") |> Float.parse()
    f
  end

  defp seq(<<c, r::binary>>, close, acc) when c == close, do: {:ok, Enum.reverse(acc), r}
  defp seq("", _close, _acc), do: {:error, "unexpected end of input"}
  defp seq(t, close, acc) do
    with {:ok, x, rest} <- form(t), do: seq(skip(rest), close, [x | acc])
  end

  defp string(<<?", r::binary>>, acc), do: {:ok, acc |> Enum.reverse() |> IO.iodata_to_binary(), r}
  defp string(<<?\\, ?n, r::binary>>, acc), do: string(r, ["\n" | acc])
  defp string(<<?\\, ?t, r::binary>>, acc), do: string(r, ["\t" | acc])
  defp string(<<?\\, ?r, r::binary>>, acc), do: string(r, ["\r" | acc])
  defp string(<<?\\, ?", r::binary>>, acc), do: string(r, ["\"" | acc])
  defp string(<<?\\, ?\\, r::binary>>, acc), do: string(r, ["\\" | acc])
  defp string(<<?\\, ?u, h::binary-size(4), r::binary>>, acc) do
    case Integer.parse(h, 16) do
      {c, ""} when c < 0xD800 or c > 0xDFFF -> string(r, [<<c::utf8>> | acc])
      _ -> {:error, "bad \\u escape: " <> h}
    end
  end
  defp string(<<?\\, c::utf8, _::binary>>, _), do: {:error, "unknown escape: \\" <> <<c::utf8>>}
  defp string(<<c::utf8, r::binary>>, acc), do: string(r, [<<c::utf8>> | acc])
  defp string(_, _), do: {:error, "unterminated string"}
end
