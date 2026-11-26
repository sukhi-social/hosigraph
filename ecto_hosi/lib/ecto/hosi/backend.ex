defmodule Ecto.Hosi.Backend do
  @moduledoc false
  # The one place that talks to hosigraph. The module that answers (`Hosi`, or a test double with
  # the same functions) lives in the adapter meta as `:backend`; the connection as `:conn`.

  def write(meta, ops, opts \\ []) do
    # a `hosi_keyword` field is a string for Elixir and a keyword for hosigraph: made here, once
    kws = keywords(meta)
    ops = Ecto.Hosi.Kw.ops(kws, ops)
    opts = Ecto.Hosi.Kw.opts(kws, opts)

    if Ecto.Hosi.Tx.active?(meta) do
      Ecto.Hosi.Tx.before_write(meta, ops)
      result = meta.backend.write(meta.conn, ops, opts)
      with {:ok, report} <- result, do: Ecto.Hosi.Tx.after_write(meta, report)
      result
    else
      meta.backend.write(meta.conn, ops, opts)
    end
  end

  def get_entities(meta, ids) do
    with {:ok, entities} <- meta.backend.get_entities(meta.conn, ids) do
      {:ok, Enum.map(entities, &Ecto.Hosi.Kw.entity(keywords(meta), &1))}
    end
  end

  @doc "`%{attr => %{string => keyword}}` of the `hosi_keyword` fields (empty if there are none)."
  def keywords(meta), do: Map.get(meta, :keywords, %{})

  @doc "One query; the rows of its answer."
  def query(meta, %{q: _} = req) do
    case meta.backend.query_many(meta.conn, [req]) do
      {:ok, [rows]} -> {:ok, rows}
      {:ok, other} -> {:error, {:internal, nil, "unexpected query answer: " <> inspect(other)}}
      {:error, _} = e -> e
    end
  end

  def ok!({:ok, value}), do: value
  def ok!({:error, reason}), do: raise(Ecto.Hosi.Error, reason: reason)

  @doc "The real id a `[:tmp, n]` became, from the report of a write."
  def temp_id(report, n) do
    temps = Map.get(report, :temps) || []

    Enum.find_value(temps, fn
      [^n, id] -> id
      {^n, id} -> id
      _ -> nil
    end) || raise Ecto.Hosi.Error, message: "hosigraph did not report an id for [:tmp, #{n}]"
  end

  @doc "A value as an op writes it: ids of other entities are `[:ref, id]` for `:ref` attributes."
  def wrap(meta, attr, value) do
    case meta.attrs do
      %{^attr => %{type: :ref}} -> [:ref, value]
      _ -> value
    end
  end

  @doc """
  The ops that retract everything an entity holds. A `(key, order)` tuple attribute (`hosi_tuple`)
  is kept by hosigraph and is neither written nor retracted, so it is left out.
  """
  def retract_all(meta, id, entity) do
    for {attr, vals} <- entity,
        not match?(%{^attr => %{tuple: _}}, meta.attrs),
        v <- vals,
        do: [:delete, id, attr, wrap(meta, attr, v)]
  end

  @doc "The first value of an attribute in an entity map (every attribute here has one value)."
  def first(entity, attr) do
    case Map.get(entity, attr) do
      [v | _] -> v
      _ -> nil
    end
  end
end
