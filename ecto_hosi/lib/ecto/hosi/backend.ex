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
      result = send_write(meta, ops, opts)
      with {:ok, report} <- result, do: Ecto.Hosi.Tx.after_write(meta, report)
      result
    else
      send_write(meta, ops, opts)
    end
  end

  @doc "`meta` with the `timeout:` of one Repo call, if it has one, in place of the Repo's."
  def with_timeout(meta, opts) do
    case Keyword.get(opts, :timeout) do
      nil -> meta
      timeout -> Map.put(meta, :timeout, timeout)
    end
  end

  def timeout(meta), do: Map.get(meta, :timeout, 15_000)

  @doc "One write to hosigraph, waited for as long as `timeout/1` says."
  def send_write(meta, ops, opts \\ []) do
    timeout = timeout(meta)

    waited(timeout, fn ->
      meta.backend.write(meta.conn, ops, Keyword.put(opts, :timeout, timeout))
    end)
  end

  @doc """
  `fun`, a call that changes the database. When the wait for it runs out, hosigraph goes on with
  it: the error says that nobody knows yet whether it will be saved.
  """
  def waited(timeout, fun) do
    fun.()
  catch
    :exit, {:timeout, {GenServer, :call, _}} ->
      raise Ecto.Hosi.Error,
        reason: :timeout,
        message:
          "hosigraph did not answer the write within #{timeout} ms. The result is unknown: " <>
            "hosigraph goes on with it, so it may still take effect, also inside a transaction, " <>
            "which then cannot take it back. Look before you try again, or give a longer timeout:."
  end

  @doc "`fun`, reading back what a write has just saved."
  def read_back(fun) do
    fun.()
  catch
    :exit, {:timeout, {GenServer, :call, _}} ->
      raise Ecto.Hosi.Error,
        reason: :timeout,
        message: "the write was saved, but reading it back timed out. Do not write it again."
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

  @doc """
  `%{n => id}`: the real id each `[:tmp, n]` became, from the report of a write. Made once for a
  write, so that finding the ids of many new rows does not walk the whole report for each one.
  """
  def temp_ids(report) do
    (Map.get(report, :temps) || [])
    |> Enum.flat_map(fn
      [n, id] -> [{n, id}]
      {n, id} -> [{n, id}]
      _ -> []
    end)
    |> Map.new()
  end

  @doc "The real id `[:tmp, n]` became, from `temp_ids/1`."
  def temp_id(temps, n) do
    case temps do
      %{^n => id} when not is_nil(id) -> id
      _ -> raise Ecto.Hosi.Error, message: "hosigraph did not report an id for [:tmp, #{n}]"
    end
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
