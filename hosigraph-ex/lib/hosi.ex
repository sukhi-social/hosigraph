defmodule Hosi do
  @moduledoc """
  hosigraph from Elixir, through libhosi (the C ABI that setu wrote).

  A connection is a process. It owns one `hosi_conn`, numbers the requests, and gives each
  answer back to whoever waited for it.

      {:ok, db} = Hosi.start_link(open: %{dir: "/var/lib/hosi", schema: %{"person/name": %{type: :string}}})
      {:ok, %{temps: _}} = Hosi.write(db, [[:create, [:tmp, 1], :"person/name", "Frege"]])
      Hosi.stop(db)

  With `open: %{}` nothing is opened: the connection answers `ping` and `abi` and refuses the rest.

  Answers are `{:ok, term}` or `{:error, {status, kind, message}}`. `status` is what the library
  said (`:app`, `:protocol`, `:io`, `:timeout`, `:cancelled`, `:closed`, `:internal`); `kind` and
  `message` are in the body when there is one (`nil` when not). A request that never reached the
  library is `{:error, {:submit, reason}}`.

  Maps come back with atom keys, because EDN keywords become atoms (see `Hosi.Edn`).
  """
  use GenServer

  alias Hosi.{Edn, Nif}

  @type conn :: GenServer.server()

  ## ---- starting and stopping

  @doc """
  Start a connection. `:open` is a map (written as EDN) or EDN text, and is what `hosi_open` is
  given. The rest of the options go to `GenServer`. Opening returns at once; if the database
  cannot be opened, the first requests are answered with that failure.
  """
  def start_link(opts \\ []) do
    {open, gen_opts} = Keyword.pop(opts, :open, %{})
    GenServer.start_link(__MODULE__, open, gen_opts)
  end

  @doc "Close it. Every request still waiting is answered `:closed`."
  def stop(conn, reason \\ :normal, timeout \\ :infinity), do: GenServer.stop(conn, reason, timeout)

  ## ---- asking

  @doc """
  One request: the operation (an atom, `:"write-many"` as the API spells it) and its arguments.
  The caller waits at most `timeout` ms (then the call exits, as `GenServer.call` does); the work
  itself is not stopped. The library's `hosi_cancel` is not offered yet: the ids are the
  connection's own, and nothing here needs it.
  """
  @spec request(conn, atom, term, timeout) :: {:ok, term} | {:error, term}
  def request(conn, op, args \\ nil, timeout \\ 5_000) do
    body = Edn.encode(if args == nil, do: %{op: op}, else: %{op: op, args: args})

    case GenServer.call(conn, {:request, body}, timeout) do
      {:error, _} = e -> e
      {status, bytes} -> answer(status, bytes)
    end
  end

  @doc false
  def answer(:ok, bytes) do
    case Edn.decode(bytes) do
      {:ok, %{ok: value}} -> {:ok, value}
      {:ok, other} -> {:error, {:internal, nil, "not an answer: " <> inspect(other)}}
      {:error, why} -> {:error, {:internal, nil, "unreadable answer: " <> why}}
    end
  end

  def answer(status, bytes) do
    case Edn.decode(bytes) do
      {:ok, %{error: kind, message: msg}} -> {:error, {status, kind, msg}}
      _ -> {:error, {status, nil, bytes}}
    end
  end

  ## ---- the operations (see Hosigraph.Abi.Api for what each takes and gives)

  def ping(conn, value \\ 1), do: request(conn, :ping, value)
  def abi(conn), do: request(conn, :abi)

  @doc "One batch, all of it or none. Ops are `[:create, ref, attr, val]` / `[:delete, ...]`, ref `id` or `[:tmp, n]`."
  def write(conn, ops), do: request(conn, :write, %{ops: ops})
  def write_many(conn, batches), do: request(conn, :"write-many", %{batches: batches})
  def seal(conn), do: request(conn, :seal)
  def get_entities(conn, ids), do: request(conn, :"get-entities", %{ids: ids})
  @doc "Each query is `%{q: \"[:find ...]\", inputs: [[:scalar, v] | [:coll, vs] | [:rel, rows]]}`; `:inputs` may be left out."
  def query_many(conn, queries), do: request(conn, :"query-many", %{queries: queries})
  def query_cells_many(conn, queries), do: request(conn, :"query-cells-many", %{queries: queries})
  def history(conn, id), do: request(conn, :history, %{id: id})
  def history_trace(conn, id), do: request(conn, :"history-trace", %{id: id})
  def verify(conn), do: request(conn, :verify)
  def stats(conn), do: request(conn, :stats)
  def rebuild_khana(conn), do: request(conn, :"rebuild-khana")

  ## ---- the process

  @impl true
  def init(open) do
    # trap exits so that `terminate/2` runs and the connection is closed properly
    Process.flag(:trap_exit, true)
    uri = if is_binary(open), do: open, else: Edn.encode(open)

    with :ok <- Nif.init(),
         {:ok, conn} <- Nif.open(uri) do
      {:ok, %{conn: conn, next: 1, waiting: %{}}}
    else
      {:error, reason} -> {:stop, {:hosi, reason}}
    end
  end

  @impl true
  def handle_call({:request, body}, from, %{conn: c, next: id} = st) do
    case Nif.submit(c, id, body, self()) do
      :ok ->
        {:noreply, %{st | next: id + 1, waiting: Map.put(st.waiting, id, from)}}

      {:error, reason} ->
        {:reply, {:error, {:submit, reason}}, %{st | next: id + 1}}
    end
  end

  @impl true
  def handle_info({:hosi_done, id, status, body}, st) do
    {from, waiting} = Map.pop(st.waiting, id)
    if from, do: GenServer.reply(from, {status, body})
    {:noreply, %{st | waiting: waiting}}
  end

  # the parent going away is handled by GenServer itself (terminate/2 runs); other links are not our business
  def handle_info({:EXIT, _from, _reason}, st), do: {:noreply, st}

  @impl true
  def terminate(_reason, st) do
    # close returns after every callback has returned, so each answer is already in the mailbox
    Nif.close(st.conn)
    drain(st.waiting)
  end

  defp drain(waiting) when map_size(waiting) == 0, do: :ok

  defp drain(waiting) do
    receive do
      {:hosi_done, id, status, body} ->
        case Map.pop(waiting, id) do
          {nil, w} -> drain(w)
          {from, w} -> GenServer.reply(from, {status, body}); drain(w)
        end
    after
      0 -> :ok
    end
  end
end
