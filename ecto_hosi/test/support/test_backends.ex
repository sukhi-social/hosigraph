defmodule Ecto.Hosi.Test.Slow do
  @moduledoc false
  # The tests' backend (the stand-in, or `Hosi`) with writes that take a while: after `Slow.by(ms)`,
  # a write of the calling process reaches the connection only after `ms`.

  def by(ms), do: Process.put(__MODULE__, ms)

  defp real, do: Application.fetch_env!(:ecto_hosi, Ecto.Hosi.Test.Repo)[:hosi]

  def start_link(opts), do: real().start_link(opts)

  # the first query of a real engine may take longer than `Hosi`'s own 5 seconds
  def query_many(conn, queries) do
    case real() do
      Hosi -> Hosi.request(conn, :"query-many", %{queries: queries}, 120_000)
      other -> other.query_many(conn, queries)
    end
  end

  def get_entities(conn, ids), do: real().get_entities(conn, ids)
  def history(conn, id), do: real().history(conn, id)
  def write(conn, ops, opts \\ []), do: slowly(conn, fn -> real().write(conn, ops, opts) end)
  def erase(conn, erasures, opts \\ []), do: slowly(conn, fn -> real().erase(conn, erasures, opts) end)
  def clean(conn, opts \\ []), do: slowly(conn, fn -> real().clean(conn, opts) end)

  defp slowly(conn, fun) do
    case Process.get(__MODULE__) do
      nil ->
        fun.()

      ms ->
        :ok = :sys.suspend(conn)

        spawn(fn ->
          Process.sleep(ms)
          :sys.resume(conn)
        end)

        fun.()
    end
  end
end

defmodule Ecto.Hosi.Test.NoOpen do
  @moduledoc false
  # A backend whose database cannot be opened: it starts, and answers every query with the failure,
  # as `Hosi` does.

  def start_link(opts), do: Ecto.Hosi.Fake.start_link(opts)

  def query_many(_conn, _queries),
    do: {:error, {:app, :"backend-missing", "KhanaDatalevinJvm: no Datalevin jar at \"/nonexistent.jar\""}}
end
