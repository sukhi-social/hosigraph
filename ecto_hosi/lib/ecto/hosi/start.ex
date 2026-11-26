defmodule Ecto.Hosi.Start do
  @moduledoc false
  # Starts the connection and does not return until hosigraph has answered one query.
  #
  # Opening is quick to return but the first answers are not: the khana (Datalevin on a JVM)
  # loads and compiles its first queries while the caller waits, and `Hosi`'s own calls give up
  # after 5 seconds. On a small or busy machine that is long enough to fail the first request a
  # booting application makes. The supervisor of the repo waits for this child, so the
  # application starts only when the store is ready.

  @warm_timeout :timer.minutes(2)

  def start_link(backend, opts, name) do
    with {:ok, pid} <- backend.start_link(opts) do
      warm(backend, name)
      {:ok, pid}
    end
  end

  defp warm(Hosi, name) do
    # an attribute that does not exist is fine: the point is the round trip
    Hosi.request(
      name,
      :"query-many",
      %{queries: [%{q: "[:find ?e :where [?e :ecto-hosi/warm _]]", inputs: []}]},
      @warm_timeout
    )

    :ok
  catch
    _, _ -> :ok
  end

  defp warm(_backend, _name), do: :ok
end
