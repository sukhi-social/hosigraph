defmodule Ecto.Hosi.Start do
  @moduledoc false
  # Starts the connection and does not return until hosigraph has answered one query.
  #
  # Opening is quick to return but the first answers are not: the khana (Datalevin on a JVM)
  # loads and compiles its first queries while the caller waits, and `Hosi`'s own calls give up
  # after 5 seconds. On a small or busy machine that is long enough to fail the first request a
  # booting application makes. The supervisor of the repo waits for this child, so the
  # application starts only when the store is ready.

  require Logger

  @warm_timeout :timer.minutes(2)

  # an attribute that does not exist is fine: the point is the round trip
  @warm_query %{q: "[:find ?e :where [?e :ecto-hosi/warm _]]", inputs: []}

  def start_link(backend, opts, name) do
    with {:ok, pid} <- backend.start_link(opts) do
      case warm(backend, name) do
        :ok ->
          {:ok, pid}

        {:error, why} ->
          Process.unlink(pid)
          GenServer.stop(pid)
          message = "ecto_hosi: hosigraph did not open: " <> describe(why)
          Logger.error(message)
          {:error, {:ecto_hosi_open, message}}
      end
    end
  end

  # Opening does not say whether it worked: the first query does.
  defp warm(backend, name) do
    answer =
      cond do
        backend == Hosi ->
          Hosi.request(name, :"query-many", %{queries: [@warm_query]}, @warm_timeout)

        Code.ensure_loaded?(backend) and function_exported?(backend, :query_many, 2) ->
          backend.query_many(name, [@warm_query])

        true ->
          :ok
      end

    case answer do
      {:error, why} -> {:error, why}
      _ -> :ok
    end
  catch
    # still working (a large khana being built again takes long): the Repo starts, and says so
    :exit, {:timeout, _} ->
      Logger.warning(
        "ecto_hosi: hosigraph has not answered the first query yet. The Repo starts anyway; " <>
          "requests wait or time out until it answers."
      )

      :ok

    :exit, why ->
      {:error, why}
  end

  defp describe({status, kind, message}) when is_binary(message),
    do: "#{message} (#{status}#{if kind, do: ", #{kind}"})"

  defp describe(other), do: inspect(other)
end
