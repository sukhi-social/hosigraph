defmodule Ecto.Hosi.Case do
  @moduledoc false
  use ExUnit.CaseTemplate

  # Every test starts its own Repo (a dynamic one, with its own directory and connection), so the test
  # modules can run async (`use Ecto.Hosi.Case, async: true`): starting a real hosigraph costs about a second and a half, and that is most of
  # the time of a real run.
  using do
    quote do
      import Ecto.Query
      import Ecto.Hosi.Case, only: [facts: 0, start_repo: 1, direct_write: 1]
      alias Ecto.Hosi.Test.{Doc, Follow, Post, Repo, Rules, User}
      alias Ecto.Hosi.Fake
    end
  end

  setup do
    start_repo()
    :ok
  end

  @doc "Start a Repo of this test's own (`opts` over the test config) and make it the current one."
  def start_repo(opts \\ []) do
    n = System.unique_integer([:positive])
    dir = Path.join(System.tmp_dir!(), "ecto_hosi_#{n}")
    name = :"ecto_hosi_test_repo_#{n}"
    File.rm_rf!(dir)
    on_exit(fn -> File.rm_rf(dir) end)

    start_supervised!(
      {Ecto.Hosi.Test.Repo, [name: name, conn_name: conn_name(name), dir: dir] ++ opts},
      id: name
    )

    Ecto.Hosi.Test.Repo.put_dynamic_repo(name)
    name
  end

  defp conn_name(name), do: :"#{name}_conn"

  @doc "The connection (what `Fake.dump/1` takes) of the Repo this test started."
  def conn, do: conn_name(Ecto.Hosi.Test.Repo.get_dynamic_repo())

  @doc "A write that goes past the adapter, as another program (or `Hosi.write`) would make it."
  def direct_write(ops) do
    backend = Application.get_env(:ecto_hosi, Ecto.Hosi.Test.Repo)[:hosi]
    {:ok, report} = backend.write(conn(), ops, [])
    report
  end

  @doc "All facts, as `%{{id, attr} => values}`: from the stand-in, or from a real hosigraph."
  def facts do
    case Application.get_env(:ecto_hosi, Ecto.Hosi.Test.Repo)[:hosi] do
      Ecto.Hosi.Fake ->
        Ecto.Hosi.Fake.dump(conn()).facts

      _real ->
        # (ids up to 1 are hosigraph's own bookkeeping)
        Ecto.Hosi.Test.Repo.datalog(%{q: "[:find ?e ?a ?v :where [?e ?a ?v] [(> ?e 1)]]", inputs: []})
        |> Enum.group_by(fn [e, a, _] -> {e, a} end, fn [_, _, v] -> v end)
    end
  end
end
