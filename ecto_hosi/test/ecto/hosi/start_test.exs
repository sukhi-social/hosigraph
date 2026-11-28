defmodule Ecto.Hosi.StartTest do
  # A Repo whose database cannot be opened does not start, and says why.
  use ExUnit.Case, async: true

  import ExUnit.CaptureLog

  alias Ecto.Hosi.Test.Repo

  defp start(opts) do
    Process.flag(:trap_exit, true)
    n = System.unique_integer([:positive])
    dir = Path.join(System.tmp_dir!(), "ecto_hosi_start_#{n}")
    on_exit(fn -> File.rm_rf(dir) end)
    name = :"ecto_hosi_start_#{n}"
    Repo.start_link([name: name, conn_name: :"#{name}_conn", dir: dir] ++ opts)
  end

  test "a database that cannot be opened fails the start, with the reason" do
    log =
      capture_log(fn ->
        assert {:error, reason} = start(hosi: Ecto.Hosi.Test.NoOpen)
        assert inspect(reason) =~ "no Datalevin jar"
      end)

    assert log =~ "hosigraph did not open"
    assert log =~ "no Datalevin jar"
  end

  @tag :real_only
  test "a wrong jar path fails the start" do
    java = System.get_env("HOSI_JAVA", "java")

    capture_log(fn ->
      assert {:error, reason} = start(khana: [jvm: "/nonexistent.jar", java: java])
      assert inspect(reason) =~ "/nonexistent.jar"
    end)
  end
end
