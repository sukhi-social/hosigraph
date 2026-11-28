defmodule Ecto.Hosi.TasksTest do
  # `mix hosi.attributes` and `mix hosi.doctor`
  use ExUnit.Case, async: true

  alias Ecto.Hosi.{Doctor, Model}
  alias Ecto.Hosi.Test.Social

  test "the attribute list: types, rules, and the fields that are each attribute" do
    text = Social.schemas() |> Model.build() |> Mix.Tasks.Hosi.Attributes.report() |> IO.iodata_to_binary()

    assert text =~ ~r/^:ap\/type\s+keyword\s+one of boost \| follow.*record type$/m
    assert text =~ ~r/^:ap\/alt-id\s+many string\s+unique$/m
    assert text =~ ~r/^:ap\/published\s+long \(:utc_datetime_usec\)$/m
    assert text =~ ~r/^:ap\/uri\s+string\s+unique, final$/m
    assert text =~ ~r/^:hosi\/owner\s+ref\s+final, the maker/m
    assert text =~ ~r/^:s3\/path\s+string\s+file$/m
    assert text =~ ~r/^:mention_ids\s+many ref\s+on delete: nilify$/m
    assert text =~ ~r/^:hosi\/ap\.attributed-to\+ap\.published\s+tuple\s+index over/m
    assert text =~ ~r/^:hosi\/uq-|^:hosi\/follow\.from\+follow\.to|unique over :follow\/from \+ :follow\/to/m
    assert text =~ "    Ecto.Hosi.Test.Status.uri\n"
    assert text =~ "    Ecto.Hosi.Test.Actor.uri\n"
  end

  defp check(checks, what), do: Enum.find(checks, &(elem(&1, 1) == what))

  defp tmp(name), do: Path.join(System.tmp_dir!(), "ecto_hosi_doctor_#{name}_#{System.unique_integer([:positive])}")

  test "the doctor says what is missing" do
    checks =
      Doctor.checks(
        schemas: Social.schemas(),
        dir: tmp("none"),
        khana: [jvm: "/nonexistent/datalevin.jar", java: "/nonexistent/java"]
      )

    assert {:ok, "views", _} = check(checks, "views")
    assert {:fail, "java", java} = check(checks, "java")
    assert java =~ "/nonexistent/java"
    assert {:fail, "Datalevin jar", jar} = check(checks, "Datalevin jar")
    assert jar =~ "/nonexistent/datalevin.jar"
    assert {:ok, "directory", directory} = check(checks, "directory")
    assert directory =~ "made at the first start"
    assert {:ok, "lock", _} = check(checks, "lock")
  end

  test "a wrong view, a wrong khana, a file where the directory should be" do
    file = tmp("file")
    File.write!(file, "")
    on_exit(fn -> File.rm(file) end)

    checks = Doctor.checks(schemas: [Ecto.Hosi.Test.Unlisted], dir: file, khana: [dtlv: "no-such-dtlv"])

    assert {:fail, "views", views} = check(checks, "views")
    assert views =~ "use Ecto.Hosi.View"
    assert {:fail, "dtlv", _} = check(checks, "dtlv")
    assert {:fail, "directory", _} = check(checks, "directory")

    checks = Doctor.checks(schemas: [], dir: file, khana: [what: 1])
    assert {:warn, "views", _} = check(checks, "views")
    assert {:fail, "Datalevin", message} = check(checks, "Datalevin")
    assert message =~ "khana: is [jvm:"
  end

  test "khana: in its short forms, and not twice" do
    model = Model.build(Social.schemas())
    open = &Ecto.Hosi.Config.open(&1, model)

    assert open.(khana: [jvm: "/a.jar"]).config.khana == [:jvm, "java", "/a.jar"]
    assert open.(khana: [dtlv: "/d"], open: %{config: %{sync: false}}).config == %{sync: false, khana: [:"dtlv-repl", "/d"]}
    assert open.(open: %{config: %{khana: [:jvm, "j", "/b.jar"]}}).config.khana == [:jvm, "j", "/b.jar"]

    assert_raise ArgumentError, ~r/twice/, fn ->
      open.(khana: [jvm: "/a.jar"], open: %{config: %{khana: [:jvm, "java", "/b.jar"]}})
    end
  end

  test "a stand-in needs no engine" do
    checks = Doctor.checks(schemas: Social.schemas(), dir: tmp("fake"), hosi: Ecto.Hosi.Fake)
    assert {:warn, "libhosi", _} = check(checks, "libhosi")
    refute check(checks, "java")
  end

  @tag :real_only
  @tag timeout: 180_000
  test "all is well with the real engine's pieces" do
    config = Application.fetch_env!(:ecto_hosi, Ecto.Hosi.Test.Repo)
    dir = tmp("real")
    on_exit(fn -> File.rm_rf(dir) end)
    {:ok, pid} = Ecto.Hosi.Test.Repo.start_link(config |> Keyword.merge(name: :hosi_doctor_repo, conn_name: :hosi_doctor_conn, dir: dir))

    checks = Doctor.checks(Keyword.put(config, :dir, dir))
    assert Enum.reject(checks, &(elem(&1, 0) == :ok)) |> Enum.map(&elem(&1, 1)) == ["lock"]
    assert {:warn, "lock", held} = check(checks, "lock")
    assert held =~ "held"

    Supervisor.stop(pid)
    assert {:ok, "lock", "free"} = check(Doctor.checks(Keyword.put(config, :dir, dir)), "lock")
  end
end
