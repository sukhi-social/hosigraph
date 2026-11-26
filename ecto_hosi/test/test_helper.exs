real? = System.get_env("HOSI_REAL") == "1"

base = [
  dir: System.tmp_dir!() |> Path.join("ecto_hosi_test_dir"),
  schemas: [Ecto.Hosi.Test.User, Ecto.Hosi.Test.Post, Ecto.Hosi.Test.Follow, Ecto.Hosi.Test.Doc, Ecto.Hosi.Test.Pair, Ecto.Hosi.Test.Team, Ecto.Hosi.Test.Member, Ecto.Hosi.Test.Item, Ecto.Hosi.Test.Note, Ecto.Hosi.Test.Seat, Ecto.Hosi.Test.Entry]
]

config =
  if real? do
    # a real hosigraph with a real Datalevin: HOSI_DTLV=/path/to/dtlv HOSI_REAL=1 mix test
    # or, with a JDK 21 and the standalone jar: HOSI_JAR=/path/to/datalevin-standalone.jar HOSI_REAL=1 mix test
    khana =
      case System.get_env("HOSI_JAR") do
        nil -> [:dtlv, System.fetch_env!("HOSI_DTLV")]
        jar -> [:jvm, System.get_env("HOSI_JAVA", "java"), jar]
      end

    base ++ [hosi: Hosi, open: %{config: %{khana: khana}}]
  else
    base ++ [hosi: Ecto.Hosi.Fake]
  end

Application.put_env(:ecto_hosi, Ecto.Hosi.Test.Repo, config)

# tests that look inside the stand-in only run on it; tests that need hosigraph's (missing) rules input
# can be left out with `--exclude needs_rules_door`
ExUnit.configure(exclude: if(real?, do: [:fake_only], else: []))
ExUnit.start()
