defmodule Mix.Tasks.Hosi.Doctor do
  @shortdoc "Checks that what a hosigraph Repo needs is there"

  @moduledoc """
  Looks at what each hosigraph Repo needs, without opening its database: the views, libhosi,
  Java or `dtlv`, the Datalevin jar, the directory and its lock.

      mix hosi.doctor                 # the application's Ecto repos
      mix hosi.doctor -r MyApp.Repo

  It exits with an error when something is missing.
  """

  use Mix.Task

  @impl Mix.Task
  def run(args) do
    failed =
      for repo <- Mix.Ecto.parse_repo(args), reduce: 0 do
        failed ->
          Mix.Ecto.ensure_repo(repo, args)

          if repo.__adapter__() == Ecto.Adapters.Hosi do
            Mix.shell().info("#{inspect(repo)}")
            checks = Ecto.Hosi.Doctor.checks(repo.config())

            for {status, what, detail} <- checks do
              Mix.shell().info("  #{String.pad_trailing(mark(status), 5)} #{what}: #{detail}")
            end

            failed + Enum.count(checks, &(elem(&1, 0) == :fail))
          else
            Mix.shell().info("#{inspect(repo)}: not a hosigraph Repo, skipped")
            failed
          end
      end

    if failed > 0, do: Mix.raise("mix hosi.doctor: #{failed} check(s) failed")
  end

  defp mark(:ok), do: "ok"
  defp mark(:warn), do: "note"
  defp mark(:fail), do: "FAIL"
end
