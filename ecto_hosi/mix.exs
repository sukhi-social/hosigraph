defmodule EctoHosi.MixProject do
  use Mix.Project

  @version "0.2.0"
  @source_url "https://github.com/sukhi-social/hosigraph/tree/main/ecto_hosi"

  def project do
    [
      app: :ecto_hosi,
      version: @version,
      elixir: "~> 1.18",
      elixirc_paths: elixirc_paths(Mix.env()),
      deps: deps(),
      description:
        "An Ecto adapter for hosigraph: events as truth, Datalog (with rules) for reading",
      source_url: @source_url,
      package: [
        links: %{"GitHub" => @source_url},
        files: ~w(lib docs mix.exs README.md README.ja.md)
      ]
    ]
  end

  def application, do: [extra_applications: [:logger]]

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      # Developed against Ecto 3.15.0-dev (master); 3.12-3.14 have not been run.
      {:ecto, "~> 3.12"},
      hosi_dep()
    ]
  end

  # In the hosigraph monorepo this is the sibling `../hosigraph-ex`.
  # `HOSI_PATH=/elsewhere/hosigraph-ex` overrides it.
  # The tests run against an in-memory stand-in, so `HOSI_BUILD=none mix test` needs no libhosi.
  defp hosi_dep do
    case System.get_env("HOSI_PATH") do
      nil -> {:hosi, path: "../hosigraph-ex"}
      path -> {:hosi, path: path}
    end
  end
end
