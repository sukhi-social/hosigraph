Code.require_file("support/hosi_build.ex", __DIR__)

defmodule Hosi.MixProject do
  use Mix.Project

  @version "0.0.1"

  def project do
    [
      app: :hosi,
      version: @version,
      elixir: "~> 1.15",
      deps: [],
      aliases: [compile: [&build_nif/1, "compile"]]
    ]
  end

  def application, do: [extra_applications: [:logger]]

  # libhosi and the NIF: built here, or downloaded. See support/hosi_build.ex.
  defp build_nif(_args), do: Hosi.Build.run(@version, __DIR__)
end
