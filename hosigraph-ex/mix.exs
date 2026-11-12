defmodule Hosi.MixProject do
  use Mix.Project

  def project do
    [
      app: :hosi,
      version: "0.0.1",
      elixir: "~> 1.15",
      deps: [],
      aliases: [compile: [&build_nif/1, "compile"]]
    ]
  end

  def application, do: [extra_applications: [:logger]]

  # ---- the NIF -------------------------------------------------------------------------------
  #
  # Needs libhosi (`cabal build hosigraph-abi:hosi`) and its header. Say where with
  #   HOSI_LIB_DIR=/dir/with/libhosi.{dylib,so}   HOSI_INCLUDE_DIR=/dir/with/hosi.h
  # Without them, the build tree of this repository is looked in.

  defp build_nif(_args) do
    root = __DIR__
    src = Path.join(root, "c_src/hosi_nif.c")
    out = Path.join(root, "priv/hosi_nif.so")
    include = System.get_env("HOSI_INCLUDE_DIR") || Path.expand("../hosigraph-abi/gen/include", root)
    lib_dir = System.get_env("HOSI_LIB_DIR") || find_lib_dir(root)

    unless lib_dir && File.exists?(Path.join(include, "hosi.h")) do
      Mix.raise("""
      libhosi not found. Build it (cabal build hosigraph-abi:hosi) and set
        HOSI_LIB_DIR (where libhosi.dylib / libhosi.so is) and HOSI_INCLUDE_DIR (where hosi.h is).
      """)
    end

    if stale?(out, [src, Path.join(include, "hosi.h")]) do
      erts = Path.join([:code.root_dir(), "erts-#{:erlang.system_info(:version)}", "include"])
      darwin = match?({:unix, :darwin}, :os.type())

      args =
        ["-shared", "-fPIC", "-O2", "-Wall", "-Wextra", "-I", erts, "-I", include, src,
         "-o", out, "-L", lib_dir, "-lhosi", "-Wl,-rpath," <> lib_dir] ++
          if(darwin, do: ["-undefined", "dynamic_lookup"], else: [])

      File.mkdir_p!(Path.dirname(out))
      {output, status} = System.cmd(System.get_env("CC", "cc"), args, stderr_to_stdout: true)
      if status != 0, do: Mix.raise("cc failed:\n" <> output)
      if output != "", do: Mix.shell().info(output)
    end

    :ok
  end

  defp find_lib_dir(root) do
    pattern = Path.join(root, "../dist-newstyle/build/*/ghc-*/hosigraph-abi-*/f/hosi/build/hosi/libhosi.*")
    case Path.wildcard(pattern) do
      [file | _] -> Path.dirname(file)
      [] -> nil
    end
  end

  defp stale?(out, inputs) do
    case File.stat(out, time: :posix) do
      {:ok, %{mtime: t}} -> Enum.any?(inputs, fn f -> match?({:ok, %{mtime: m}} when m > t, File.stat(f, time: :posix)) end)
      _ -> true
    end
  end
end
