defmodule Hosi.Build do
  @moduledoc false
  # Puts the NIF and libhosi into priv/.  Two ways:
  #
  #   source        libhosi was built here (cabal build hosigraph-abi:flib:hosi): compile the NIF with cc.
  #   precompiled   download the bundle for this machine from the GitHub release, check its sha256
  #                 against checksum.txt (shipped with the package), unpack it into priv/.
  #
  # HOSI_BUILD=source | precompiled | none   force one (none: use priv/ as it is).
  # Without it: source if libhosi is found (HOSI_LIB_DIR, or this repository's dist-newstyle),
  # otherwise precompiled.
  #
  # HOSI_LIB_DIR, HOSI_INCLUDE_DIR   where libhosi and hosi.h are (source).
  # HOSI_PRECOMPILED_URL             where the release's files are (a mirror, or a test server).

  @repo "https://github.com/sukhi-social/hosigraph"
  # The bundles are built with the oldest NIF API we support; a newer OTP loads them.
  @nif_min "2.17.0"
  @targets ~w(aarch64-darwin x86_64-linux aarch64-linux)

  def run(version, root) do
    priv = Path.join(root, "priv")

    case System.get_env("HOSI_BUILD") do
      "none" -> :ok
      "source" -> source(root, priv)
      "precompiled" -> precompiled(version, root, priv)
      nil -> if lib_dir(root), do: source(root, priv), else: precompiled(version, root, priv)
      other -> Mix.raise("HOSI_BUILD=#{other}: say source, precompiled or none")
    end
  end

  @doc "The file name of the bundle for this machine, e.g. hosi-nif-v0.0.1-nif2.17-aarch64-darwin.tar.gz"
  def asset(version, target \\ target()), do: "hosi-nif-v#{version}-nif2.17-#{target}.tar.gz"

  @doc "aarch64-darwin, x86_64-linux, ... or the reason there is none."
  def target do
    arch = :erlang.system_info(:system_architecture) |> to_string()
    cpu = arch |> String.split("-") |> hd() |> then(&if(&1 in ["arm64", "aarch64"], do: "aarch64", else: &1))

    os =
      cond do
        String.contains?(arch, "darwin") -> "darwin"
        String.contains?(arch, "musl") -> "musl"
        String.contains?(arch, "linux") -> "linux"
        true -> "other"
      end

    "#{cpu}-#{os}"
  end

  ## ---- source

  defp lib_dir(root) do
    System.get_env("HOSI_LIB_DIR") ||
      case Path.wildcard(Path.join(root, "../dist-newstyle/build/*/ghc-*/hosigraph-abi-*/f/hosi/build/hosi/libhosi.*")) do
        [file | _] -> Path.dirname(file)
        [] -> nil
      end
  end

  defp source(root, priv) do
    src = Path.join(root, "c_src/hosi_nif.c")
    out = Path.join(priv, "hosi_nif.so")
    include = System.get_env("HOSI_INCLUDE_DIR") || Path.expand("../hosigraph-abi/gen/include", root)
    lib_dir = lib_dir(root)

    unless lib_dir && File.exists?(Path.join(include, "hosi.h")) do
      Mix.raise("""
      libhosi not found. Build it (cabal build hosigraph-abi:flib:hosi) and set
        HOSI_LIB_DIR (where libhosi.dylib / libhosi.so is) and HOSI_INCLUDE_DIR (where hosi.h is),
      or leave HOSI_BUILD unset on a machine with no source, to download a prebuilt one.
      """)
    end

    if stale?(out, [src, Path.join(include, "hosi.h")]) do
      erts = Path.join([:code.root_dir(), "erts-#{:erlang.system_info(:version)}", "include"])
      darwin = match?({:unix, :darwin}, :os.type())
      # next to itself first (a bundle in priv/), then where it was built
      here = if darwin, do: "@loader_path", else: "$ORIGIN"
      rpaths = Enum.uniq([here, lib_dir]) |> Enum.reject(&(&1 == Path.expand(priv)))

      args =
        ["-shared", "-fPIC", "-O2", "-Wall", "-Wextra", "-I", erts, "-I", include, src, "-o", out,
         "-L", lib_dir, "-lhosi"] ++
          Enum.map(rpaths, &("-Wl,-rpath," <> &1)) ++
          if(darwin, do: ["-undefined", "dynamic_lookup"], else: [])

      File.mkdir_p!(priv)
      {output, status} = System.cmd(System.get_env("CC", "cc"), args, stderr_to_stdout: true)
      if status != 0, do: Mix.raise("cc failed:\n" <> output)
      if output != "", do: Mix.shell().info(output)
    end

    :ok
  end

  defp stale?(out, inputs) do
    case File.stat(out, time: :posix) do
      {:ok, %{mtime: t}} ->
        Enum.any?(inputs, fn f -> match?({:ok, %{mtime: m}} when m > t, File.stat(f, time: :posix)) end)

      _ ->
        true
    end
  end

  ## ---- precompiled

  defp precompiled(version, root, priv) do
    marker = Path.join(priv, ".precompiled")
    target = target()
    name = asset(version, target)

    if File.exists?(Path.join(priv, "hosi_nif.so")) and File.read(marker) == {:ok, name} do
      :ok
    else
      check_supported!(target)
      expected = expected_sha256!(root, name)
      url = (System.get_env("HOSI_PRECOMPILED_URL") || "#{@repo}/releases/download/hosi-ex-v#{version}") <> "/" <> name
      Mix.shell().info("hosi: downloading #{url}")
      body = download!(url)
      actual = :crypto.hash(:sha256, body) |> Base.encode16(case: :lower)

      if actual != expected,
        do: Mix.raise("hosi: #{name} has sha256 #{actual}, checksum.txt says #{expected}. Not using it.")

      File.rm_rf!(priv)
      File.mkdir_p!(priv)

      case :erl_tar.extract({:binary, body}, [:compressed, {:cwd, String.to_charlist(priv)}]) do
        :ok -> File.write!(marker, name)
        {:error, why} -> Mix.raise("hosi: cannot unpack #{name}: #{inspect(why)}")
      end
    end

    :ok
  end

  defp check_supported!(target) do
    nif = :erlang.system_info(:nif_version) |> to_string() |> Kernel.<>(".0")

    cond do
      target not in @targets ->
        Mix.raise("hosi: no prebuilt library for #{target} (there are: #{Enum.join(@targets, ", ")}). Build from source: see README.")

      Version.compare(nif, @nif_min) == :lt ->
        Mix.raise("hosi: the prebuilt NIF needs NIF API #{@nif_min} or newer (OTP 26+); this is #{nif}. Build from source: see README.")

      true ->
        :ok
    end
  end

  defp expected_sha256!(root, name) do
    file = Path.join(root, "checksum.txt")

    sha =
      with {:ok, text} <- File.read(file) do
        text
        |> String.split("\n", trim: true)
        |> Enum.find_value(fn line ->
          case String.split(line, "  ", parts: 2) do
            [sha, ^name] -> sha
            _ -> nil
          end
        end)
      else
        _ -> nil
      end

    sha ||
      Mix.raise("""
      hosi: #{name} is not in checksum.txt, so it cannot be checked, and is not downloaded.
      This happens on a checkout that is not a release. Build from source (see README),
      or use a tagged release.
      """)
  end

  defp download!(url) do
    {:ok, _} = Application.ensure_all_started(:inets)
    {:ok, _} = Application.ensure_all_started(:ssl)

    http_opts = [
      ssl: [
        verify: :verify_peer,
        cacerts: :public_key.cacerts_get(),
        customize_hostname_check: [match_fun: :public_key.pkix_verify_hostname_match_fun(:https)]
      ],
      autoredirect: true
    ]

    case :httpc.request(:get, {String.to_charlist(url), [{~c"user-agent", ~c"hosi-mix"}]}, http_opts, body_format: :binary) do
      {:ok, {{_, 200, _}, _, body}} -> body
      {:ok, {{_, status, _}, _, _}} -> Mix.raise("hosi: #{url} answered #{status}")
      {:error, why} -> Mix.raise("hosi: cannot fetch #{url}: #{inspect(why)}")
    end
  end
end
