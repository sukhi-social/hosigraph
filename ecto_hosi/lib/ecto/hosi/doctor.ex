defmodule Ecto.Hosi.Doctor do
  @moduledoc false
  # What `mix hosi.doctor` looks at: that the pieces a Repo needs are there, found by looking and
  # without opening the database. Each check is `{:ok | :fail | :warn, what, detail}`.

  alias Ecto.Hosi.{Config, Model}

  @java 21

  def checks(config) do
    [views(config)] ++ engine(config) ++ [directory(config), lock(config)]
  end

  defp views(config) do
    case Keyword.get(config, :schemas, []) do
      [] ->
        {:warn, "views", "the Repo has no :schemas, so nothing can be read or written through Ecto"}

      schemas ->
        model = Model.build(schemas)
        {:ok, "views", "#{map_size(model.views)} view(s), #{map_size(model.attrs)} attribute(s)"}
    end
  rescue
    e in [ArgumentError, UndefinedFunctionError] -> {:fail, "views", Exception.message(e)}
  end

  # libhosi and Datalevin: not asked of a Repo that talks to a stand-in
  defp engine(config) do
    case Keyword.get(config, :hosi, Hosi) do
      Hosi ->
        [libhosi() | datalevin(config)]

      other ->
        [{:warn, "libhosi", "the Repo uses hosi: #{inspect(other)}, so libhosi and Datalevin are not looked at"}]
    end
  end

  def libhosi do
    path = Path.join(to_string(:code.priv_dir(:hosi)), "hosi_nif.so")

    case Code.ensure_loaded(Hosi.Nif) do
      {:module, nif} ->
        {:ok, "libhosi", "loaded, ABI #{inspect(nif.abi_version())} (#{path})"}

      {:error, reason} ->
        why =
          if File.exists?(path),
            do: "#{path} is there but does not load (#{reason}): build it again (mix deps.compile hosi --force)",
            else: "#{path} is missing: see hosigraph-ex's README, \"Getting libhosi\" (HOSI_BUILD, HOSI_LIB_DIR)"

        {:fail, "libhosi", why}
    end
  catch
    _kind, reason -> {:fail, "libhosi", "cannot be asked: #{inspect(reason)}"}
  end

  # the khana hosigraph would be given
  defp datalevin(config) do
    case khana(config) do
      [:jvm, java, jar] ->
        [java(java), jar(jar)]

      [:"dtlv-repl", dtlv] ->
        [executable("dtlv", dtlv)]

      nil ->
        [java("java"), jar("datalevin-standalone.jar")]

      other ->
        [{:fail, "Datalevin", "khana #{inspect(other)} is not one hosigraph knows"}]
    end
  rescue
    e in ArgumentError -> [{:fail, "Datalevin", Exception.message(e)}]
  end

  defp khana(config) do
    case Config.khana(config) do
      nil -> config |> Keyword.get(:open, %{}) |> Map.new() |> get_in([:config, :khana])
      khana -> khana
    end
  end

  defp find(exe) do
    if String.contains?(exe, "/") do
      case File.stat(exe) do
        {:ok, %{type: :regular, mode: mode}} -> if Bitwise.band(mode, 0o111) != 0, do: exe
        _ -> nil
      end
    else
      System.find_executable(exe)
    end
  end

  defp executable(what, exe) do
    case find(exe) do
      nil -> {:fail, what, "no executable at #{inspect(exe)}"}
      path -> {:ok, what, path}
    end
  end

  defp java(java) do
    case find(java) do
      nil ->
        {:fail, "java", "no executable at #{inspect(java)}: install JDK #{@java} or newer, or say where: khana: [jvm: jar, java: path]"}

      path ->
        {out, status} = System.cmd(path, ["-version"], stderr_to_stdout: true)

        case Regex.run(~r/version "(\d+)/, out) do
          [_, major] when status == 0 ->
            if String.to_integer(major) >= @java,
              do: {:ok, "java", "#{path} (#{major})"},
              else: {:fail, "java", "#{path} is Java #{major}; Datalevin needs #{@java} or newer"}

          _ ->
            {:warn, "java", "#{path} did not say its version"}
        end
    end
  end

  defp jar(jar) do
    if File.regular?(jar),
      do: {:ok, "Datalevin jar", Path.expand(jar)},
      else: {:fail, "Datalevin jar", "no file at #{inspect(jar)}: khana: [jvm: \"/path/datalevin-standalone.jar\"]"}
  end

  defp dir(config), do: config[:dir] || config |> Keyword.get(:open, %{}) |> Map.new() |> Map.get(:dir)

  defp directory(config) do
    case dir(config) do
      nil ->
        {:fail, "directory", "the Repo has no :dir"}

      dir ->
        case File.stat(dir) do
          {:ok, %{type: :directory, access: access}} when access in [:read_write, :write] ->
            {:ok, "directory", Path.expand(dir)}

          {:ok, %{type: :directory}} ->
            {:fail, "directory", "#{dir} cannot be written to"}

          {:ok, _} ->
            {:fail, "directory", "#{dir} is not a directory"}

          {:error, :enoent} ->
            {:ok, "directory", "#{Path.expand(dir)} is not there yet: it is made at the first start"}

          {:error, reason} ->
            {:fail, "directory", "#{dir}: #{:file.format_error(reason)}"}
        end
    end
  end

  # one process opens a database (hosigraph holds `events/lock`); asked with flock(1), where there
  # is one, which takes the lock for a moment and lets go
  defp lock(config) do
    file = dir(config) && Path.join([dir(config), "events", "lock"])

    cond do
      is_nil(file) or not File.exists?(file) ->
        {:ok, "lock", "nothing has opened the database"}

      flock = System.find_executable("flock") ->
        case System.cmd(flock, ["--nonblock", "--exclusive", "--conflict-exit-code", "75", file, "true"], stderr_to_stdout: true) do
          {_, 0} -> {:ok, "lock", "free"}
          {_, 75} -> {:warn, "lock", "held: a process has the database open (only one can)"}
          {out, _} -> {:warn, "lock", "could not be asked: #{String.trim(out)}"}
        end

      true ->
        {:warn, "lock", "not asked (there is no flock command here)"}
    end
  end
end
