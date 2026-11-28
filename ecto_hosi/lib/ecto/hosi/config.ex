defmodule Ecto.Hosi.Config do
  @moduledoc false
  # What hosigraph is opened with, from the Repo's configuration. Also used by `mix hosi.doctor`.

  @doc "The `open:` map: `:open`, with `:dir`, `:khana` and the schema of the views."
  def open(config, model) do
    open = config |> Keyword.get(:open, %{}) |> Map.new()

    open = if config[:dir], do: Map.put_new(open, :dir, config[:dir]), else: open

    open =
      case khana(config) do
        nil -> open
        khana -> put_khana(open, khana)
      end

    own = Map.get(open, :schema, %{})

    for {attr, spec} <- own, Map.has_key?(model.schema, attr), model.schema[attr] != spec do
      raise ArgumentError,
            "open: schema: declares #{inspect(attr)} as #{inspect(spec)}, and the views as " <>
              "#{inspect(model.schema[attr])}. A name has one definition"
    end

    Map.put(open, :schema, Map.merge(model.schema, own))
  end

  defp put_khana(open, khana) do
    given = open |> Map.get(:config, %{}) |> Map.new()

    case given do
      %{khana: other} when other != khana ->
        raise ArgumentError,
              "the Repo says where \"now\" is kept twice: khana: and open: %{config: %{khana: " <>
                "#{inspect(other)}}} do not agree. Keep one of them"

      _ ->
        Map.put(open, :config, Map.put(given, :khana, khana))
    end
  end

  @doc """
  hosigraph's `:khana` from the Repo's short form, or nil when `:khana` is not given:
  `[jvm: jar]`, `[jvm: jar, java: java]` or `[dtlv: executable]`.
  """
  def khana(config) do
    case Keyword.get(config, :khana) do
      nil ->
        nil

      short ->
        cond do
          Keyword.keyword?(short) and Keyword.keys(short) -- [:jvm, :java] == [] and
              is_binary(short[:jvm]) ->
            [:jvm, short[:java] || "java", short[:jvm]]

          Keyword.keyword?(short) and Keyword.keys(short) == [:dtlv] and is_binary(short[:dtlv]) ->
            [:"dtlv-repl", short[:dtlv]]

          true ->
            raise ArgumentError,
                  "khana: is [jvm: \"/path/datalevin-standalone.jar\"] (with java: \"/path/java\" " <>
                    "if java is not on the PATH) or [dtlv: \"/path/dtlv\"], got #{inspect(short)}"
        end
    end
  end
end
