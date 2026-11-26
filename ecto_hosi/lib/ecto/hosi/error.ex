defmodule Ecto.Hosi.Error do
  @moduledoc "Raised when hosigraph answers with something the adapter cannot turn into an Ecto result."
  defexception [:message, :reason]

  @impl true
  def exception(opts) when is_list(opts) do
    reason = Keyword.get(opts, :reason)
    msg = Keyword.get(opts, :message) || "hosigraph error: " <> inspect(reason)
    %__MODULE__{message: msg, reason: reason}
  end
end
