defmodule Hosi.Nif do
  @moduledoc false
  # The doors of libhosi, one for one. Use `Hosi`, not this.
  @on_load :load_nif

  def load_nif do
    path = :filename.join(:code.priv_dir(:hosi), ~c"hosi_nif")
    :erlang.load_nif(path, 0)
  end

  def init, do: :erlang.nif_error(:not_loaded)
  def open(_uri), do: :erlang.nif_error(:not_loaded)
  def submit(_conn, _id, _body, _receiver), do: :erlang.nif_error(:not_loaded)
  def cancel(_conn, _id), do: :erlang.nif_error(:not_loaded)
  def close(_conn), do: :erlang.nif_error(:not_loaded)
  def abi_version, do: :erlang.nif_error(:not_loaded)
end
