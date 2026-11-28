defmodule Ecto.Hosi.EngineRefusalsTest do
  # The stand-in refuses what hosigraph refuses, in the same words' kind. These run against both.
  use Ecto.Hosi.Case, async: true

  setup do
    start_repo(schemas: Ecto.Hosi.Test.Social.schemas())
    :ok
  end

  defp backend, do: Application.fetch_env!(:ecto_hosi, Ecto.Hosi.Test.Repo)[:hosi]
  defp write(ops, opts \\ []), do: backend().write(conn(), ops, opts)

  defp conn, do: Ecto.Hosi.Case.conn()

  defp new! do
    {:ok, %{temps: [[1, id]]}} = write([[:create, [:tmp, 1], :content, "x"]])
    id
  end

  test "writes" do
    id = new!()

    assert {:error, {:app, :"bad-argument", "write: no operations"}} = write([])
    assert {:error, {:app, :"bad-argument", message}} = write([[:create, 1, :content, "y"]])
    assert message =~ "not a valid entity id"

    assert {:error, {:app, :"bad-argument", _}} =
             write([[:create, id, :content, "y"]], when: [[:unset, 1, :content]])

    assert {:error, {:app, :"write-rejected", _}} =
             write([[:create, id, :"ap/published", "yesterday"]])

    assert {:error, {:app, :"write-rejected", _}} = write([[:create, id, :"ap/uuid", "no"]])

    assert {:ok, _} = write([[:create, id, :"ap/uri", "u:1"]])
    assert {:error, {:app, :"write-rejected", message}} = write([[:create, new!(), :"ap/uri", "u:1"]])
    assert message =~ "unique"

    # a tuple is the khana's own
    tuple = :"hosi/ap.attributed-to+ap.published"
    assert {:error, {_, _, _}} = write([[:create, id, tuple, 1]])

    backend().erase(conn(), [[:ids, [id]]], [])
    assert {:error, {:app, :"bad-argument", message}} = write([[:create, id, :content, "again"]])
    assert message =~ "was erased"
    assert {:ok, [%{} = gone]} = backend().get_entities(conn(), [id])
    assert gone == %{}
  end

  test "highest, highest_in and erase" do
    id = new!()
    refused = fn answer -> assert {:error, {:app, :"bad-argument", _}} = answer end

    refused.(backend().highest(conn(), :"ap/published", 0, []))
    refused.(backend().highest(conn(), :undeclared_here, 1, []))
    refused.(backend().highest(conn(), :"hosi/ap.attributed-to+ap.published", 1, []))
    refused.(backend().highest(conn(), :"ap/published", 1, after: %{val: "x", id: id}))
    refused.(backend().highest_in(conn(), :"ap/published", [id], 1, []))
    refused.(backend().highest_in(conn(), :"hosi/ap.attributed-to+ap.published", ["x"], 1, []))
    refused.(backend().erase(conn(), [[:ids, [1]]], []))
    assert {:ok, []} = backend().highest(conn(), :"ap/published", 1, [])
  end
end
