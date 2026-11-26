defmodule Ecto.Hosi.CompositeTypesTest do
  use Ecto.Hosi.Case, async: true

  @attrs %{
    meta: %{"a" => 1, "nested" => %{"b" => [1, 2, "x"]}, "t" => true},
    tags: ["one", "two"],
    items: [%{"k" => "v"}, %{"k" => nil}],
    scores: [3, 1, 2],
    blob: <<0, 255, 1, 2, 128>>
  }

  defp doc! do
    %Doc{} |> Doc.changeset(@attrs) |> Repo.insert!()
  end

  test "maps, arrays and binaries round-trip" do
    doc = doc!()
    got = Repo.get!(Doc, doc.id)

    assert got.meta == @attrs.meta
    assert got.tags == @attrs.tags
    assert got.items == @attrs.items
    assert got.scores == @attrs.scores
    assert got.blob == @attrs.blob
  end

  test "they are kept as strings in hosigraph" do
    doc = doc!()
    f = facts()

    assert [meta] = f[{doc.id, :"doc/meta"}]
    assert is_binary(meta)
    assert JSON.decode!(meta) == @attrs.meta
    assert f[{doc.id, :"doc/blob"}] == [Base.encode64(@attrs.blob)]
  end

  test "update replaces them, an empty list and an empty map are kept" do
    doc = doc!()
    {:ok, _} = doc |> Doc.changeset(%{tags: [], meta: %{}}) |> Repo.update()

    got = Repo.get!(Doc, doc.id)
    assert got.tags == []
    assert got.meta == %{}
    assert got.items == @attrs.items
  end

  test "an unset field reads back as nil" do
    doc = Repo.insert!(%Doc{})
    got = Repo.get!(Doc, doc.id)
    assert got.meta == nil and got.tags == nil and got.blob == nil
  end

  test "where on the whole value matches" do
    doc = doc!()
    assert [%{id: id}] = Repo.all(from d in Doc, where: d.tags == ^["one", "two"])
    assert id == doc.id
    assert [] == Repo.all(from d in Doc, where: d.tags == ^["two", "one"])
  end

  test "update_all can set an array or a map column to a list parameter" do
    doc = doc!()
    tags = ["x", "y"]
    items = [%{"k" => 1}]

    {1, _} =
      Ecto.Query.from(d in Doc, where: d.id == ^doc.id, update: [set: [tags: ^tags, items: ^items]])
      |> Repo.update_all([])

    got = Repo.get!(Doc, doc.id)
    assert got.tags == tags
    assert got.items == items
  end
end
