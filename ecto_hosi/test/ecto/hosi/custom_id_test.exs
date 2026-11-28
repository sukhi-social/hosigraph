defmodule Ecto.Hosi.CustomIdTest do
  # An id may be a custom Ecto type over `:id`: what the Repo holds and what the app sees differ.
  use Ecto.Hosi.Case, async: true

  alias Ecto.Hosi.Test.{Crate, Parcel, Shifted}
  alias Ecto.Multi

  defp crate(label), do: Repo.insert!(%Crate{label: label})
  defp parcel(c, rank), do: Repo.insert!(%Parcel{crate_id: c.id, rank: rank})
  defp change(row, attrs), do: Ecto.Changeset.change(row, attrs)

  test "the Repo holds the dumped id, the app sees the loaded one" do
    c = crate("a")
    p = parcel(c, 1)

    assert facts()[{p.id + Shifted.shift(), :"parcel/crate_id"}] == [c.id + Shifted.shift()]
    assert Repo.get(Parcel, p.id).crate_id == c.id
    assert Repo.all(from p in Parcel, where: p.crate_id == ^c.id, select: p.id) == [p.id]
    assert {:ok, _} = p |> change(rank: 2) |> Repo.update()
    assert Repo.get(Parcel, p.id).rank == 2
    assert {:ok, _} = Repo.delete(Repo.get(Parcel, p.id))
    assert Repo.get(Parcel, p.id) == nil
  end

  test "write_batch inserts return the app's ids, refs included" do
    assert {:ok, %{crate: c, parcel: p}} =
             Repo.write_batch(
               insert: {:crate, change(%Crate{}, label: "b")},
               insert: {:parcel, change(%Parcel{}, rank: 1), refs: [crate_id: :crate]}
             )

    assert Repo.get(Crate, c.id).label == "b"
    assert Repo.get(Parcel, p.id).crate_id == c.id
    assert p.crate_id == c.id
  end

  test "write_batch updates and deletes the row the app named" do
    c = crate("c")
    p = parcel(c, 1)

    assert {:ok, %{p: updated}} = Repo.write_batch(update: {:p, change(p, rank: 9)})
    assert updated.id == p.id
    assert Repo.get(Parcel, p.id).rank == 9

    assert {:ok, _} = Repo.write_batch(delete: {:p, updated})
    assert Repo.get(Parcel, p.id) == nil
  end

  test "a Multi of changesets reaches the same rows" do
    c = crate("d")
    p = parcel(c, 1)

    assert {:ok, _} =
             Multi.new()
             |> Multi.update(:p, change(p, rank: 3))
             |> Multi.delete(:c, c)
             |> Repo.transaction()

    assert Repo.get(Parcel, p.id).rank == 3
    assert Repo.get(Crate, c.id) == nil
  end

  test "highest_in speaks the app's ids, and takes one back as `after:`" do
    c = crate("e")
    [one, two, three] = for n <- 1..3, do: parcel(c, n)

    assert [first, second] = Repo.highest_in(Parcel, {:crate_id, :rank}, [c.id], 2)
    assert Enum.map([first, second], & &1.id) == [three.id, two.id]
    assert first.key == c.id

    assert [%{id: id}] = Repo.highest_in(Parcel, {:crate_id, :rank}, [c.id], 2, after: second)
    assert id == one.id
  end

  test "highest returns structs with the app's ids" do
    c = crate("f")
    p = parcel(c, 1)

    assert {[got], _} = Repo.highest(:"parcel/rank", 1, load: Parcel)
    assert {got.id, got.crate_id} == {p.id, c.id}
  end
end
