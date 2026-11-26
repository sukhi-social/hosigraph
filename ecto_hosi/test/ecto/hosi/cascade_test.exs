defmodule Ecto.Hosi.CascadeTest do
  use Ecto.Hosi.Case, async: true

  alias Ecto.Hosi.Test.{Entry, Item, Member, Note, Seat, Team}

  import Ecto.Query

  defp team!(name \\ "t"), do: Repo.insert!(%Team{name: name})
  defp member!(team, name \\ "m"), do: Repo.insert!(%Member{team_id: team.id, name: name})

  test "delete cascades through two levels" do
    t = team!()
    m = member!(t)
    Repo.insert!(%Item{member_id: m.id, name: "i"})
    other = member!(team!("o"))
    Repo.insert!(%Item{member_id: other.id, name: "keep"})

    Repo.delete!(t)

    assert Repo.all(Member) |> Enum.map(& &1.id) == [other.id]
    assert Repo.all(Item) |> Enum.map(& &1.name) == ["keep"]
  end

  test "delete_all cascades" do
    t = team!()
    m = member!(t)
    Repo.insert!(%Item{member_id: m.id, name: "i"})
    assert {1, _} = Repo.delete_all(from(x in Team, where: x.id == ^t.id))
    assert Repo.all(Member) == []
    assert Repo.all(Item) == []
  end

  test "write_batch delete cascades" do
    t = team!()
    member!(t)
    assert {:ok, _} = Repo.write_batch(delete: {:gone, t})
    assert Repo.all(Member) == []
  end

  test "nilify clears the key and keeps the row (and its derived unique)" do
    t = team!()
    n = Repo.insert!(%Note{team_id: t.id, name: "a"})
    Repo.delete!(t)
    assert %Note{team_id: nil, name: "a"} = Repo.get!(Note, n.id)
    # (team, name) no longer holds, so the same name can be taken again under a new team
    t2 = team!("t2")
    assert {:ok, _} = Repo.insert(%Note{team_id: t2.id, name: "a"})
  end

  test "restrict refuses, as a constraint error when declared" do
    t = team!()
    Repo.insert!(%Seat{team_id: t.id})

    assert_raise Ecto.ConstraintError, ~r/seat_team_fk/, fn -> Repo.delete(t) end

    cs = t |> Ecto.Changeset.change() |> Ecto.Changeset.foreign_key_constraint(:id, name: "seat_team_fk", message: "in use")
    assert {:error, cs} = Repo.delete(cs)
    assert [id: {"in use", _}] = cs.errors
    assert Repo.get(Team, t.id)

    assert_raise Ecto.Hosi.Error, ~r/seat_team_fk/, fn ->
      Repo.delete_all(from(x in Team, where: x.id == ^t.id))
    end

    assert {:error, :gone, {:foreign_key, "seat_team_fk"}} = Repo.write_batch(delete: {:gone, t})
  end

  test "a restricted delete deletes nothing, even what would cascade" do
    t = team!()
    member!(t)
    Repo.insert!(%Seat{team_id: t.id})
    assert_raise Ecto.ConstraintError, fn -> Repo.delete(t) end
    assert length(Repo.all(Member)) == 1
  end

  test "append-only rows are written, never changed" do
    e = Repo.insert!(%Entry{text: "x"})
    assert_raise ArgumentError, ~r/append-only/, fn -> e |> Ecto.Changeset.change(text: "y") |> Repo.update() end
    assert_raise ArgumentError, ~r/append-only/, fn -> Repo.delete(e) end
    assert_raise ArgumentError, ~r/append-only/, fn -> Repo.delete_all(Entry) end
    assert_raise ArgumentError, ~r/append-only/, fn -> Repo.update_all(Entry, set: [text: "z"]) end
    assert [%Entry{text: "x"}] = Repo.all(Entry)
  end
end
