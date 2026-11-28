defmodule Ecto.Hosi.AccountTest do
  # `Repo.erase_account/2`: what an account made goes, what others wrote stays, and the account is
  # kept as its tombstone.
  use Ecto.Hosi.Case, async: true

  alias Ecto.Changeset

  alias Ecto.Hosi.Test.{
    Actor,
    Following,
    Like,
    Media,
    Pinned,
    Profile,
    Report,
    Social,
    Status,
    Tombstone
  }

  setup do
    start_repo(schemas: Social.schemas())

    me = actor!("ada")
    other = actor!("bo")
    profile = Repo.insert!(%Profile{actor_id: me.id, display_name: "Ada L."})
    note = Repo.insert!(%Status{attributed_to_id: me.id, uri: "u:mine", content: "secret"})
    old = Repo.insert!(%Status{attributed_to_id: me.id, uri: "u:old", content: "old"})
    {:ok, _} = Repo.delete(old)
    media = Repo.insert!(%Media{attributed_to_id: me.id, status_id: note.id, path: "f/1"})
    reply = Repo.insert!(%Status{attributed_to_id: other.id, reply_to_id: note.id, content: "re"})
    mine = Repo.insert!(%Following{from_id: me.id, to_id: other.id})
    theirs = Repo.insert!(%Following{from_id: other.id, to_id: me.id})
    like = Repo.insert!(%Like{liker_id: other.id, status_id: note.id})

    %{
      me: me,
      other: other,
      owned: [profile.id, note.id, old.id, media.id, mine.id],
      note: note,
      reply: reply,
      theirs: theirs,
      like: like
    }
  end

  defp actor!(name),
    do: Repo.insert!(%Actor{username: name, uri: "https://x/users/#{name}", key: "k-#{name}"})

  defp ids, do: facts() |> Map.keys() |> Enum.map(&elem(&1, 0)) |> Enum.uniq()
  defp values, do: facts() |> Map.values() |> Enum.concat()

  defp deleting(answer \\ :ok) do
    test = self()

    fn paths ->
      send(test, {:files, paths})
      answer
    end
  end

  test "erases what the account made, follows what points at it, and keeps its tombstone",
       %{me: me, other: other, owned: owned, note: note, reply: reply, theirs: theirs, like: like} do
    assert {:ok, %{erased: erased, left: []}} = Repo.erase_account(me, delete_files: deleting())
    assert_received {:files, ["f/1"]}
    assert Enum.sort(erased) == Enum.sort(owned)

    # nothing of what it made is left, in now or in history
    for id <- owned do
      refute id in ids()
      assert Repo.history(id) == []
    end

    refute "secret" in values()
    refute "Ada L." in values()

    # others: the reply stays and points at what is gone; the like and the follow of the account go
    assert Repo.get!(Status, reply.id).reply_to_id == note.id
    refute like.id in ids()
    refute theirs.id in ids()
    assert Repo.get!(Actor, other.id).username == "bo"

    # the account: its tombstone, with the name still taken
    assert Repo.get(Actor, me.id) == nil
    assert %Tombstone{former_type: "person", uri: "https://x/users/ada"} = Repo.get!(Tombstone, me.id)
    refute "k-ada" in values()

    assert_raise Ecto.ConstraintError, fn -> Repo.insert(%Actor{username: "ada", uri: "u:new"}) end

    # run again: nothing more to do
    assert {:ok, %{left: []}} = Repo.erase_account(me.id, delete_files: deleting())
    refute_received {:files, _}
    assert %{"records-left": 0} = Repo.clean()
  end

  test "a file that cannot be deleted stops it, with nothing changed", %{me: me} do
    before = facts()

    assert {:error, {:files, :denied}} =
             Repo.erase_account(me, delete_files: deleting({:error, :denied}))

    assert facts() == before
  end

  test "files need somebody to delete them", %{me: me} do
    before = facts()
    assert_raise ArgumentError, ~r/delete_files/, fn -> Repo.erase_account(me) end
    assert facts() == before
  end

  test "a restrict reference to the account refuses before anything changes", %{me: me} do
    Repo.insert!(%Report{about_id: me.id})
    before = facts()

    assert {:error, {:foreign_key, "report_about_fk"}} =
             Repo.erase_account(me, delete_files: deleting())

    refute_received {:files, _}
    assert facts() == before
  end

  test "a restrict reference to an erased record is reported: an erase is not refused", %{me: me, note: note} do
    report = Repo.insert!(%Report{about_id: note.id})

    assert {:ok, %{left: [{id, "report_about_fk"}]}} =
             Repo.erase_account(me, delete_files: deleting())

    assert id == report.id
    assert Repo.get!(Report, report.id).about_id == note.id
  end

  test "an account needs a tombstone" do
    p = Repo.insert!(%Pinned{pinned_at: DateTime.utc_now()})
    assert_raise ArgumentError, ~r/keeps the account as a tombstone/, fn -> Repo.erase_account(p) end
    assert {:error, :stale} = Repo.erase_account(999_999)
  end

  test "not inside a transaction", %{me: me} do
    assert_raise ArgumentError, ~r/inside a transaction/, fn ->
      Repo.transaction(fn -> Repo.erase_account(me) end)
    end

    assert Repo.get(Actor, me.id)
  end

  test "a Repo.delete of the account alone leaves its records, and erase_account finishes", %{me: me, other: other} do
    {:ok, _} = Repo.delete(me)
    assert Repo.aggregate(from(s in Status, where: s.attributed_to_id == ^me.id), :count) == 1
    assert {:ok, _} = Repo.erase_account(me, delete_files: deleting())
    assert Repo.all(from s in Status, select: s.attributed_to_id) == [other.id]
    assert %Tombstone{former_type: "person"} = Repo.get!(Tombstone, me.id)
    assert {:ok, _} = Repo.update(Changeset.change(Repo.get!(Actor, other.id), key: "k2"))
  end
end
