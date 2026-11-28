defmodule Ecto.Hosi.DeleteScopeTest do
  # A delete removes every fact of the record. So it is refused when the record also holds facts
  # that are not the schema's: another schema's, or a direct write's.
  use Ecto.Hosi.Case, async: true

  alias Ecto.Changeset
  alias Ecto.Hosi.Test.{Member, Team}
  alias Ecto.Multi

  defp user! do
    Repo.insert!(%User{name: "Ada", email: "#{System.unique_integer([:positive])}@x"})
  end

  defp followed_user! do
    u = user!()
    direct_write([[:create, u.id, :"follow/is", true], [:create, u.id, :"follow/from", 7]])
    u
  end

  defp facts_of(id), do: for({{^id, a}, v} <- facts(), into: %{}, do: {a, v})

  test "Repo.delete is refused, names the other facts, and writes nothing" do
    u = followed_user!()
    before = facts_of(u.id)

    error = assert_raise Ecto.Hosi.Error, fn -> Repo.delete(u) end

    assert error.message =~ "#{u.id}"
    assert error.message =~ ":follow/from, :follow/is"
    assert error.message =~ "ecto_hosi's limit"
    assert facts_of(u.id) == before
  end

  test "a name without a namespace is not the schema's either" do
    u = user!()
    direct_write([[:create, u.id, :pinned_at, 1]])

    assert_raise Ecto.Hosi.Error, ~r/:pinned_at/, fn -> Repo.delete(u) end
    assert Repo.get(User, u.id)
  end

  test "once the other facts are gone, the delete goes through" do
    u = followed_user!()
    direct_write([[:delete, u.id, :"follow/is", true], [:delete, u.id, :"follow/from", 7]])

    assert {:ok, _} = Repo.delete(u)
    assert facts_of(u.id) == %{}
  end

  test "a name that looks like the view's, but is not a field of it, is another fact" do
    u = user!()
    direct_write([[:create, u.id, :"user/left_over", "x"]])

    assert_raise Ecto.Hosi.Error, ~r/:user\/left_over/, fn -> Repo.delete(u) end
    assert Repo.get(User, u.id)
  end

  test "delete_all is refused as a whole" do
    plain = user!()
    u = followed_user!()

    assert_raise Ecto.Hosi.Error, ~r/:follow\/from/, fn -> Repo.delete_all(User) end
    assert Repo.get(User, plain.id)
    assert Repo.get(User, u.id)
  end

  test "write_batch is refused as a whole, and names the step" do
    u = followed_user!()

    assert {:error, :gone, {:refused, %{id: id, reason: :other_facts, attributes: attributes}}} =
             Repo.write_batch(
               insert: {:new, Changeset.change(%User{}, name: "New", email: "new@x")},
               delete: {:gone, u}
             )

    assert id == u.id
    assert attributes == [:"follow/from", :"follow/is"]
    assert Repo.aggregate(User, :count) == 1
  end

  test "a Multi raises, and nothing is written" do
    u = followed_user!()

    assert_raise Ecto.Hosi.Error, ~r/step :gone.*:follow\/from/s, fn ->
      Multi.new() |> Multi.delete(:gone, u) |> Repo.transaction()
    end

    assert Repo.get(User, u.id)
  end

  test "a cascade does not take other facts with it" do
    team = Repo.insert!(%Team{name: "t"})
    member = Repo.insert!(%Member{team_id: team.id, name: "m"})
    direct_write([[:create, member.id, :note_to_self, "keep me"]])

    assert_raise Ecto.Hosi.Error, ~r/:note_to_self/, fn -> Repo.delete(team) end
    assert Repo.get(Team, team.id)
    assert Repo.get(Member, member.id)
  end
end
