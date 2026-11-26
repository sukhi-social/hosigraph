defmodule Ecto.Hosi.TransactionTest do
  use Ecto.Hosi.Case, async: true

  alias Ecto.Multi

  defp user!(attrs \\ %{}) do
    %User{}
    |> User.changeset(Map.merge(%{name: "n", email: "#{System.unique_integer([:positive])}@x"}, attrs))
    |> Repo.insert!()
  end

  # the facts of the rows, without hosigraph's own bookkeeping
  defp rows do
    Map.reject(facts(), fn {{_, attr}, _} -> String.starts_with?(Atom.to_string(attr), "hosi.meta/") end)
  end

  test "a commit keeps everything and returns the value" do
    assert {:ok, %User{id: id}} =
             Repo.transaction(fn ->
               u = user!()
               Repo.insert!(%Post{title: "t", user_id: u.id})
               u
             end)

    assert Repo.get(User, id)
    assert Repo.aggregate(Post, :count) == 1
  end

  test "rollback undoes inserts, updates and deletes" do
    keep = user!(%{name: "keep", age: 1})
    gone = user!(%{name: "gone"})
    before = rows()

    assert {:error, :boom} =
             Repo.transaction(fn ->
               new = user!(%{name: "new"})
               Repo.insert!(%Post{title: "p", user_id: new.id})
               keep |> User.changeset(%{name: "changed", age: 2}) |> Repo.update!()
               Repo.delete!(gone)
               Repo.rollback(:boom)
             end)

    assert Repo.aggregate(User, :count) == 2
    assert Repo.aggregate(Post, :count) == 0
    assert %{name: "keep", age: 1} = Repo.get!(User, keep.id)
    assert %{name: "gone"} = Repo.get!(User, gone.id)
    assert rows() == before
  end

  test "an exception undoes the writes and is re-raised" do
    assert_raise RuntimeError, "oops", fn ->
      Repo.transaction(fn ->
        user!()
        raise "oops"
      end)
    end

    assert Repo.aggregate(User, :count) == 0
    refute Repo.in_transaction?()
  end

  test "update_all and delete_all inside are undone too" do
    a = user!(%{age: 1})

    assert {:error, :x} =
             Repo.transaction(fn ->
               Repo.update_all(User, inc: [age: 10])
               Repo.delete_all(User)
               Repo.rollback(:x)
             end)

    assert %{age: 1} = Repo.get!(User, a.id)
  end

  test "Ecto.Multi: a failing step undoes the earlier ones" do
    user!(%{email: "taken@x"})

    # a Multi.run makes it not static, so it runs step by step and is undone (static ones: StaticMultiTest)
    multi =
      Multi.new()
      |> Multi.run(:not_static, fn _, _ -> {:ok, 1} end)
      |> Multi.insert(:ok, User.changeset(%User{}, %{name: "fine", email: "fine@x"}))
      |> Multi.insert(:dup, User.changeset(%User{}, %{name: "dup", email: "taken@x"}))

    assert {:error, :dup, %Ecto.Changeset{}, %{ok: %User{}}} = Repo.transaction(multi)
    assert Repo.aggregate(User, :count) == 1
    assert Repo.all(from(u in User, select: u.email)) == ["taken@x"]
  end

  test "Ecto.Multi commits when every step passes" do
    multi =
      Multi.new()
      |> Multi.insert(:u, User.changeset(%User{}, %{name: "a", email: "a@x"}))
      |> Multi.run(:p, fn repo, %{u: u} -> repo.insert(%Post{title: "t", user_id: u.id}) end)

    assert {:ok, %{u: u, p: p}} = Repo.transaction(multi)
    assert p.user_id == u.id
  end

  test "a nested rollback fails the outer transaction" do
    assert {:error, :rollback} =
             Repo.transaction(fn ->
               user!()
               assert {:error, :inner} = Repo.transaction(fn -> Repo.rollback(:inner) end)
               :still_running
             end)

    assert Repo.aggregate(User, :count) == 0
  end

  test "rollback outside a transaction is an error" do
    assert_raise ArgumentError, ~r/outside of a transaction/, fn -> Repo.rollback(:x) end
  end
end
