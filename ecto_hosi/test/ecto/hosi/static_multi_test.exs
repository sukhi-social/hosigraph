defmodule Ecto.Hosi.StaticMultiTest do
  use Ecto.Hosi.Case, async: true

  alias Ecto.Multi
  alias Ecto.Hosi.StaticMulti

  defp user!(attrs \\ %{}) do
    %User{}
    |> User.changeset(Map.merge(%{name: "n", email: "#{System.unique_integer([:positive])}@x"}, attrs))
    |> Repo.insert!()
  end

  defp cs(attrs), do: User.changeset(%User{}, Map.merge(%{name: "n"}, attrs))

  describe "what compiles" do
    test "insert, update and delete steps with known changesets" do
      u = user!()

      multi =
        Multi.new()
        |> Multi.insert(:a, cs(%{email: "a@x"}))
        |> Multi.update(:b, User.changeset(u, %{name: "changed"}))
        |> Multi.delete(:c, u)

      assert {:ok, [insert: {:a, _}, update: {:b, _}, delete: {:c, _}]} = StaticMulti.plan(multi)
    end

    test "not a run, a function, update_all, options, or an empty or associated changeset" do
      refute match?({:ok, _}, StaticMulti.plan(Multi.new()))
      refute match?({:ok, _}, StaticMulti.plan(Multi.run(Multi.new(), :r, fn _, _ -> {:ok, 1} end)))
      refute match?({:ok, _}, StaticMulti.plan(Multi.insert(Multi.new(), :f, fn _ -> cs(%{}) end)))

      refute match?(
               {:ok, _},
               StaticMulti.plan(Multi.insert(Multi.new(), :o, cs(%{}), on_conflict: :nothing))
             )

      refute match?(
               {:ok, _},
               StaticMulti.plan(Multi.update_all(Multi.new(), :u, User, set: [name: "x"]))
             )

      assign = Ecto.Changeset.put_assoc(cs(%{}), :posts, [%Post{title: "t"}])
      refute match?({:ok, _}, StaticMulti.plan(Multi.insert(Multi.new(), :p, assign)))
    end
  end

  describe "Repo.transaction(multi)" do
    test "a static multi gives the same result as a step-by-step one" do
      u = user!(%{name: "old"})
      gone = user!()

      assert {:ok, %{a: a, b: b, c: c}} =
               Repo.transaction(
                 Multi.new()
                 |> Multi.insert(:a, cs(%{email: "a@x"}))
                 |> Multi.update(:b, User.changeset(u, %{name: "new"}))
                 |> Multi.delete(:c, gone)
               )

      assert %User{id: id, __meta__: %{state: :loaded}} = a
      assert Repo.get!(User, id).email == "a@x"
      assert b.name == "new" and Repo.get!(User, u.id).name == "new"
      assert c.__meta__.state == :deleted and Repo.get(User, gone.id) == nil
    end

    test "all or nothing: a step that cannot be made stops the lot, no changes before it" do
      user!(%{email: "taken@x"})

      multi =
        Multi.new()
        |> Multi.insert(:ok, cs(%{email: "fine@x"}))
        |> Multi.insert(:dup, cs(%{email: "taken@x"}))

      assert {:error, :dup, %Ecto.Changeset{errors: [email: {_, _}]}, %{}} =
               Repo.transaction(multi)

      assert Repo.all(from(u in User, select: u.email)) == ["taken@x"]
    end

    test "an invalid changeset fails its step" do
      multi =
        Multi.new()
        |> Multi.insert(:ok, cs(%{email: "fine@x"}))
        |> Multi.insert(:bad, User.changeset(%User{}, %{}))

      assert {:error, :bad, %Ecto.Changeset{valid?: false}, %{}} = Repo.transaction(multi)
      assert Repo.aggregate(User, :count) == 0
    end

    test "it is one write, not a transaction of steps" do
      Repo.transaction(Multi.new() |> Multi.insert(:a, cs(%{email: "a@x"})))
      refute Repo.in_transaction?()
    end

    test "a multi that is not static still runs, step by step, undone on failure" do
      multi =
        Multi.new()
        |> Multi.insert(:u, cs(%{email: "a@x"}))
        |> Multi.run(:p, fn repo, %{u: u} -> repo.insert(%Post{title: "t", user_id: u.id}) end)
        |> Multi.run(:fail, fn _, _ -> {:error, :bad} end)

      assert {:error, :fail, :bad, %{u: _, p: _}} = Repo.transaction(multi)
      assert Repo.aggregate(User, :count) == 0
      assert Repo.aggregate(Post, :count) == 0
    end

    test "a function is passed through untouched" do
      assert {:ok, 1} = Repo.transaction(fn -> 1 end)
    end
  end
end
