defmodule Ecto.Hosi.BulkTest do
  use Ecto.Hosi.Case, async: true

  defp user!(attrs) do
    %User{}
    |> User.changeset(Map.merge(%{name: "n", email: "#{System.unique_integer([:positive])}@x"}, attrs))
    |> Repo.insert!()
  end

  describe "update_all" do
    test "set and inc on the rows a where matches, and only those" do
      a = user!(%{name: "Ada", age: 36})
      b = user!(%{name: "Bob", age: 40})
      c = user!(%{name: "Cy", age: 50})

      assert {2, nil} =
               Repo.update_all(from(u in User, where: u.age >= 40),
                 set: [name: "old"],
                 inc: [age: 1]
               )

      assert Repo.get!(User, a.id).name == "Ada"
      assert %{name: "old", age: 41} = Repo.get!(User, b.id)
      assert %{name: "old", age: 51} = Repo.get!(User, c.id)
    end

    test "with ^params, nil (retracts), and another field of the row" do
      a = user!(%{name: "Ada", age: 36})
      new = "Augusta"

      assert {1, nil} = Repo.update_all(from(u in User, where: u.id == ^a.id, update: [set: [name: ^new]]), [])
      assert Repo.get!(User, a.id).name == "Augusta"

      assert {1, nil} = Repo.update_all(from(u in User, where: u.id == ^a.id), set: [age: nil])
      assert Repo.get!(User, a.id).age == nil
      refute Map.has_key?(facts(), {a.id, :"user/age"}) and facts()[{a.id, :"user/age"}] != []

      assert {1, nil} =
               Repo.update_all(from(u in User, where: u.id == ^a.id, update: [set: [email: ^"e@x"]]), [])
    end

    test "dumps times and decimals through the field type" do
      a = user!(%{})
      at = ~U[2020-01-02 03:04:05.000000Z]

      assert {1, nil} =
               Repo.update_all(from(u in User, where: u.id == ^a.id, update: [set: [inserted_at: ^at]]), [])

      assert Repo.get!(User, a.id).inserted_at == at
    end

    test "no match is {0, nil}; the whole table without a where" do
      assert {0, nil} = Repo.update_all(User, set: [name: "x"])
      user!(%{})
      user!(%{})
      assert {2, nil} = Repo.update_all(User, set: [name: "x"])
      assert Repo.all(from(u in User, select: u.name)) == ["x", "x"]
    end
  end

  describe "delete_all" do
    test "removes the matching rows completely" do
      a = user!(%{age: 1})
      b = user!(%{age: 2})
      p = Repo.insert!(%Post{title: "t", user_id: b.id})

      assert {1, nil} = Repo.delete_all(from(u in User, where: u.age == 1))
      assert Repo.get(User, a.id) == nil
      assert Repo.get(User, b.id)
      assert Repo.get(Post, p.id)
      assert Enum.all?(facts(), fn {{id, _}, _} -> id != a.id end)
    end

    test "the whole table, and none" do
      assert {0, nil} = Repo.delete_all(User)
      user!(%{})
      user!(%{})
      assert {2, nil} = Repo.delete_all(User)
      assert Repo.all(User) == []
    end
  end

  describe "returning" do
    test "update_all returns the rows as they are after the change" do
      a = user!(%{name: "Ada", age: 1})
      user!(%{name: "Bob", age: 2})

      assert {1, [[id, "Ada!", 1]]} =
               Repo.update_all(
                 from(u in User, where: u.age == 1, select: [u.id, u.name, u.age], update: [set: [name: "Ada!"]]),
                 []
               )

      assert id == a.id
    end

    test "delete_all returns the rows it removed" do
      a = user!(%{name: "Ada"})

      assert {1, [%{id: id, name: "Ada"}]} =
               Repo.delete_all(from(u in User, where: u.id == ^a.id, select: %{id: u.id, name: u.name}))

      assert id == a.id
      assert Repo.get(User, a.id) == nil
    end
  end

  describe "by name" do
    test "nothing is written or read by name: a name says nothing about the facts" do
      assert_raise ArgumentError, ~r/does not write by name.*no tables/s, fn ->
        Repo.insert_all("user", [%{name: "N", email: "n@x"}])
      end

      assert_raise ArgumentError, ~r/does not query by name.*no tables/s, fn ->
        Repo.all(from(u in "user", select: u.name))
      end

      assert_raise ArgumentError, ~r/does not query by name/, fn ->
        Repo.update_all("user", set: [name: "x"])
      end

      assert_raise ArgumentError, ~r/does not query by name/, fn -> Repo.delete_all(from(u in "user")) end
      assert facts() == %{}
    end
  end

  describe "type/2 in a where" do
    test "a tagged parameter is read as its value" do
      a = user!(%{name: "Tg", age: 3})
      n = 3

      assert [a.id] ==
               Repo.all(from(u in User, where: u.age == type(^n, :integer), select: u.id))
    end
  end

  describe "EXISTS fragments" do
    test "are not offered (Datalog says them)" do
      user!(%{name: "Ada"})

      assert_raise ArgumentError, ~r/cannot evaluate the fragment.*Repo.datalog/s, fn ->
        Repo.all(
          from(u in User,
            where: fragment("EXISTS (SELECT 1 FROM post p WHERE p.user_id = ?)", u.id),
            select: u.name
          )
        )
      end
    end
  end
end
