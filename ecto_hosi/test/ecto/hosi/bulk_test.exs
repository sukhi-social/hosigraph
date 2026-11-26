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

  describe "schemaless insert_all" do
    test "dumps times, maps, arrays and binaries by the registered schema" do
      at = ~U[2026-01-02 03:04:05.000000Z]

      assert {1, _} =
               Repo.insert_all("doc", [
                 %{meta: %{"a" => 1}, tags: ["x"], blob: <<1, 2, 255>>}
               ])

      assert {1, _} = Repo.insert_all("user", [%{name: "N", email: "n@x", inserted_at: at, updated_at: at}])
      assert %{inserted_at: _} = Repo.one(from(u in User, where: u.name == "N"))
      assert [%{meta: %{"a" => 1}, tags: ["x"], blob: <<1, 2, 255>>}] = Repo.all(Doc)
    end
  end

  describe "schemaless insert_all defaults" do
    test "a field the row leaves out gets the schema's default" do
      assert {1, _} = Repo.insert_all("user", [%{name: "D", email: "d@x"}])
      assert %{active: true, lock_version: 1} = Repo.one(from(u in User, where: u.name == "D"))
    end
  end

  describe "values with no type to dump them" do
    test "a time in a schemaless query, and a time without microseconds in a _usec column" do
      at = ~N[2026-01-02 03:04:05]
      assert {1, _} = Repo.insert_all("user", [%{name: "T", email: "t@x", inserted_at: at, updated_at: ~U[2026-01-02 03:04:05Z]}])

      assert ["T"] ==
               Repo.all(from(u in "user", where: u.inserted_at <= ^~N[2026-01-02 03:04:05.000000], select: u.name))

      assert [] == Repo.all(from(u in "user", where: u.inserted_at < ^~N[2026-01-02 03:04:05], select: u.name))
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
    test "EXISTS and NOT EXISTS look at another table" do
      a = user!(%{name: "Ada"})
      b = user!(%{name: "Bob"})
      Repo.insert!(%Post{title: "t", user_id: a.id})

      with_posts =
        from(u in User,
          where: fragment("EXISTS (SELECT 1 FROM post p WHERE p.user_id = ?)", u.id),
          select: u.name
        )

      without =
        from(u in User,
          where: fragment("NOT EXISTS (SELECT 1 FROM post p WHERE p.user_id = ?)", u.id),
          select: u.name
        )

      assert Repo.all(with_posts) == ["Ada"]
      assert Repo.all(without) == ["Bob"]
      assert b.id
    end
  end
end
