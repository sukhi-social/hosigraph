defmodule Ecto.Adapters.HosiTest do
  use Ecto.Hosi.Case, async: true

  defp user!(attrs \\ %{}) do
    %User{}
    |> User.changeset(Map.merge(%{name: "Ada", email: "ada@example.com", age: 36}, attrs))
    |> Repo.insert!()
  end

  describe "how rows are kept" do
    test "a row is facts: a marker, one attribute per field, :ref for foreign keys" do
      user = user!(%{score: 1.5, born: ~D[1815-12-10], balance: Decimal.new("12.50")})
      post = Repo.insert!(%Post{title: "Notes", user_id: user.id})

      assert is_integer(user.id)
      assert user.__meta__.state == :loaded

      f = facts()
      assert f[{user.id, :"user/ecto-kind"}] == [true]
      assert f[{user.id, :"user/name"}] == ["Ada"]
      assert f[{user.id, :"user/age"}] == [36]
      assert f[{user.id, :"user/score"}] == [1.5]
      assert f[{user.id, :"user/born"}] == [Date.diff(~D[1815-12-10], ~D[1970-01-01])]

      assert f[{user.id, :"user/balance"}] == ["12.5"] or
               f[{user.id, :"user/balance"}] == ["12.50"]

      assert [micros] = f[{user.id, :"user/inserted_at"}]
      assert is_integer(micros)
      assert f[{post.id, :"post/user_id"}] == [user.id]
    end

    @tag :fake_only
    test "the hosigraph schema is derived from the Ecto schemas" do
      schema = Fake.dump(Ecto.Hosi.Case.conn()).open.schema

      assert schema[:"user/email"] == %{type: :string, unique: :value}
      assert schema[:"user/age"] == %{type: :long}
      assert schema[:"user/score"] == %{type: :double}
      assert schema[:"user/inserted_at"] == %{type: :long}
      assert schema[:"post/user_id"] == %{type: :ref}
      assert schema[:"post/user_id"] != %{type: :long}
      assert schema[:"user/ecto-kind"] == %{type: :boolean}
    end

    test "values come back as the Ecto types they were" do
      user = user!(%{score: 2.0, born: ~D[1815-12-10], balance: Decimal.new("3.14")})
      loaded = Repo.get!(User, user.id)

      assert loaded.born == ~D[1815-12-10]
      assert Decimal.equal?(loaded.balance, Decimal.new("3.14"))
      assert loaded.score == 2.0
      assert %DateTime{} = loaded.inserted_at
      assert loaded.inserted_at == user.inserted_at
      assert loaded.active == true
    end
  end

  describe "reading" do
    setup do
      ada = user!(%{name: "Ada", email: "a@x", age: 36})
      bob = user!(%{name: "Bob", email: "b@x", age: 25})
      cy = user!(%{name: "Cy", email: "c@x", age: 40})
      eve = user!(%{name: "Eve", email: "e@x", age: nil})
      Repo.insert!(%Post{title: "p1", user_id: ada.id})
      %{ada: ada, bob: bob, cy: cy, eve: eve}
    end

    test "get, get_by, one, exists?", %{ada: ada} do
      assert Repo.get(User, ada.id).name == "Ada"
      assert Repo.get_by(User, name: "Bob", age: 25).name == "Bob"
      assert Repo.get_by(User, name: "Bob", age: 99) == nil
      assert Repo.one(from(u in User, where: u.name == "Cy")).age == 40
      assert Repo.exists?(from(u in User, where: u.age > 39))
      refute Repo.exists?(from(u in User, where: u.age > 99))
    end

    test "a row of another schema is not found by this schema's id", %{ada: _} do
      post = Repo.one!(Post)
      assert Repo.get(User, post.id) == nil
    end

    test "all returns the rows of one schema, oldest first" do
      assert Repo.all(User) |> Enum.map(& &1.name) == ["Ada", "Bob", "Cy", "Eve"]
      assert Repo.all(Post) |> length() == 1
    end

    test "comparisons" do
      names = fn q -> q |> Repo.all() |> Enum.map(& &1.name) end
      assert names.(from(u in User, where: u.age > 30)) == ["Ada", "Cy"]
      assert names.(from(u in User, where: u.age >= 36)) == ["Ada", "Cy"]
      assert names.(from(u in User, where: u.age < 36)) == ["Bob"]
      assert names.(from(u in User, where: u.age != 36)) == ["Bob", "Cy"]
      assert names.(from(u in User, where: 30 < u.age)) == ["Ada", "Cy"]
    end

    test "== with parameters, in, is_nil, not, and, or" do
      names = fn q -> q |> Repo.all() |> Enum.map(& &1.name) end
      who = "Bob"
      ages = [25, 40]

      assert names.(from(u in User, where: u.name == ^who)) == ["Bob"]
      assert names.(from(u in User, where: u.age in ^ages)) == ["Bob", "Cy"]
      assert names.(from(u in User, where: u.age in [36, 99])) == ["Ada"]
      assert names.(from(u in User, where: u.age in ^[])) == []
      assert names.(from(u in User, where: is_nil(u.age))) == ["Eve"]
      assert names.(from(u in User, where: not is_nil(u.age))) == ["Ada", "Bob", "Cy"]
      # `not` means "there is no such fact": a row with no age at all passes (unlike SQL's NULL)
      assert names.(from(u in User, where: not (u.age > 30))) == ["Bob", "Eve"]
      # `!=` needs the fact to exist, so it does not
      assert names.(from(u in User, where: u.age != 36)) == ["Bob", "Cy"]
      assert names.(from(u in User, where: u.age > 30 and u.name != "Cy")) == ["Ada"]
      assert names.(from(u in User, where: u.name == "Ada" or u.name == "Cy")) == ["Ada", "Cy"]
      assert names.(from(u in User, where: u.age < 30 or u.name == ^"Ada")) == ["Ada", "Bob"]
      assert names.(from(u in User, where: u.age > 30, where: u.age < 40)) == ["Ada"]
      assert names.(from(u in User, where: u.age > 100, or_where: u.name == "Eve")) == ["Eve"]
    end

    test "order_by, limit, offset" do
      q = from(u in User, where: not is_nil(u.age), order_by: [desc: u.age], select: u.name)
      assert Repo.all(q) == ["Cy", "Ada", "Bob"]

      assert Repo.all(
               from(u in User,
                 where: not is_nil(u.age),
                 order_by: [desc: u.age],
                 limit: 2,
                 select: u.name
               )
             ) == ["Cy", "Ada"]

      assert Repo.all(
               from(u in User, order_by: [asc: u.name], limit: 2, offset: 1, select: u.name)
             ) == ["Bob", "Cy"]

      n = 1
      assert Repo.all(from(u in User, order_by: u.name, limit: ^n, select: u.name)) == ["Ada"]
    end

    test "select fields, tuples and count" do
      assert Repo.all(from(u in User, where: u.name == "Ada", select: {u.id, u.name, u.age}))
             |> hd()
             |> elem(1) == "Ada"

      assert Repo.all(from(u in User, order_by: u.id, select: u.name)) == [
               "Ada",
               "Bob",
               "Cy",
               "Eve"
             ]

      assert Repo.aggregate(User, :count) == 4
      assert Repo.aggregate(from(u in User, where: u.age > 30), :count) == 2
      assert Repo.all(from(u in User, where: u.age == 25, select: %{n: u.name})) == [%{n: "Bob"}]
    end

    test "preload follows foreign keys both ways", %{ada: ada} do
      user = Repo.get!(User, ada.id) |> Repo.preload(:posts)
      assert [%Post{title: "p1"}] = user.posts

      post = Repo.one!(Post) |> Repo.preload(:user)
      assert post.user.name == "Ada"

      users = Repo.all(User) |> Repo.preload(:posts)
      assert Enum.map(users, &length(&1.posts)) == [1, 0, 0, 0]
    end

    test "what a table-less store cannot do says so" do
      assert_raise ArgumentError, ~r/subquery.*Repo.datalog/s, fn ->
        Repo.all(from(u in subquery(from(u in User, select: %{id: u.id})), select: u.id))
      end

      assert_raise ArgumentError, ~r/windows.*Repo.datalog/s, fn ->
        Repo.all(from(u in User, windows: [w: [partition_by: u.age]], select: count(u.id) |> over(:w)))
      end

    end
  end

  describe "update" do
    test "changes only what changed; the old value is replaced" do
      user = user!()
      {:ok, updated} = user |> User.changeset(%{name: "Augusta", age: 37}) |> Repo.update()

      assert updated.name == "Augusta"
      assert Repo.get!(User, user.id).age == 37
      assert facts()[{user.id, :"user/name"}] == ["Augusta"]
      assert facts()[{user.id, :"user/email"}] == ["ada@example.com"]
    end

    test "setting a field to nil retracts its fact" do
      user = user!()
      {:ok, _} = user |> User.changeset(%{age: nil}) |> Repo.update()

      refute Map.has_key?(facts(), {user.id, :"user/age"})
      assert Repo.get!(User, user.id).age == nil
    end

    test "updating a row that was deleted meanwhile is stale" do
      user = user!()
      Repo.delete!(user)

      assert_raise Ecto.StaleEntryError, fn ->
        user |> User.changeset(%{name: "Ghost"}) |> Repo.update()
      end
    end

    test "optimistic_lock: a second writer with an old version is stale" do
      user = user!()
      same = Repo.get!(User, user.id)

      {:ok, first} = user |> User.locked(%{name: "First"}) |> Repo.update()
      assert first.lock_version == 2
      assert Repo.get!(User, user.id).lock_version == 2

      assert_raise Ecto.StaleEntryError, fn ->
        same |> User.locked(%{name: "Second"}) |> Repo.update()
      end

      assert Repo.get!(User, user.id).name == "First"
    end
  end

  describe "delete" do
    test "removes every fact of the row, and only that row" do
      ada = user!()
      bob = user!(%{name: "Bob", email: "b@x"})
      {:ok, _} = Repo.delete(ada)

      assert Repo.get(User, ada.id) == nil
      assert Repo.get(User, bob.id)
      refute Enum.any?(Map.keys(facts()), fn {id, _} -> id == ada.id end)
    end

    test "deleting twice is stale" do
      user = user!()
      Repo.delete!(user)
      assert_raise Ecto.StaleEntryError, fn -> Repo.delete!(user) end
    end
  end

  describe "unique_constraint" do
    test "a taken value comes back as a changeset error, not a crash" do
      user!(%{email: "same@x"})

      assert {:error, cs} =
               %User{} |> User.changeset(%{name: "Other", email: "same@x"}) |> Repo.insert()

      assert {"has already been taken", meta} = cs.errors[:email]
      assert meta[:constraint] == :unique
      assert Repo.aggregate(User, :count) == 1
    end

    test "on update, another row's value is refused but your own is fine" do
      a = user!(%{email: "a@x"})
      b = user!(%{name: "Bob", email: "b@x"})

      assert {:error, cs} = b |> User.changeset(%{email: "a@x"}) |> Repo.update()
      assert cs.errors[:email]

      assert {:ok, _} = a |> User.changeset(%{email: "a@x", name: "Ada L."}) |> Repo.update()
    end

    test "without the pre-check, hosigraph's own refusal is turned into the same constraint" do
      user!(%{email: "race@x"})
      meta = Ecto.Adapter.lookup_meta(Repo.get_dynamic_repo()) |> Map.put(:precheck_unique, false)

      assert {:invalid, [unique: "user_email_index"]} =
               Ecto.Adapters.Hosi.insert(
                 meta,
                 %{source: "user"},
                 [name: "Racer", email: "race@x"],
                 {:raise, [], []},
                 [:id],
                 []
               )

      # nothing was kept of the refused write
      assert Repo.aggregate(User, :count) == 1
    end
  end

  describe "insert_all" do
    test "writes all rows in one batch and returns ids" do
      {2, rows} =
        Repo.insert_all(User, [[name: "A", email: "a1@x"], [name: "B", email: "b1@x"]],
          returning: [:id]
        )

      assert [%{id: a}, %{id: b}] = rows
      assert a != b
      assert Repo.get!(User, a).name == "A"
      assert Repo.get!(User, b).name == "B"
    end

    test "without returning" do
      assert {2, nil} = Repo.insert_all(Follow, [[from: 1, to: 2], [from: 2, to: 3]])
      assert Repo.aggregate(Follow, :count) == 2
    end
  end

  describe "storage" do
    test "up, status, down" do
      dir =
        Path.join(System.tmp_dir!(), "ecto_hosi_storage_#{System.unique_integer([:positive])}")

      assert Ecto.Adapters.Hosi.storage_status(dir: dir) == :down
      assert Ecto.Adapters.Hosi.storage_up(dir: dir) == :ok
      assert Ecto.Adapters.Hosi.storage_up(dir: dir) == {:error, :already_up}
      assert Ecto.Adapters.Hosi.storage_status(dir: dir) == :up
      assert Ecto.Adapters.Hosi.storage_down(dir: dir) == :ok
      assert Ecto.Adapters.Hosi.storage_down(dir: dir) == {:error, :already_down}
    end
  end
end
