defmodule Ecto.Hosi.JoinTest do
  use Ecto.Hosi.Case, async: true

  defp user!(name, attrs) do
    %User{}
    |> User.changeset(Map.merge(%{name: name, email: "#{name}@x"}, attrs))
    |> Repo.insert!()
  end

  defp post!(user, title, likes),
    do: Repo.insert!(%Post{title: title, likes: likes, user_id: user.id})

  setup do
    ada = user!("Ada", %{age: 36})
    bob = user!("Bob", %{age: 40})
    cy = user!("Cy", %{age: 20})
    post!(ada, "a1", 3)
    post!(ada, "a2", 7)
    post!(bob, "b1", 1)
    {:ok, ada: ada, bob: bob, cy: cy}
  end

  describe "inner join" do
    test "on a foreign key, selecting fields of both", %{ada: ada} do
      rows =
        Repo.all(
          from(p in Post,
            join: u in User,
            on: u.id == p.user_id,
            where: u.name == "Ada",
            order_by: p.title,
            select: {p.title, u.name}
          )
        )

      assert rows == [{"a1", "Ada"}, {"a2", "Ada"}]
      assert ada.id
    end

    test "with a param, and a where on the joined table" do
      min = 5

      assert ["a2"] ==
               Repo.all(
                 from(p in Post,
                   join: u in User,
                   on: u.id == p.user_id,
                   where: p.likes > ^min and u.age >= 30,
                   select: p.title
                 )
               )
    end

    test "a struct from each side", %{bob: bob} do
      assert [{%Post{title: "b1"}, %User{id: id}}] =
               Repo.all(
                 from(p in Post,
                   join: u in User,
                   on: u.id == p.user_id,
                   where: u.name == "Bob",
                   select: {p, u}
                 )
               )

      assert id == bob.id
    end

    test "preload through a join", %{ada: ada} do
      [u] =
        Repo.all(
          from(u in User, join: p in assoc(u, :posts), where: u.id == ^ada.id, preload: [posts: p])
        )
        |> Enum.uniq_by(& &1.id)

      assert u.posts |> Enum.map(& &1.title) |> Enum.sort() == ["a1", "a2"]
    end

    test "order, limit and offset apply after the join" do
      assert ["b1", "a2"] ==
               Repo.all(
                 from(p in Post,
                   join: u in User,
                   on: u.id == p.user_id,
                   order_by: [desc: p.title],
                   offset: 0,
                   limit: 2,
                   select: p.title
                 )
               )
    end
  end

  describe "left join" do
    test "keeps the rows with nothing to join; the joined side is nil" do
      rows =
        Repo.all(
          from(u in User,
            left_join: p in Post,
            on: p.user_id == u.id,
            order_by: [u.name, p.title],
            select: {u.name, p.title}
          )
        )

      assert rows == [{"Ada", "a1"}, {"Ada", "a2"}, {"Bob", "b1"}, {"Cy", nil}]
    end

    test "is_nil on the joined side finds the rows with none" do
      assert ["Cy"] ==
               Repo.all(
                 from(u in User,
                   left_join: p in Post,
                   on: p.user_id == u.id,
                   where: is_nil(p.id),
                   select: u.name
                 )
               )
    end
  end

  describe "group_by and aggregates" do
    test "count and sum per group, with having" do
      rows =
        Repo.all(
          from(u in User,
            join: p in Post,
            on: p.user_id == u.id,
            group_by: u.name,
            having: count(p.id) > 1,
            select: {u.name, count(p.id), sum(p.likes)}
          )
        )

      assert rows == [{"Ada", 2, 10}]
    end

    test "an aggregate without group_by is one row, even over nothing" do
      assert [0] == Repo.all(from(p in Post, where: p.likes > 100, select: count(p.id)))
      assert [{3, 7, 1}] == Repo.all(from(p in Post, select: {count(p.id), max(p.likes), min(p.likes)}))
    end

    test "count distinct, and a filtered count" do
      assert [2] ==
               Repo.all(from(p in Post, select: count(p.user_id, :distinct)))

      assert [2] == Repo.all(from(p in Post, select: filter(count(p.id), p.likes > 2)))
    end

    test "order by an aggregate" do
      assert ["Ada", "Bob"] ==
               Repo.all(
                 from(u in User,
                   join: p in Post,
                   on: p.user_id == u.id,
                   group_by: u.name,
                   order_by: [desc: sum(p.likes)],
                   select: u.name
                 )
               )
    end
  end

  describe "fragments, like, distinct, schemaless" do
    test "lower(), like and ilike" do
      assert ["Ada"] ==
               Repo.all(from(u in User, where: fragment("lower(?)", u.name) == "ada", select: u.name))

      assert ["Bob", "Cy"] ==
               Repo.all(from(u in User, where: not like(u.name, "A%"), order_by: u.name, select: u.name))

      assert ["Ada"] == Repo.all(from(u in User, where: ilike(u.name, "ad_"), select: u.name))
    end

    test "distinct" do
      assert [ada_id, bob_id] = Repo.all(from(p in Post, distinct: true, select: p.user_id)) |> Enum.sort()
      assert ada_id < bob_id
    end

    test "a schemaless query on a table name" do
      assert [{"Ada", 36}] ==
               Repo.all(from(u in "user", where: u.name == "Ada", select: {u.name, u.age}))
    end

    test "coalesce and exists-style lookups", %{cy: cy} do
      assert [20] == Repo.all(from(u in User, where: u.id == ^cy.id, select: coalesce(u.age, 0)))
    end
  end
end
