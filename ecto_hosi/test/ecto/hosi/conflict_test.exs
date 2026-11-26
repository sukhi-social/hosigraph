defmodule Ecto.Hosi.ConflictTest do
  use Ecto.Hosi.Case, async: true

  defp follow!(attrs), do: Repo.insert_all(Follow, [attrs]) |> elem(0)

  describe "insert with on_conflict" do
    test ":nothing with a target: the existing row stays, no id comes back" do
      {:ok, a} = %Follow{from: 1, to: 2} |> Repo.insert()

      assert {:ok, %Follow{id: nil}} =
               Repo.insert(%Follow{from: 1, to: 2},
                 on_conflict: :nothing,
                 conflict_target: [:from, :to]
               )

      assert {:ok, %Follow{id: id}} =
               Repo.insert(%Follow{from: 1, to: 3},
                 on_conflict: :nothing,
                 conflict_target: [:from, :to]
               )

      assert id != a.id
      assert Repo.aggregate(Follow, :count) == 2
    end

    test ":nothing with no target skips a taken hosi_unique value" do
      u = %User{name: "A", email: "a@x"} |> Repo.insert!()

      assert {:ok, %User{id: nil}} =
               Repo.insert(%User{name: "B", email: "a@x"}, on_conflict: :nothing)

      assert Repo.aggregate(User, :count) == 1
      assert Repo.get!(User, u.id).name == "A"
    end

    test "{:replace, fields} updates the existing row and returns its id" do
      u = %User{name: "A", email: "a@x", age: 1} |> Repo.insert!()

      assert {:ok, %User{id: id}} =
               Repo.insert(%User{name: "B", email: "a@x", age: 2},
                 on_conflict: {:replace, [:name]},
                 conflict_target: [:email]
               )

      assert id == u.id
      assert %{name: "B", age: 1} = Repo.get!(User, u.id)
      assert Repo.aggregate(User, :count) == 1
    end

    test ":replace_all_except and a nil replacement retract" do
      u = %User{name: "A", email: "a@x", age: 1} |> Repo.insert!()

      assert {:ok, _} =
               Repo.insert(%User{name: "B", email: "a@x"},
                 on_conflict: {:replace_all_except, [:id, :email, :inserted_at]},
                 conflict_target: [:email]
               )

      assert %{name: "B", age: nil} = Repo.get!(User, u.id)
    end

    test "an update query: set and inc, with params" do
      u = %User{name: "A", email: "a@x", age: 1} |> Repo.insert!()
      by = 5

      assert {:ok, _} =
               Repo.insert(%User{name: "B", email: "a@x"},
                 on_conflict: [set: [name: "set"], inc: [age: by]],
                 conflict_target: [:email]
               )

      assert %{name: "set", age: 6} = Repo.get!(User, u.id)
    end

    test "a new row is just inserted, and the other conflict_target forms are refused" do
      assert {:ok, %User{id: id}} =
               Repo.insert(%User{name: "A", email: "a@x"},
                 on_conflict: {:replace, [:name]},
                 conflict_target: [:email]
               )

      assert is_integer(id)

      assert_raise ArgumentError, ~r/conflict_target/, fn ->
        Repo.insert(%User{name: "A", email: "b@x"},
          on_conflict: :nothing,
          conflict_target: {:constraint, "user_email_index"}
        )
      end
    end
  end

  describe "an expression or partial index as the conflict target" do
    @target {:unsafe_fragment, "(lower(email)) WHERE age IS NULL"}

    test "lower(column): a different case is the same row" do
      u = %User{name: "A", email: "Ada@X"} |> Repo.insert!()

      assert {:ok, %User{id: id}} =
               Repo.insert(%User{name: "B", email: "ada@x"},
                 on_conflict: {:replace, [:name]},
                 conflict_target: @target
               )

      assert id == u.id
      assert Repo.get!(User, u.id).name == "B"
      assert Repo.aggregate(User, :count) == 1
    end

    test "a row outside the WHERE is not in the index: it is just inserted" do
      %User{name: "A", email: "Ada@X"} |> Repo.insert!()

      assert {:ok, %User{id: id}} =
               Repo.insert(%User{name: "B", email: "ada@x", age: 5},
                 on_conflict: :nothing,
                 conflict_target: @target
               )

      assert is_integer(id)
      assert Repo.aggregate(User, :count) == 2
    end

    test "a plain column and an equality, in insert_all too" do
      target = {:unsafe_fragment, "(name) WHERE active = true"}
      %User{name: "A", email: "a@x", active: true} |> Repo.insert!()

      assert {1, _} =
               Repo.insert_all(
                 User,
                 [%{name: "A", email: "b@x", active: true}, %{name: "C", email: "c@x", active: true}],
                 on_conflict: :nothing,
                 conflict_target: target
               )

      assert Repo.all(from(u in User, order_by: u.name, select: u.name)) == ["A", "C"]
    end

    test "what it cannot read says so" do
      assert_raise ArgumentError, ~r/cannot read the conflict_target/, fn ->
        Repo.insert(%User{name: "A", email: "x@x"},
          on_conflict: :nothing,
          conflict_target: {:unsafe_fragment, "(email) WHERE age > 5"}
        )
      end
    end
  end

  describe "insert_all with on_conflict" do
    test ":nothing skips existing rows and repeats inside the batch" do
      follow!(%{from: 1, to: 2})

      assert {2, _} =
               Repo.insert_all(
                 Follow,
                 [%{from: 1, to: 2}, %{from: 1, to: 3}, %{from: 1, to: 3}, %{from: 2, to: 3}],
                 on_conflict: :nothing,
                 conflict_target: [:from, :to]
               )

      assert Repo.aggregate(Follow, :count) == 3
    end

    test "replace updates the existing rows and counts them; returning gives every id" do
      {:ok, u} = Repo.insert(%User{name: "A", email: "a@x"})

      assert {2, rows} =
               Repo.insert_all(
                 User,
                 [%{name: "A2", email: "a@x"}, %{name: "C", email: "c@x"}],
                 on_conflict: {:replace, [:name]},
                 conflict_target: [:email],
                 returning: [:id]
               )

      assert [%{id: id1}, %{id: _id2}] = rows
      assert id1 == u.id
      assert Repo.get!(User, u.id).name == "A2"
      assert Repo.aggregate(User, :count) == 2
    end

    test "a replace cannot affect a row twice" do
      assert_raise ArgumentError, ~r/second time/, fn ->
        Repo.insert_all(
          User,
          [%{name: "A", email: "a@x"}, %{name: "B", email: "a@x"}],
          on_conflict: {:replace, [:name]},
          conflict_target: [:email]
        )
      end
    end
  end
end
