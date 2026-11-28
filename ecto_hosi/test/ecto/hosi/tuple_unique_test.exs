defmodule Ecto.Hosi.TupleUniqueTest do
  use Ecto.Hosi.Case, async: true

  alias Ecto.Hosi.Test.{BadTuple, Link}

  @tuple :"hosi/uq-link_a_b_index"

  defp ins(attrs), do: %Link{} |> Link.changeset(attrs) |> Repo.insert()

  # the entities that hold this pair, asked as hosigraph's tuple value: `[?e :link/uq-... [a b]]`
  defp holders(a, b) do
    Repo.datalog(%{
      q: "[:find ?e :in $ ?p :where [?e #{Ecto.Hosi.Model.kw(@tuple)} ?p]]",
      inputs: [[:scalar, [a, b]]]
    })
    |> List.flatten()
  end

  describe "hosi_unique [...], tuple: true" do
    test "is hosigraph's tuple with unique, not a JSON text" do
      attrs = Ecto.Hosi.Model.build([Link]).schema
      assert %{tuple: [:"link/a", :"link/b"], unique: :value} = attrs[@tuple]
      refute Map.has_key?(attrs[@tuple], :type)
    end

    test "the pair is found by its value, and nothing is written for it" do
      {:ok, l} = ins(%{a: 1, b: 2})

      assert holders(1, 2) == [l.id]
      assert holders(2, 1) == []
      # no JSON text under the rule's attribute (the old derived key): the khana keeps the tuple
      assert for({{_, @tuple}, vs} <- facts(), v <- vs, is_binary(v), do: v) == []
      assert facts()[{l.id, :"link/a"}] == [1]
    end

    test "refuses the same pair, allows another" do
      assert {:ok, _} = ins(%{a: 1, b: 1})
      assert {:error, cs} = ins(%{a: 1, b: 1})
      assert [a: {_, [constraint: :unique, constraint_name: "link_a_b_index"]}] = cs.errors
      assert {:ok, _} = ins(%{a: 1, b: 2})
      assert {:ok, _} = ins(%{a: 2, b: 1})
    end

    test "an update into a taken pair is refused; out of it frees the old one" do
      {:ok, _} = ins(%{a: 1, b: 1})
      {:ok, l} = ins(%{a: 1, b: 2})

      assert {:error, cs} = l |> Link.changeset(%{b: 1}) |> Repo.update()
      assert cs.errors[:a]

      assert {:ok, l} = l |> Link.changeset(%{b: 3}) |> Repo.update()
      assert holders(1, 2) == []
      assert holders(1, 3) == [l.id]
      assert {:ok, _} = ins(%{a: 1, b: 2})

      # a change that leaves the pair alone is not a conflict with itself
      assert {:ok, _} = l |> Link.changeset(%{note: "same pair"}) |> Repo.update()
    end

    test "delete frees the pair" do
      {:ok, l} = ins(%{a: 5, b: 5})
      Repo.delete!(l)
      assert holders(5, 5) == []
      assert {:ok, _} = ins(%{a: 5, b: 5})
    end

    test "update_all moves the pair" do
      {:ok, _} = ins(%{a: 1, b: 1})
      {:ok, l} = ins(%{a: 1, b: 2})

      Repo.update_all(from(x in Link, where: x.id == ^l.id), set: [b: 9])

      assert holders(1, 2) == []
      assert {:ok, _} = ins(%{a: 1, b: 2})
      assert {:error, _} = ins(%{a: 1, b: 9})
    end

    test "on_conflict: :nothing skips a taken pair, for insert and for insert_all" do
      {:ok, _} = ins(%{a: 7, b: 7})
      assert {:ok, %Link{id: nil}} = Repo.insert(%Link{a: 7, b: 7}, on_conflict: :nothing)

      assert {1, _} =
               Repo.insert_all(Link, [%{a: 7, b: 7}, %{a: 7, b: 8}], on_conflict: :nothing)

      assert Repo.aggregate(Link, :count) == 2
    end

    test "two equal rows in ONE insert_all are refused by hosigraph, and nothing is kept" do
      assert_raise Ecto.Hosi.Error, ~r/insert_all was rejected/, fn ->
        Repo.insert_all(Link, [%{a: 3, b: 3}, %{a: 3, b: 3}])
      end

      assert Repo.aggregate(Link, :count) == 0
    end

    test "write_batch checks a pair that is stored already" do
      {:ok, _} = ins(%{a: 1, b: 1})

      assert {:error, :second, _} =
               Repo.write_batch(
                 insert: {:first, Link.changeset(%Link{}, %{a: 2, b: 2})},
                 insert: {:second, Link.changeset(%Link{}, %{a: 1, b: 1})}
               )

      # all or nothing
      assert holders(2, 2) == []
    end

    test "without the pre-check, hosigraph's own refusal is turned into the same constraint" do
      {:ok, _} = ins(%{a: 4, b: 4})
      meta = Ecto.Adapter.lookup_meta(Repo.get_dynamic_repo()) |> Map.put(:precheck_unique, false)

      assert {:invalid, [unique: "link_a_b_index"]} =
               Ecto.Adapters.Hosi.insert(
                 meta,
                 %{source: "link", schema: Link},
                 ["link/a": 4, "link/b": 4],
                 {:raise, [], []},
                 [:id],
                 []
               )

      assert Repo.aggregate(Link, :count) == 1
    end
  end

  describe "a tuple's nil is a value, SQL's NULL is not" do
    test "a new row has to have every field" do
      assert_raise ArgumentError, ~r/needs every field set/, fn -> Repo.insert(%Link{a: 1}) end
      assert Repo.aggregate(Link, :count) == 0
    end

    test "a field cannot be set to nil" do
      {:ok, l} = ins(%{a: 1, b: 1})

      assert_raise ArgumentError, ~r/needs every field set/, fn ->
        l |> Ecto.Changeset.change(b: nil) |> Repo.update()
      end

      assert Repo.get(Link, l.id).b == 1
    end

    test "the rule says what it cannot do" do
      assert_raise ArgumentError, ~r/plain combination/, fn -> Ecto.Hosi.Model.build([BadTuple]) end
    end
  end
end
