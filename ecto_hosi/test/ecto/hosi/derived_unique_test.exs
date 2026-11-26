defmodule Ecto.Hosi.DerivedUniqueTest do
  use Ecto.Hosi.Case, async: true

  alias Ecto.Hosi.Test.Pair

  defp ins(attrs), do: %Pair{} |> Pair.changeset(attrs) |> Repo.insert()

  describe "a composite unique" do
    test "refuses the same combination, allows a different one" do
      assert {:ok, _} = ins(%{a: 1, b: 1})
      assert {:error, cs} = ins(%{a: 1, b: 1})
      assert [a: {_, [constraint: :unique, constraint_name: "pair_a_b_index"]}] = cs.errors
      assert {:ok, _} = ins(%{a: 1, b: 2})
      assert {:ok, _} = ins(%{a: 2, b: 1})
    end

    test "a nil member is never in conflict" do
      assert {:ok, _} = ins(%{a: 1})
      assert {:ok, _} = ins(%{a: 1})
    end

    test "update into a taken combination is refused, out of it frees the old one" do
      {:ok, _} = ins(%{a: 1, b: 1})
      {:ok, p} = ins(%{a: 1, b: 2})

      assert {:error, _} = p |> Pair.changeset(%{b: 1}) |> Repo.update()
      assert {:ok, p} = p |> Pair.changeset(%{b: 3}) |> Repo.update()
      # (1, 2) is free now
      assert {:ok, _} = ins(%{a: 1, b: 2})
      # an update of an unrelated field leaves the key alone
      assert {:ok, _} = p |> Pair.changeset(%{domain: "x"}) |> Repo.update()
      assert {:error, _} = ins(%{a: 1, b: 3})
    end

    test "delete frees the combination" do
      {:ok, p} = ins(%{a: 5, b: 5})
      Repo.delete!(p)
      assert {:ok, _} = ins(%{a: 5, b: 5})
    end

    test "update_all moves the key" do
      {:ok, _} = ins(%{a: 1, b: 1})
      {:ok, p} = ins(%{a: 1, b: 2})

      Ecto.Query.from(x in Pair, where: x.id == ^p.id, update: [set: [b: 9]]) |> Repo.update_all([])
      assert {:ok, _} = ins(%{a: 1, b: 2})
      assert {:error, _} = ins(%{a: 1, b: 9})
    end

    test "on_conflict :nothing skips a taken combination" do
      {:ok, _} = ins(%{a: 7, b: 7})
      assert {:ok, %Pair{id: nil}} = Repo.insert(%Pair{a: 7, b: 7}, on_conflict: :nothing)
    end
  end

  describe "a lower-cased, partial unique" do
    test "applies to rows without a domain, ignoring case" do
      assert {:ok, _} = ins(%{email: "Ada@x.org"})
      assert {:error, cs} = ins(%{email: "ada@X.org"})
      assert [email: {_, [constraint: :unique, constraint_name: "pair_local_email_index"]}] = cs.errors
      # a row with a domain is outside the rule
      assert {:ok, _} = ins(%{email: "ada@x.org", domain: "remote"})
      assert {:ok, _} = ins(%{email: "ada@x.org", domain: "remote"})
    end

    test "giving a row a domain takes it out of the rule, removing it puts it back" do
      {:ok, p} = ins(%{email: "bo@x"})
      {:ok, p} = p |> Pair.changeset(%{domain: "r"}) |> Repo.update()
      assert {:ok, _} = ins(%{email: "bo@x"})
      assert {:error, _} = p |> Pair.changeset(%{domain: nil}) |> Repo.update()
    end
  end

  test "a static Multi enforces them too" do
    {:ok, _} = ins(%{a: 4, b: 4})

    multi = Ecto.Multi.new() |> Ecto.Multi.insert(:p, Pair.changeset(%Pair{}, %{a: 4, b: 4}))
    assert {:error, :p, cs, _} = Repo.transaction(multi)
    assert [a: _] = cs.errors
  end
end
