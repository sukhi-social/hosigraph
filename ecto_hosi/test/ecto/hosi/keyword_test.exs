defmodule Ecto.Hosi.KeywordTest do
  use Ecto.Hosi.Case, async: true

  alias Ecto.Hosi.Test.{BadKeyword, BadKeywordName, Ticket}

  defp ticket!(attrs), do: %Ticket{} |> Ticket.changeset(attrs) |> Repo.insert!()
  defp titles(query), do: query |> Repo.all() |> Enum.sort()

  describe "a hosi_keyword field" do
    test "is a keyword in the schema hosigraph is opened with" do
      attrs = Ecto.Hosi.Model.build([Ticket]).schema
      assert %{type: :keyword} = attrs[:"ticket/state"]
      # the other fields are what they were
      assert %{type: :string} = attrs[:"ticket/title"]
    end

    test "is a string for Elixir and a keyword in the store" do
      t = ticket!(%{title: "a", state: "pending"})

      assert Repo.get(Ticket, t.id).state == "pending"
      assert facts()[{t.id, :"ticket/state"}] == [:pending]
    end

    test "the schema default is written as a keyword too" do
      t = Repo.insert!(%Ticket{title: "d"})
      assert facts()[{t.id, :"ticket/state"}] == [:open]
      assert Repo.get(Ticket, t.id).state == "open"
    end

    test "a string that is not on the list is refused before anything is written" do
      assert_raise ArgumentError, ~r/"bogus" is not one of the values/, fn ->
        Repo.insert(%Ticket{title: "z", state: "bogus"})
      end

      assert Repo.aggregate(Ticket, :count) == 0
    end

    test "update changes the keyword, and setting nil takes it away" do
      t = ticket!(%{title: "u"})
      {:ok, t} = t |> Ticket.changeset(%{state: "closed"}) |> Repo.update()

      assert t.state == "closed"
      assert facts()[{t.id, :"ticket/state"}] == [:closed]

      {:ok, _} = t |> Ecto.Changeset.change(state: nil) |> Repo.update()
      refute Map.has_key?(facts(), {t.id, :"ticket/state"})
    end
  end

  describe "queries compare the string the app has" do
    setup do
      for {title, state} <- [{"a", "open"}, {"b", "pending"}, {"c", "closed"}, {"d", "open"}],
          do: ticket!(%{title: title, state: state})

      :ok
    end

    test "== with a literal and with a parameter" do
      assert titles(from(t in Ticket, where: t.state == "open", select: t.title)) == ["a", "d"]

      state = "pending"
      assert titles(from(t in Ticket, where: t.state == ^state, select: t.title)) == ["b"]
    end

    test "!= and in" do
      assert titles(from(t in Ticket, where: t.state != "open", select: t.title)) == ["b", "c"]
      assert titles(from(t in Ticket, where: t.state in ["open", "closed"], select: t.title)) == ["a", "c", "d"]

      states = ["pending"]
      assert titles(from(t in Ticket, where: t.state in ^states, select: t.title)) == ["b"]
    end

    test "get_by and select of the field give strings" do
      assert %Ticket{title: "c", state: "closed"} = Repo.get_by(Ticket, state: "closed")
      assert titles(from(t in Ticket, select: t.state)) == ["closed", "open", "open", "pending"]
    end

    test "group_by (the hybrid path) reads strings back" do
      rows =
        from(t in Ticket, group_by: t.state, select: {t.state, count(t.id)})
        |> Repo.all()
        |> Enum.sort()

      assert rows == [{"closed", 1}, {"open", 2}, {"pending", 1}]
    end

    test "update_all and delete_all" do
      assert {2, _} = Repo.update_all(from(t in Ticket, where: t.state == "open"), set: [state: "closed"])
      assert titles(from(t in Ticket, where: t.state == "closed", select: t.title)) == ["a", "c", "d"]

      assert {3, _} = Repo.delete_all(from(t in Ticket, where: t.state == "closed"))
      assert titles(from(t in Ticket, select: t.title)) == ["b"]
    end

    test "insert_all" do
      Repo.insert_all(Ticket, [%{title: "e", state: "closed"}, %{title: "f", state: "pending"}])

      assert titles(from(t in Ticket, where: t.title in ["e", "f"], select: {t.title, t.state})) ==
               [{"e", "closed"}, {"f", "pending"}]

      assert for({_, vs} <- facts(), v <- vs, v in [:closed, :pending], do: v) |> length() == 4
    end
  end

  test "a transaction that rolls back puts the keyword back" do
    t = ticket!(%{title: "tx", state: "open"})

    Repo.transaction(fn ->
      t |> Ticket.changeset(%{state: "closed"}) |> Repo.update!()
      assert Repo.get(Ticket, t.id).state == "closed"
      Repo.rollback(:no)
    end)

    assert Repo.get(Ticket, t.id).state == "open"
    assert facts()[{t.id, :"ticket/state"}] == [:open]
  end

  test "a unique field next to a keyword field is untouched" do
    ticket!(%{title: "x", slug: "s"})
    assert {:error, cs} = %Ticket{} |> Ticket.changeset(%{title: "y", slug: "s"}) |> Repo.insert()
    assert cs.errors[:slug]
  end

  describe "what the schema must say" do
    test "the field has to be a string" do
      assert_raise ArgumentError, ~r/needs a :string field/, fn -> Ecto.Hosi.Model.build([BadKeyword]) end
    end

    test "the values have to be plain keyword names" do
      assert_raise ArgumentError, ~r/keyword names/, fn -> Ecto.Hosi.Model.build([BadKeywordName]) end
    end
  end
end
