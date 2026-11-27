defmodule Ecto.Hosi.LoudTest do
  # Mistakes are said, not answered with `[]` or an internal error.
  use Ecto.Hosi.Case, async: true

  import Ecto.Hosi.Query

  alias Ecto.Hosi.Test.Ticket

  defp q(text, inputs \\ []), do: %{q: text, inputs: inputs}
  defp user!, do: Repo.insert!(%User{name: "Ada", email: "#{System.unique_integer([:positive])}@x"})

  describe "names in Datalog" do
    test "a misspelt attribute is said, with the name that was meant" do
      user!()

      error = assert_raise ArgumentError, fn -> Repo.datalog(q("[:find ?e :where [?e :user/nmae _]]")) end

      assert error.message =~ ":user/nmae"
      assert error.message =~ "Did you mean :user/name?"
    end

    test "also in not, or-join, a rule, a misspelt namespace" do
      for query <- [
            q("[:find ?e :where [?e :user/ecto-kind true] (not [?e :user/nmae _])]"),
            q("[:find ?e :where [?e :user/ecto-kind true] (or-join [?e] [?e :user/age 1] [?e :user/nmae _])]"),
            q("[:find ?e :in $ % :where (named ?e)]", [[:rules, "[[(named ?e) [?e :user/nmae _]]]"]]),
            q("[:find ?e :where [?e :usr/name _]]")
          ] do
        assert_raise ArgumentError, ~r/Did you mean :user\/name\?/, fn -> Repo.datalog(query) end
      end
    end

    test "an attribute some record has is no mistake, declared or not" do
      u = user!()
      direct_write([[:create, u.id, :"user/nmae", "al"]])

      assert Repo.datalog(q("[:find ?e :where [?e :user/nmae _]]")) == [[u.id]]
    end

    test "an unknown name unlike any declared one is left to hosigraph" do
      assert Repo.datalog(q("[:find ?e :where [?e :zzz _]]")) == []
    end

    test "check: false asks hosigraph as it is" do
      assert Repo.datalog(q("[:find ?e :where [?e :user/nmae _]]"), check: false) == []
    end
  end

  describe "a string where a keyword is kept" do
    test "written in the query, or passed in" do
      for query <- [
            q("[:find ?t :where [?t :ticket/state \"open\"]]"),
            q("[:find ?t :in $ ?s :where [?t :ticket/state ?s]]", [[:scalar, "open"]]),
            q("[:find ?t :in $ [?s ...] :where [?t :ticket/state ?s]]", [[:coll, [:closed, "open"]]])
          ] do
        assert_raise ArgumentError, ~r/:ticket\/state is a keyword.*:open, not "open"/s, fn ->
          Repo.datalog(query)
        end
      end
    end

    test "from the datalog macro" do
      state = "open"

      assert_raise ArgumentError, ~r/:open, not "open"/, fn ->
        Repo.datalog(datalog(find: [t], where: [match(Ticket, id: t, state: ^state)]))
      end
    end

    test "the keyword finds the row" do
      t = Repo.insert!(%Ticket{title: "a"})
      assert Repo.datalog(q("[:find ?t :where [?t :ticket/state :open]]")) == [[t.id]]
    end
  end

  describe "history, erase, clean" do
    test "history takes the struct too, and says what else it cannot take" do
      u = user!()
      assert Repo.history(u) == Repo.history(u.id)

      assert_raise ArgumentError, ~r/struct or its integer id/, fn -> Repo.history("x") end
      assert_raise ArgumentError, ~r/struct or its integer id/, fn -> Repo.history(%User{}) end
    end

    test "erase says what it takes" do
      u = user!()

      for wrong <- [[u.id], u.id] do
        assert_raise ArgumentError, ~r/\[\[:ids, \[id\]\]\]/, fn -> Repo.erase(wrong) end
      end

      assert Repo.get(User, u.id)
    end

    test "clean removes what erase left on disk" do
      u = user!()
      Repo.erase([[:ids, [u.id]]])

      assert %{"records-left": 0} = Repo.clean()
    end
  end

  describe "unsupported queries" do
    test "a subquery is named, with a Datalog place to start" do
      sub = from(x in User, select: x.id)
      in_where = from(u in User, where: u.id in subquery(sub))

      for ask <- [
            fn -> Repo.all(in_where) end,
            fn -> Repo.all(from(u in subquery(sub), select: u.id)) end,
            fn -> Repo.delete_all(in_where) end,
            fn -> Repo.update_all(in_where, set: [name: "x"]) end
          ] do
        error = assert_raise ArgumentError, ask

        assert error.message =~ "a subquery"
        assert error.message =~ "ecto_hosi's limit"
        assert error.message =~ ~s|Repo.datalog(%{q: "[:find ?e :where [?e :user/ecto-kind true]]"|
        refute error.message =~ "Ecto.SubQuery"
      end
    end

    test "says whose limit it is" do
      user!()
      assert_raise ArgumentError, ~r/ecto_hosi's limit/, fn -> Repo.all(from(u in User, lock: "x")) end
      assert_raise ArgumentError, ~r/ecto_hosi's limit/, fn -> Repo.stream(User) |> Enum.to_list() end

      assert_raise ArgumentError, ~r/ecto_hosi's limit/, fn ->
        Repo.all(from(u in User, where: fragment("soundex(?)", u.name) == "a"))
      end
    end
  end
end
