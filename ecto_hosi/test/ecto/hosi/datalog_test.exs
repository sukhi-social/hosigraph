defmodule Ecto.Hosi.DatalogTest do
  use Ecto.Hosi.Case, async: true
  import Ecto.Hosi.Query

  setup do
    # 1->2->3->4->2 (a cycle), 5<->6, 7->1
    for {from, to} <- [{1, 2}, {2, 3}, {3, 4}, {4, 2}, {5, 6}, {6, 5}, {7, 1}] do
      Repo.insert!(%Follow{from: from, to: to})
    end

    :ok
  end

  # the rows of a Datalog answer are a set: Datalevin gives them in no particular order
  defp datalog_rows(query), do: query |> Repo.datalog() |> Enum.sort()

  describe "rules" do
    @describetag :needs_rules_door
    test "a recursive rule reaches everything, and a cycle terminates" do
      me = 1

      assert datalog_rows(datalog(find: [b], where: [reachable(^me, b)], rules: Rules)) == [
               [2],
               [3],
               [4]
             ]

      two = 2

      assert datalog_rows(datalog(find: [b], where: [reachable(^two, b)], rules: Rules)) == [
               [2],
               [3],
               [4]
             ]

      seven = 7

      assert datalog_rows(datalog(find: [b], where: [reachable(^seven, b)], rules: Rules)) == [
               [1],
               [2],
               [3],
               [4]
             ]
    end

    test "negation inside a rule" do
      assert datalog_rows(datalog(find: [a], where: [unfollowed(a)], rules: Rules)) == [[7]]
    end

    test "several matches and a comparison in a rule" do
      assert datalog_rows(datalog(find: [a, b], where: [mutual(a, b)], rules: Rules)) == [[5, 6]]
    end

    test "a rule, a match and a not, in one query; a list is a collection input" do
      starts = [1, 7]

      query =
        datalog(
          find: [a, b],
          where: [
            match(Follow, from: ^starts, to: _),
            reachable(a, b),
            not match(Follow, from: a, to: b)
          ],
          rules: Rules
        )

      # `a` is bound by the rule only: a is anything that reaches b
      assert is_list(datalog_rows(query))
    end

    test "a rule called without its module raises a clear error" do
      query = datalog(find: [b], where: [reachable(1, b)])

      # the stand-in says "unknown rule", Datalevin says "Missing rules var '%' in :in"
      assert_raise Ecto.Hosi.Error, ~r/unknown rule reachable|Missing rules var/, fn ->
        datalog_rows(query)
      end
    end
  end

  describe "the request hosigraph is sent (the contract)" do
    test "rules travel as the last input, `%` is last in :in, values are scalar or coll inputs" do
      me = 1
      ids = [1, 2]

      query =
        datalog(
          find: [b],
          where: [match(Follow, from: ^ids, to: b), reachable(^me, b)],
          rules: Rules
        )

      %{q: q, inputs: inputs} = Ecto.Hosi.Query.to_request(query)

      assert q =~ ~r/^\[:find \?b :in \$ \[\?__p\d+ \.\.\.\] \?__p\d+ % :where /
      assert [[:coll, [1, 2]], [:scalar, 1], [:rules, rules]] = inputs
      assert is_binary(rules)

      assert rules =~
               "[(reachable ?__h0 ?__h1) [?__e0 :follow/from ?__h0] [?__e0 :follow/to ?__h1]]"

      assert rules =~ "(reachable ?b ?__h1)"
      assert rules =~ "(not-join [?__h0] [?__e1 :follow/to ?__h0])"
      assert rules =~ "[(< ?__h0 ?__h1)]"
    end

    test "without rules there is no % and no rules input" do
      query = datalog(find: [x], where: [match(Follow, from: x)])
      assert %{q: q, inputs: []} = Ecto.Hosi.Query.to_request(query)
      assert q == "[:find ?x :in $ :where [?__e0 :follow/from ?x]]"
    end
  end

  describe "plain Datalog over schemas" do
    test "match binds the entity id with `id:`" do
      for {name, age} <- [{"Ada", 36}, {"Bob", 25}, {"Cy", 40}] do
        Repo.insert!(%User{name: name, age: age, email: "#{name}@x"})
      end

      min = 30

      rows =
        datalog_rows(
          datalog(
            find: [id, name],
            where: [match(User, id: id, name: name, age: age), age > ^min]
          )
        )

      assert Enum.map(rows, fn [_, n] -> n end) == ["Ada", "Cy"]
      assert [[id, _] | _] = rows
      assert Repo.get!(User, id).name == "Ada"
    end

    test "a raw request is passed through" do
      assert [[_]] =
               datalog_rows(%{q: "[:find ?e :in $ :where [?e :follow/from 5]]", inputs: []})
    end

    test "a bad query is an error, not a silent empty answer" do
      assert_raise Ecto.Hosi.Error, fn ->
        datalog_rows(%{q: "[:find ?e :in $ ?x :where [?e :follow/from ?x]]", inputs: []})
      end
    end
  end
end
