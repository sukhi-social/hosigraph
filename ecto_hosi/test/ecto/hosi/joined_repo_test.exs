defmodule Ecto.Hosi.JoinedRepoTest do
  use Ecto.Hosi.Case, async: true

  alias Ecto.Hosi.Test.{Joiner, Member, Team}

  test "a joined repo reads and writes the same store, and keeps its rules" do
    name = :"joiner_#{System.unique_integer([:positive])}"
    start_supervised!({Joiner, name: name, join: {Repo, Repo.get_dynamic_repo()}})
    Joiner.put_dynamic_repo(name)

    t = Repo.insert!(%Team{name: "t"})
    Repo.insert!(%Member{team_id: t.id, name: "m"})

    assert [%Team{name: "t"}] = Joiner.all(Team)
    # the foreign keys of the joined repo apply to deletes made through this one
    Joiner.delete!(t)
    assert Repo.all(Member) == []
  end
end
