defmodule Ecto.Hosi.NewIdsTest do
  # The ids hosigraph reports for the new rows of one write land on the rows that asked for them.
  use Ecto.Hosi.Case, async: true

  alias Ecto.Changeset

  # %{id => from}, as the store has it
  defp stored, do: Repo.all(from(f in Follow, select: {f.id, f.from})) |> Map.new()

  test "insert_all returns the ids in the order of the rows" do
    {200, rows} = Repo.insert_all(Follow, for(n <- 1..200, do: [from: n]), returning: [:id])

    stored = stored()
    assert Enum.map(rows, &stored[&1.id]) == Enum.to_list(1..200)
  end

  test "also when some rows are new and some are there already" do
    for n <- 1..60//3, do: Repo.insert!(%User{name: "old", email: "#{n}@x"})
    emails = for n <- 1..60, do: "#{n}@x"

    {60, rows} =
      Repo.insert_all(User, for(email <- emails, do: [name: "new", email: email]),
        on_conflict: {:replace, [:name]},
        conflict_target: [:email],
        returning: [:id]
      )

    stored = Repo.all(from(u in User, select: {u.id, u.email})) |> Map.new()
    assert Enum.map(rows, &stored[&1.id]) == emails
  end

  test "write_batch gives each step its own row" do
    steps = for n <- 1..200, do: {:insert, {n, Changeset.change(%Follow{}, from: n)}}
    assert {:ok, results} = Repo.write_batch(steps)

    stored = stored()
    assert Enum.map(1..200, &stored[results[&1].id]) == Enum.to_list(1..200)
  end
end
