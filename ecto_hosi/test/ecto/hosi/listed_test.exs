defmodule Ecto.Hosi.ListedTest do
  # A schema's rules and types come from the Repo's `:schemas`. A write through a schema that is
  # not there would pass them by, so it is refused.
  use Ecto.Hosi.Case, async: true

  alias Ecto.Changeset
  alias Ecto.Hosi.Test.{Unlisted, UserName}
  alias Ecto.Multi

  defp loaded, do: Ecto.put_meta(struct(Unlisted, id: 1, name: "a"), state: :loaded)

  test "every way of writing is refused, and nothing is written" do
    writes = [
      fn -> Repo.insert(%Unlisted{name: "a"}) end,
      fn -> Repo.insert_all(Unlisted, [[name: "a"]]) end,
      fn -> Repo.update(Changeset.change(loaded(), name: "b")) end,
      fn -> Repo.delete(loaded()) end,
      fn -> Repo.update_all(Unlisted, set: [name: "b"]) end,
      fn -> Repo.delete_all(Unlisted) end,
      fn -> Repo.write_batch(insert: {:u, Changeset.change(%Unlisted{}, name: "a")}) end,
      fn -> Repo.write_batch(update: {:u, Changeset.change(loaded(), name: "b")}) end,
      fn -> Repo.write_batch(delete: {:u, loaded()}) end,
      fn -> Multi.new() |> Multi.insert(:u, %Unlisted{name: "a"}) |> Repo.transaction() end
    ]

    for write <- writes do
      assert_raise ArgumentError, ~r/Unlisted.*not in the Repo's :schemas.*ecto_hosi's limit/s, write
    end

    assert facts() |> Map.keys() |> Enum.filter(fn {_, a} -> "#{a}" =~ "unlisted/" end) == []
  end

  test "a listed schema under another source is refused" do
    assert_raise ArgumentError, ~r/"other".*:schemas/s, fn ->
      %User{email: "a@x"} |> Ecto.put_meta(source: "other") |> Repo.insert()
    end
  end

  test "a second schema of one source is refused until it is listed" do
    assert_raise ArgumentError, ~r/UserName.*:schemas/s, fn -> Repo.insert(%UserName{name: "a"}) end
  end

  test "listed after the first, it does not take the unique rule away" do
    start_repo(schemas: [User, UserName])
    Repo.insert!(struct(UserName, name: "a", email: "a@x"))

    assert_raise Ecto.ConstraintError, fn -> Repo.insert(struct(UserName, email: "a@x")) end

    assert {:error, _} = %User{} |> User.changeset(%{name: "b", email: "a@x"}) |> Repo.insert()
    assert Repo.aggregate(User, :count) == 1
  end
end
