defmodule Ecto.Hosi.BatchNamesTest do
  # A refused batch says which step was refused.
  use Ecto.Hosi.Case, async: true

  alias Ecto.Changeset
  alias Ecto.Hosi.Test.{Link, Pair}
  alias Ecto.Multi

  defp change(row, fields), do: Changeset.change(row, fields)

  test "two steps with one name are refused, and nothing is written" do
    assert_raise ArgumentError, ~r/two steps are named :n/, fn ->
      Repo.write_batch(
        insert: {:n, change(%User{}, email: "1@x")},
        insert: {:n, change(%User{}, email: "2@x")}
      )
    end

    assert Repo.aggregate(User, :count) == 0
  end

  test "the same unique value twice in one batch: the later step" do
    assert {:error, :two, {:rejected, _}} =
             Repo.write_batch(
               insert: {:one, change(%User{}, email: "a@x")},
               insert: {:two, change(%User{}, email: "a@x")},
               insert: {:three, change(%User{}, email: "c@x")}
             )

    assert Repo.aggregate(User, :count) == 0
  end

  test "a taken value that only hosigraph catches: its step" do
    start_repo(precheck_unique: false)
    Repo.insert!(%User{email: "a@x"})

    assert {:error, :taken, {:rejected, _}} =
             Repo.write_batch(
               insert: {:fine, change(%User{}, email: "b@x")},
               insert: {:taken, change(%User{}, email: "a@x")}
             )
  end

  test "a combination kept by the adapter" do
    assert {:error, :two, {:rejected, _}} =
             Repo.write_batch(
               insert: {:one, change(%Pair{}, a: 1, b: 1)},
               insert: {:two, change(%Pair{}, a: 1, b: 1)}
             )
  end

  test "a combination kept as hosigraph's tuple" do
    assert {:error, :two, {:rejected, _}} =
             Repo.write_batch(
               insert: {:other, change(%Link{}, a: 2, b: 2)},
               insert: {:one, change(%Link{}, a: 1, b: 1)},
               insert: {:two, change(%Link{}, a: 1, b: 1)}
             )
  end

  test "a row that changed meanwhile: the step that wanted it" do
    ada = Repo.insert!(%User{name: "Ada", email: "a@x"})
    Repo.delete!(ada)

    assert {:error, :old, :stale} =
             Repo.write_batch(
               insert: {:new, change(%User{}, email: "n@x")},
               update: {:old, change(ada, name: "Ghost")}
             )
  end

  test "through a Multi the message names the step" do
    assert_raise Ecto.Hosi.Error, ~r/refused step :two/, fn ->
      Multi.new()
      |> Multi.insert(:one, change(%User{}, email: "a@x"))
      |> Multi.insert(:two, change(%User{}, email: "a@x"))
      |> Repo.transaction()
    end
  end
end
