defmodule Ecto.Hosi.TimeoutTest do
  # Writes wait as long as `timeout:` says. When it runs out, the write goes on without the caller,
  # and the error says that the result is unknown.
  use Ecto.Hosi.Case, async: true

  alias Ecto.Changeset
  alias Ecto.Hosi.Test.{Follow, Slow}
  alias Ecto.Multi

  setup do
    start_repo(hosi: Slow)
    :ok
  end

  defp eventually(fun, left \\ 100) do
    cond do
      fun.() -> true
      left == 0 -> false
      true -> Process.sleep(50) && eventually(fun, left - 1)
    end
  end

  defp tos, do: Repo.all(from(f in Follow, select: f.to)) |> Enum.sort()
  defp change(row \\ %Follow{}, fields), do: Changeset.change(row, fields)

  test "a write slower than 5 seconds succeeds when timeout: allows it" do
    Slow.by(5_500)
    assert {:ok, _} = Repo.insert(%Follow{from: 1, to: 2}, timeout: 30_000)
    assert tos() == [2]
  end

  test "the Repo's :timeout is the default" do
    start_repo(hosi: Slow, timeout: 100)
    Slow.by(600)

    assert_raise Ecto.Hosi.Error, ~r/100 ms.*unknown/s, fn -> Repo.insert(%Follow{to: 2}) end
    assert eventually(fn -> tos() == [2] end)
  end

  # {what, what it leaves behind once the held-back write has landed}; `row` is Follow{from: 1, to: 1}
  @writes [
    insert: [2, 1],
    insert_all: [1, 2],
    update: [2],
    delete: [],
    update_all: [2],
    delete_all: [],
    write_batch: [1, 2],
    multi: [1, 2],
    erase: []
  ]

  defp run(:insert, _row), do: Repo.insert(%Follow{to: 2}, timeout: 50)
  defp run(:insert_all, _row), do: Repo.insert_all(Follow, [[to: 2]], timeout: 50)
  defp run(:update, row), do: Repo.update(change(row, to: 2), timeout: 50)
  defp run(:delete, row), do: Repo.delete(row, timeout: 50)
  defp run(:update_all, _row), do: Repo.update_all(Follow, [set: [to: 2]], timeout: 50)
  defp run(:delete_all, _row), do: Repo.delete_all(Follow, timeout: 50)
  defp run(:write_batch, _row), do: Repo.write_batch([insert: {:f, change(to: 2)}], timeout: 50)
  defp run(:multi, _row), do: Multi.new() |> Multi.insert(:f, change(to: 2)) |> Repo.transaction(timeout: 50)
  defp run(:erase, row), do: Repo.erase([[:ids, [row.id]]], timeout: 50)

  for {what, after_} <- @writes do
    test "#{what} honours timeout:, and says the result is unknown" do
      row = Repo.insert!(%Follow{from: 1, to: 1})
      Slow.by(300)

      error = assert_raise Ecto.Hosi.Error, fn -> run(unquote(what), row) end

      assert error.message =~ "50 ms"
      assert error.message =~ "unknown"
      assert eventually(fn -> tos() == Enum.sort(unquote(after_)) end)
    end
  end
end
