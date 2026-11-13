defmodule HosiTest do
  # No database: `open: %{}` opens nothing; the connection answers :ping and :abi, refuses the rest.
  use ExUnit.Case, async: true

  setup do
    {:ok, db} = Hosi.start_link(open: %{})
    %{db: db}
  end

  test "the header and the library agree" do
    assert Hosi.Nif.abi_version() == 1
  end

  test "ping gives back what it was given, through EDN both ways", %{db: db} do
    v = [1, "é", :k, 2.5, true, nil, %{a: [1]}]
    assert Hosi.ping(db, v) == {:ok, v}
  end

  test "abi says what it speaks", %{db: db} do
    assert {:ok, %{name: "hosi", version: 1, ops: [:ping, :abi, :write | _]}} = Hosi.abi(db)
  end

  test "what is not the contract is :protocol, with its kind", %{db: db} do
    assert {:error, {:protocol, _kind, msg}} = Hosi.request(db, :nope)
    assert is_binary(msg)
    assert {:error, {:protocol, _, _}} = Hosi.get_entities(db, ["x"])
  end

  test "without a database the other operations are refused as :app", %{db: db} do
    assert {:error, {:app, :"bad-argument", _}} = Hosi.stats(db)
    assert {:error, {:app, :"bad-argument", _}} = Hosi.write(db, [])
  end

  test "write takes when: conditions; a condition that is not the shape is :protocol, one that is reaches the library", %{db: db} do
    # (no database here: a request of the right shape is refused as :app, and that is how we know it got there)
    assert {:error, {:protocol, _, _}} = Hosi.write(db, [[:create, 1, :a, 2]], when: [[:equals, 1, :a, 2]])
    assert {:error, {:protocol, _, _}} = Hosi.write(db, [[:create, 1, :a, 2]], when: [[:holds, [:tmp, 1], :a, 2]])
    assert {:error, {:app, :"bad-argument", _}} =
             Hosi.write(db, [[:create, 1, :a, 2]], when: [[:holds, 1, :a, 2], [:lacks, 1, :b, 3], [:unset, 1, :c]])
  end

  test "erase, clean and freeze: a request that is not the shape is :protocol, one that is reaches the library", %{db: db} do
    # (no database here: a request of the right shape is refused as :app)
    assert {:error, {:protocol, _, _}} = Hosi.erase(db, [[:everything]])
    assert {:error, {:protocol, _, _}} = Hosi.erase(db, [[:ids, "x"]])
    assert {:error, {:app, :"bad-argument", _}} =
             Hosi.erase(db, [[:ids, [1, 2]], [:range, 1, 9], [:between, 0, 10], [:"owned-by", [:a], [3]], [:"owned-by-between", [:a], [3], 0, 10]])
    assert {:error, {:app, :"bad-argument", _}} = Hosi.clean(db)
    assert {:error, {:app, :"bad-argument", _}} = Hosi.clean(db, max_chunks: 2)
    assert {:error, {:protocol, _, _}} = Hosi.clean(db, max_chunks: "two")
    assert {:error, {:app, :"bad-argument", _}} = Hosi.freeze(db)
    assert {:error, {:app, :"bad-argument", _}} = Hosi.freeze(db, max_units: 1)
  end

  test "an address that is not the shape is said at the first request" do
    {:ok, db} = Hosi.start_link(open: %{dir: 5})
    assert {:error, {:protocol, :"bad-open", msg}} = Hosi.ping(db)
    assert msg =~ ":dir"
    Hosi.stop(db)
  end

  test "many at once are each answered once, to their own caller", %{db: db} do
    results =
      1..500
      |> Task.async_stream(fn i -> {i, Hosi.ping(db, i)} end, max_concurrency: 50, ordered: false)
      |> Enum.map(fn {:ok, r} -> r end)

    assert length(results) == 500
    assert Enum.all?(results, fn {i, r} -> r == {:ok, i} end)
  end

  test "stop closes; a later request finds no process", %{db: db} do
    assert :ok = Hosi.stop(db)
    assert catch_exit(Hosi.ping(db)) |> elem(0) == :noproc
  end

  test "stop answers what is still waiting", %{db: db} do
    parent = self()

    callers =
      for i <- 1..50 do
        spawn_link(fn -> send(parent, {:done, i, catch_exit_or_value(fn -> Hosi.ping(db, i) end)}) end)
      end

    Hosi.stop(db)
    # every caller got an answer or saw the process go; none is left hanging
    for i <- 1..length(callers) do
      assert_receive {:done, ^i, _}, 5_000
    end
  end

  defp catch_exit_or_value(f) do
    try do
      f.()
    catch
      :exit, why -> {:exit, why}
    end
  end

  test "a process killed with its connection does not leave the library with a dangling callback" do
    for _ <- 1..20 do
      {:ok, db} = Hosi.start_link(open: %{})
      Process.unlink(db)
      for i <- 1..20, do: spawn(fn -> catch_exit_or_value(fn -> Hosi.ping(db, i) end) end)
      Process.exit(db, :kill)
    end

    # the library lives on: a fresh connection still answers
    {:ok, db} = Hosi.start_link(open: %{})
    assert Hosi.ping(db, :still) == {:ok, :still}
    Hosi.stop(db)
  end
end
