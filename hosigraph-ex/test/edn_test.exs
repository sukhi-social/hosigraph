defmodule Hosi.EdnTest do
  use ExUnit.Case, async: true
  alias Hosi.Edn

  test "writes the things the wire takes" do
    assert Edn.encode(%{args: [1, "é", :k, 2.5, true, nil]}) == ~s({:args [1 "é" :k 2.5 true nil]})
    assert Edn.encode(%{}) == "{}"

    assert Edn.encode(["a\"b\\c\n", <<1>>, <<127>>]) == ~s(["a\\"b\\\\c\\n" "\\u0001" "\\u007f"])
    assert Edn.encode([:create, [:tmp, 1], :"person/name", "Frege"]) ==
             ~s([:create [:tmp 1] :person/name "Frege"])
  end

  test "refuses what it cannot say" do
    assert_raise ArgumentError, fn -> Edn.encode(<<0xFF, 0xFE>>) end
    assert_raise ArgumentError, fn -> Edn.encode({:a, 1}) end
  end

  test "reads what takka writes (a comma is a space)" do
    assert {:ok, %{ok: %{name: "hosi", version: 1, ops: [:ping, :abi]}}} =
             Edn.decode(~s({:ok {:name "hosi", :version 1, :ops [:ping :abi]}}))
  end

  test "scalars" do
    assert Edn.decode("nil") == {:ok, nil}
    assert Edn.decode("true") == {:ok, true}
    assert Edn.decode("-42") == {:ok, -42}
    assert Edn.decode("9223372036854775807") == {:ok, 9_223_372_036_854_775_807}
    assert Edn.decode("1.5") == {:ok, 1.5}
    assert Edn.decode("1.0e10") == {:ok, 1.0e10}
    assert Edn.decode("2e3") == {:ok, 2000.0}
    assert Edn.decode("##NaN") == {:ok, {:float, :nan}}
    assert Edn.decode("##-Inf") == {:ok, {:float, :neg_inf}}
    assert Edn.decode(":person/name") == {:ok, :"person/name"}
    assert Edn.decode("nilly") == {:ok, {:symbol, "nilly"}}
  end

  test "strings, with the escapes and non-ASCII" do
    assert Edn.decode(~s("a\\n\\"é\\u0001")) == {:ok, "a\n\"é\u0001"}
    assert {:error, _} = Edn.decode(~s("open))
    assert {:error, _} = Edn.decode(~S("\q"))
  end

  test "collections" do
    assert Edn.decode("[1 [2] (3)]") == {:ok, [1, [2], [3]]}
    assert Edn.decode("\#{1 2}") == {:ok, MapSet.new([1, 2])}
    assert Edn.decode("{:a [1 2], :b {}}") == {:ok, %{a: [1, 2], b: %{}}}
    assert Edn.decode("[1 ; a comment\n 2]") == {:ok, [1, 2]}
  end

  test "bad input is an error, not a crash" do
    for bad <- ["", "[1", "{:a}", "1 2", "#inst \"x\"", "]", "{:a 1"] do
      assert {:error, _} = Edn.decode(bad), "for #{inspect(bad)}"
    end
  end

  test "a term written is the term read" do
    t = %{"s" => %{b: []}, a: [1, -2, "x y", nil, true, 3.25, :"k/v"]}
    assert Edn.decode(Edn.encode(t)) == {:ok, t}
  end
end
