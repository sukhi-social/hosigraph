defmodule Ecto.Hosi.RulesTest do
  use ExUnit.Case, async: true

  defp compile(code) do
    Code.compile_string("""
    defmodule T#{System.unique_integer([:positive])} do
      use Ecto.Hosi.Rules
      #{code}
    end
    """)
  end

  test "recursion without negation is fine" do
    assert [{_mod, _}] =
             compile("""
             rule :r, [a, b] do
               match Ecto.Hosi.Test.Follow, from: a, to: b
             end
             rule :r, [a, c] do
               match Ecto.Hosi.Test.Follow, from: a, to: b
               r(b, c)
             end
             """)
  end

  test "a rule may not reach itself through `not` (stratification)" do
    assert_raise ArgumentError, ~r/not stratified/, fn ->
      compile("""
      rule :p, [a] do
        match Ecto.Hosi.Test.Follow, from: a
        not q(a)
      end
      rule :q, [a] do
        match Ecto.Hosi.Test.Follow, from: a
        not p(a)
      end
      """)
    end

    assert_raise ArgumentError, ~r/not stratified/, fn ->
      compile("""
      rule :p, [a] do
        match Ecto.Hosi.Test.Follow, from: a
        not p(a)
      end
      """)
    end
  end

  test "negation of a different, lower rule is fine" do
    assert [{_, _}] =
             compile("""
             rule :base, [a] do
               match Ecto.Hosi.Test.Follow, from: a
             end
             rule :top, [a] do
               match Ecto.Hosi.Test.Follow, to: a
               not base(a)
             end
             """)
  end

  test "every head variable must be bound" do
    assert_raise ArgumentError, ~r/variable z is not bound/, fn ->
      compile("""
      rule :p, [a, z] do
        match Ecto.Hosi.Test.Follow, from: a
      end
      """)
    end
  end

  test "variables of a comparison or a not must be bound elsewhere" do
    assert_raise ArgumentError, ~r/variable y is not bound/, fn ->
      compile("""
      rule :p, [a] do
        match Ecto.Hosi.Test.Follow, from: a
        a < y
      end
      """)
    end

    assert_raise ArgumentError, ~r/variable y is not bound/, fn ->
      compile("""
      rule :p, [a] do
        match Ecto.Hosi.Test.Follow, from: a
        not match(Ecto.Hosi.Test.Follow, from: y)
      end
      """)
    end
  end

  test "field names are checked against the schema while compiling" do
    assert_raise ArgumentError, ~r/has no field :nope/, fn ->
      compile("""
      rule :p, [a] do
        match Ecto.Hosi.Test.Follow, nope: a
      end
      """)
    end
  end

  test "a rule keeps one number of arguments" do
    assert_raise ArgumentError, ~r/takes \[1\]/, fn ->
      compile("""
      rule :p, [a] do
        match Ecto.Hosi.Test.Follow, from: a
      end
      rule :r, [a] do
        match Ecto.Hosi.Test.Follow, from: a
        p(a, a)
      end
      """)
    end
  end

  test "a head may not repeat a variable" do
    assert_raise ArgumentError, ~r/repeats a variable/, fn ->
      compile("""
      rule :p, [a, a] do
        match Ecto.Hosi.Test.Follow, from: a
      end
      """)
    end
  end

  test "^values belong in queries, not rules" do
    assert_raise ArgumentError, ~r/only allowed in queries/, fn ->
      compile("""
      rule :p, [a] do
        match Ecto.Hosi.Test.Follow, from: ^a
      end
      """)
    end
  end
end
