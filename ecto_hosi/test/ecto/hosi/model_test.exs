defmodule Ecto.Hosi.ModelTest do
  # A Repo whose declarations do not agree does not start, and says why.
  use ExUnit.Case, async: true

  alias Ecto.Hosi.Model
  alias Ecto.Hosi.Test.{AP, Social}

  # compile a module from its source, and answer its name
  defp view(body, opts \\ []) do
    name = "Ecto.Hosi.ModelTest.V#{System.unique_integer([:positive])}"
    use_view = if Keyword.get(opts, :view, true), do: "use Ecto.Hosi.View", else: ""

    [{module, _} | _] =
      Code.compile_string("""
      defmodule #{name} do
        use Ecto.Schema
        #{use_view}
        #{body}
      end
      """)

    module
  end

  defp refused(modules, message), do: assert_raise(ArgumentError, message, fn -> Model.build(modules) end)

  test "the views of the tests agree" do
    model = Model.build(Social.schemas())

    assert model.schema[:"ap/type"] == %{type: :keyword}
    assert model.schema[:"ap/uuid"] == %{type: :uuid, unique: :value}
    assert model.schema[:"ap/alt-id"] == %{type: :string, card: :many, unique: :value}
    assert model.schema[:"ap/attributed-to"] == %{type: :ref}
    assert model.schema[:"hosi/owner"] == %{type: :ref}
    assert model.schema[:content] == %{type: :string}
    assert %{tuple: [:"ap/attributed-to", :"ap/published"]} =
             model.schema[:"hosi/ap.attributed-to+ap.published"]
  end

  test "a schema has to be a view" do
    refused([view(~s|schema "a" do field :x, :string end|, view: false)], ~r/Add `use Ecto.Hosi.View`/)
  end

  test "a view says which facts make a record show up, and who owns the records" do
    refused([view(~s|hosi_owner :nobody\n schema "a" do field :x, :string end|)], ~r/hosi_where/)
    refused([view(~s|hosi_where :x\n schema "a" do field :x, :string end|)], ~r/who owns.*hosi_owner/s)
  end

  test "a name has one definition" do
    a = view(~s|hosi_where :x\n hosi_owner :nobody\n schema "a" do field :x, :string end|)
    b = view(~s|hosi_where :x\n hosi_owner :nobody\n schema "b" do field :x, :integer end|)
    refused([a, b], ~r/:x is declared in two ways: :string and :integer/)

    c = view(~s|hosi_where :x\n hosi_owner :nobody\n schema "c" do field :x, {:array, :string} end|)

    d =
      view(
        ~s|hosi_where :x\n hosi_owner :nobody\n hosi_many :x\n schema "d" do field :x, {:array, :string} end|
      )

    refused([c, d], ~r/declared in two ways.*many/)
  end

  test "a rule said once holds for the other views of the attribute" do
    a = view(~s|hosi_where :x\n hosi_owner :nobody\n hosi_unique :x\n schema "a" do field :x, :string end|)
    b = view(~s|hosi_where :x\n hosi_owner :nobody\n schema "b" do field :x, :string end|)
    assert Model.build([a, b]).schema[:x] == %{type: :string, unique: :value}
  end

  test "a vocabulary owns its prefix" do
    a =
      view(
        ~s|hosi_where :t\n hosi_owner :nobody\n| <>
          ~s|schema "a" do\n hosi_attributes #{inspect(AP)}, t: :type\n field :x, :string, source: :"ap/mine"\n end|
      )

    refused([a], ~r|owns the prefix ap/ and has no attribute :ap/mine|)
  end

  test "the adapter's and Datalevin's names are not a view's" do
    for source <- [~s|:"hosi/mine"|, ~s|:"db/mine"|, ~s|:"a b"|] do
      v = view(~s|hosi_where :x\n hosi_owner :nobody\n schema "a" do field :x, :string, source: #{source} end|)
      assert_raise ArgumentError, fn -> Model.build([v]) end
    end
  end

  test "a field that hosi_where gives a value has it as its default" do
    v = view(~s|hosi_where kind: "a"\n hosi_owner :nobody\n schema "a" do field :kind, :string end|)
    refused([v], ~r/give the field the same default/)
  end

  test "a tombstone leaves the view, and belongs to one record type" do
    body = fn set ->
      """
      hosi_where kind: "a"
      hosi_owner :nobody
      hosi_record_type :kind
      hosi_tombstone set: #{set}
      schema "a" do
        field :kind, :string, default: "a"
        field :x, :string
      end
      """
    end

    refused([view(body.(~s|[x: "gone"]|))], ~r/must change the record type/)
    refused([view(body.(~s|[kind: "g"]|)), view(body.(~s|[kind: "h"]|))], ~r/two different tombstones|declare different tombstones/)

    refused(
      [view(~s|hosi_where :x\n hosi_owner :nobody\n hosi_tombstone set: [x: "y"]\n schema "a" do field :x, :string end|)],
      ~r/needs a view of a record type/
    )
  end

  test "a final reference cannot be nilified" do
    v =
      view("""
      hosi_where :r
      hosi_owner :nobody
      hosi_final :r
      hosi_ref :r, on_delete: :nilify
      schema "a" do
        field :r, :id
      end
      """)

    refused([v], ~r/final and on_delete: :nilify/)
  end

  test "the owner is the id of one record" do
    v = view(~s|hosi_where :x\n hosi_owner :x\n schema "a" do field :x, :string end|)
    refused([v], ~r/hosi_owner :x needs a field/)
  end
end
