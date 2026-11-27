defmodule Ecto.Hosi.BelongsToTest.Author do
  use Ecto.Schema

  schema "bt_author" do
    field(:name, :string)
  end
end

# says nothing about what deleting its author does
defmodule Ecto.Hosi.BelongsToTest.Book do
  use Ecto.Schema

  schema "bt_book" do
    belongs_to(:author, Ecto.Hosi.BelongsToTest.Author)
  end
end

# says: keep the reference
defmodule Ecto.Hosi.BelongsToTest.Quote do
  use Ecto.Schema
  use Ecto.Hosi.Schema

  hosi_ref :author_id, to: Ecto.Hosi.BelongsToTest.Author, on_delete: :keep

  schema "bt_quote" do
    belongs_to(:author, Ecto.Hosi.BelongsToTest.Author)
  end
end

# the parent says it, the Ecto way
defmodule Ecto.Hosi.BelongsToTest.Shelf do
  use Ecto.Schema

  schema "bt_shelf" do
    has_many(:labels, Ecto.Hosi.BelongsToTest.Label, foreign_key: :shelf_id, on_delete: :delete_all)
  end
end

defmodule Ecto.Hosi.BelongsToTest.Label do
  use Ecto.Schema

  schema "bt_label" do
    belongs_to(:shelf, Ecto.Hosi.BelongsToTest.Shelf)
  end
end

defmodule Ecto.Hosi.BelongsToTest do
  use Ecto.Hosi.Case, async: true

  import ExUnit.CaptureLog

  alias Ecto.Hosi.BelongsToTest.{Author, Book, Label, Quote, Shelf}

  test "a belongs_to with no delete rule is warned about at the start" do
    log = capture_log(fn -> start_repo(schemas: [Author, Book, Quote, Shelf, Label]) end)

    assert log =~ "BelongsToTest.Book.author_id"
    assert log =~ "on_delete"
    refute log =~ "BelongsToTest.Quote.author_id"
    refute log =~ "BelongsToTest.Label.shelf_id"
  end

  test "not when what it points at is not kept here" do
    log = capture_log(fn -> start_repo(schemas: [Book]) end)
    refute log =~ "BelongsToTest.Book.author_id"
  end

  test "on_delete: :keep deletes the parent and leaves the reference" do
    start_repo(schemas: [Author, Quote])
    a = Repo.insert!(struct(Author, name: "a"))
    q = Repo.insert!(struct(Quote, author_id: a.id))

    assert {:ok, _} = Repo.delete(a)
    assert Repo.get(Quote, q.id).author_id == a.id
  end
end
