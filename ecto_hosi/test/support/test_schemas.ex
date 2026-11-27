defmodule Ecto.Hosi.Test.User do
  use Ecto.Schema
  use Ecto.Hosi.Schema
  import Ecto.Changeset

  hosi_unique :email

  schema "user" do
    field(:name, :string)
    field(:email, :string)
    field(:age, :integer)
    field(:score, :float)
    field(:active, :boolean, default: true)
    field(:born, :date)
    field(:balance, :decimal)
    field(:lock_version, :integer, default: 1)
    has_many(:posts, Ecto.Hosi.Test.Post)
    timestamps(type: :utc_datetime_usec)
  end

  def changeset(user, attrs) do
    user
    |> cast(attrs, [:name, :email, :age, :score, :active, :born, :balance])
    |> validate_required([:name])
    |> unique_constraint(:email)
  end

  def locked(user, attrs) do
    user |> cast(attrs, [:name]) |> optimistic_lock(:lock_version)
  end
end

defmodule Ecto.Hosi.Test.Post do
  use Ecto.Schema
  use Ecto.Hosi.Schema
  import Ecto.Changeset

  hosi_tuple :user_id, :inserted_at

  schema "post" do
    field(:title, :string)
    field(:likes, :integer, default: 0)
    belongs_to(:user, Ecto.Hosi.Test.User)
    timestamps(type: :utc_datetime_usec)
  end

  def changeset(post, attrs),
    do: post |> cast(attrs, [:title, :likes, :user_id]) |> validate_required([:title])
end

defmodule Ecto.Hosi.Test.Doc do
  use Ecto.Schema
  import Ecto.Changeset

  schema "doc" do
    field(:meta, :map)
    field(:tags, {:array, :string})
    field(:items, {:array, :map})
    field(:scores, {:array, :integer})
    field(:blob, :binary)
  end

  def changeset(doc, attrs), do: cast(doc, attrs, [:meta, :tags, :items, :scores, :blob])
end

defmodule Ecto.Hosi.Test.Follow do
  use Ecto.Schema

  schema "follow" do
    field(:from, :integer)
    field(:to, :integer)
  end
end

defmodule Ecto.Hosi.Test.Rules do
  use Ecto.Hosi.Rules
  alias Ecto.Hosi.Test.Follow

  # a reaches b if a follows b, or follows someone who reaches b
  rule :reachable, [a, b] do
    match Follow, from: a, to: b
  end

  rule :reachable, [a, c] do
    match Follow, from: a, to: b
    reachable(b, c)
  end

  # follows someone, but nobody follows them
  rule :unfollowed, [a] do
    match Follow, from: a
    not match(Follow, to: a)
  end

  # follows each other
  rule :mutual, [a, b] do
    match Follow, from: a, to: b
    match Follow, from: b, to: a
    a < b
  end
end

defmodule Ecto.Hosi.Test.Repo do
  use Ecto.Repo, otp_app: :ecto_hosi, adapter: Ecto.Adapters.Hosi
end

defmodule Ecto.Hosi.Test.Pair do
  use Ecto.Schema
  use Ecto.Hosi.Schema
  import Ecto.Changeset

  hosi_unique [:a, :b], name: "pair_a_b_index"
  hosi_unique :email, lower: [:email], where: [null: [:domain]], name: "pair_local_email_index"

  schema "pair" do
    field(:a, :integer)
    field(:b, :integer)
    field(:email, :string)
    field(:domain, :string)
  end

  def changeset(pair, attrs) do
    pair
    |> cast(attrs, [:a, :b, :email, :domain])
    |> unique_constraint(:a, name: :pair_a_b_index)
    |> unique_constraint(:email, name: :pair_local_email_index)
  end
end

defmodule Ecto.Hosi.Test.Team do
  use Ecto.Schema
  schema("team", do: field(:name, :string))
end

defmodule Ecto.Hosi.Test.Member do
  use Ecto.Schema
  use Ecto.Hosi.Schema
  hosi_ref :team_id, to: Ecto.Hosi.Test.Team, on_delete: :cascade
  schema "member" do
    field(:name, :string)
    belongs_to(:team, Ecto.Hosi.Test.Team)
  end
end

defmodule Ecto.Hosi.Test.Item do
  use Ecto.Schema
  use Ecto.Hosi.Schema
  hosi_ref :member_id, to: "member", on_delete: :cascade
  schema "item" do
    field(:name, :string)
    belongs_to(:member, Ecto.Hosi.Test.Member)
  end
end

defmodule Ecto.Hosi.Test.Note do
  use Ecto.Schema
  use Ecto.Hosi.Schema
  hosi_ref :team_id, to: "team", on_delete: :nilify
  hosi_unique [:team_id, :name], name: "note_team_name_index"
  schema "note" do
    field(:name, :string)
    belongs_to(:team, Ecto.Hosi.Test.Team)
  end
end

defmodule Ecto.Hosi.Test.Seat do
  use Ecto.Schema
  use Ecto.Hosi.Schema
  hosi_ref :team_id, to: "team", on_delete: :restrict, name: "seat_team_fk"
  schema "seat" do
    belongs_to(:team, Ecto.Hosi.Test.Team)
  end
end

defmodule Ecto.Hosi.Test.Entry do
  use Ecto.Schema
  use Ecto.Hosi.Schema
  hosi_append_only()
  schema "entry", do: field(:text, :string)
end

defmodule Ecto.Hosi.Test.Joiner do
  use Ecto.Repo, otp_app: :ecto_hosi, adapter: Ecto.Adapters.Hosi
end

defmodule Ecto.Hosi.Test.Ticket do
  use Ecto.Schema
  use Ecto.Hosi.Schema
  import Ecto.Changeset

  # Elixir sees "open", hosigraph keeps :open
  hosi_keyword :state, ["open", "pending", "closed"]
  hosi_unique :slug

  schema "ticket" do
    field(:title, :string)
    field(:slug, :string)
    field(:state, :string, default: "open")
  end

  def changeset(ticket, attrs),
    do:
      ticket
      |> cast(attrs, [:title, :slug, :state])
      |> validate_inclusion(:state, ["open", "pending", "closed"])
      |> unique_constraint(:slug)
end

defmodule Ecto.Hosi.Test.Link do
  use Ecto.Schema
  use Ecto.Hosi.Schema
  import Ecto.Changeset

  # the pair is hosigraph's own tuple, not a JSON text in a derived attribute
  hosi_unique [:a, :b], tuple: true, name: "link_a_b_index"

  schema "link" do
    field(:a, :integer)
    field(:b, :integer)
    field(:note, :string)
  end

  def changeset(link, attrs) do
    link
    |> cast(attrs, [:a, :b, :note])
    |> unique_constraint(:a, name: :link_a_b_index)
  end
end

# not given to the Repo: schemas that must be refused
defmodule Ecto.Hosi.Test.BadKeyword do
  use Ecto.Schema
  use Ecto.Hosi.Schema

  hosi_keyword :n, ["a", "b"]

  schema "bad_keyword" do
    field(:n, :integer)
  end
end

defmodule Ecto.Hosi.Test.BadKeywordName do
  use Ecto.Schema
  use Ecto.Hosi.Schema

  hosi_keyword :n, ["fine", "not fine/at all"]

  schema "bad_keyword_name" do
    field(:n, :string)
  end
end

defmodule Ecto.Hosi.Test.BadTuple do
  use Ecto.Schema
  use Ecto.Hosi.Schema

  hosi_unique [:a, :b], tuple: true, lower: [:b]

  schema "bad_tuple" do
    field(:a, :string)
    field(:b, :string)
  end
end

# An id type of the app's own: the Repo holds `id + 1000`, the app sees `id`.
defmodule Ecto.Hosi.Test.Shifted do
  use Ecto.Type

  def shift, do: 1000
  def type, do: :id
  def cast(v), do: Ecto.Type.cast(:integer, v)
  def load(v), do: {:ok, v - shift()}
  def dump(v) when is_integer(v), do: {:ok, v + shift()}
  def dump(_), do: :error
end

defmodule Ecto.Hosi.Test.Crate do
  use Ecto.Schema

  @primary_key {:id, Ecto.Hosi.Test.Shifted, read_after_writes: true}
  schema "crate" do
    field(:label, :string)
  end
end

defmodule Ecto.Hosi.Test.Parcel do
  use Ecto.Schema
  use Ecto.Hosi.Schema

  hosi_tuple :crate_id, :rank

  @primary_key {:id, Ecto.Hosi.Test.Shifted, read_after_writes: true}
  schema "parcel" do
    field(:rank, :integer)
    belongs_to(:crate, Ecto.Hosi.Test.Crate, type: Ecto.Hosi.Test.Shifted)
  end
end
