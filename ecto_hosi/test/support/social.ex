# Views as an ActivityPub server would write them: shared attributes in a vocabulary, a record
# type per kind of record, and records that show up in more than one view.
defmodule Ecto.Hosi.Test.AP do
  use Ecto.Hosi.Vocabulary, prefix: "ap"

  attribute :type, :keyword,
    values: ["person", "profile", "note", "media", "follow", "like", "boost", "report", "tombstone"],
    record_type: true

  attribute :former_type, :keyword, values: ["person", "note", "media"]
  attribute :uuid, Ecto.UUID, unique: true, final: true
  attribute :uri, :string, unique: true, final: true
  attribute :alt_id, :string, unique: true, many: true
  attribute :attributed_to, :ref, on_delete: :keep, final: true
  attribute :published, :utc_datetime_usec
  attribute :deleted, :utc_datetime_usec
end

defmodule Ecto.Hosi.Test.Actor do
  use Ecto.Schema
  use Ecto.Hosi.View
  import Ecto.Changeset

  hosi_where type: "person"
  hosi_owner :nobody
  hosi_unique :username

  hosi_tombstone keep: [:uri, :username],
                 set: [type: "tombstone", former_type: "person"],
                 mark: :deleted_at

  schema "actor" do
    hosi_attributes Ecto.Hosi.Test.AP,
      type: :type,
      former_type: :former_type,
      uri: :uri,
      deleted_at: :deleted

    field :username, :string
    field :key, :string
  end

  def changeset(actor, attrs),
    do: actor |> cast(attrs, [:uri, :username, :key]) |> unique_constraint(:username)
end

# what an account says about itself is a record of its own, so that an erase takes it
defmodule Ecto.Hosi.Test.Profile do
  use Ecto.Schema
  use Ecto.Hosi.View

  hosi_where type: "profile"
  hosi_owner :actor_id

  schema "profile" do
    hosi_attributes Ecto.Hosi.Test.AP, type: :type
    field :actor_id, :id
    field :display_name, :string
  end
end

defmodule Ecto.Hosi.Test.Status do
  use Ecto.Schema
  use Ecto.Hosi.View
  import Ecto.Changeset

  hosi_where type: "note"
  hosi_owner :attributed_to_id
  hosi_tuple :attributed_to_id, :published_at

  hosi_tombstone keep: [:uuid, :uri, :attributed_to_id],
                 set: [type: "tombstone", former_type: "note"],
                 mark: :deleted_at

  hosi_keyword :visibility, ["public", "followers"]
  hosi_ref :reply_to_id, on_delete: :keep
  hosi_many [:tags, :mention_ids]
  hosi_ref :mention_ids, on_delete: :nilify

  schema "status" do
    hosi_attributes Ecto.Hosi.Test.AP,
      type: :type,
      former_type: :former_type,
      uuid: :uuid,
      uri: :uri,
      alt_ids: :alt_id,
      attributed_to_id: :attributed_to,
      published_at: :published,
      deleted_at: :deleted

    field :content, :string
    field :visibility, :string, default: "public"
    field :reply_to_id, :id
    field :tags, {:array, :string}
    field :mention_ids, {:array, :id}
  end

  def changeset(status, attrs) do
    status
    |> cast(attrs, [:uri, :alt_ids, :content, :tags, :visibility])
    |> unique_constraint(:uri, name: "status_ap/uri_index")
    |> unique_constraint(:alt_ids, name: "status_ap/alt-id_index")
  end
end

# another look at some of the same records: whatever is pinned
defmodule Ecto.Hosi.Test.Pinned do
  use Ecto.Schema
  use Ecto.Hosi.View

  hosi_where :pinned_at
  hosi_owner :nobody

  schema "pinned" do
    field :pinned_at, :utc_datetime_usec
    field :content, :string
  end
end

defmodule Ecto.Hosi.Test.Media do
  use Ecto.Schema
  use Ecto.Hosi.View

  hosi_where type: "media"
  hosi_owner :attributed_to_id

  hosi_tombstone keep: [:attributed_to_id],
                 set: [type: "tombstone", former_type: "media"],
                 mark: :deleted_at

  hosi_ref :status_id, on_delete: :cascade
  hosi_file :path

  schema "media" do
    hosi_attributes Ecto.Hosi.Test.AP,
      type: :type,
      former_type: :former_type,
      attributed_to_id: :attributed_to,
      deleted_at: :deleted

    field :status_id, :id
    field :path, :string, source: :"s3/path"
  end
end

defmodule Ecto.Hosi.Test.Following do
  use Ecto.Schema
  use Ecto.Hosi.View
  import Ecto.Changeset

  hosi_where type: "follow"
  hosi_owner :from_id
  hosi_ref :from_id, on_delete: :cascade
  hosi_ref :to_id, on_delete: :cascade
  hosi_unique [:from_id, :to_id], tuple: true

  schema "following" do
    hosi_attributes Ecto.Hosi.Test.AP, type: :type
    field :from_id, :id, source: :"follow/from"
    field :to_id, :id, source: :"follow/to"
  end

  def changeset(follow, attrs) do
    follow
    |> cast(attrs, [:from_id, :to_id])
    |> unique_constraint(:from_id, name: "following_follow/from_follow/to_index")
  end
end

defmodule Ecto.Hosi.Test.Like do
  use Ecto.Schema
  use Ecto.Hosi.View

  hosi_where type: "like"
  hosi_owner :liker_id
  hosi_ref :liker_id, on_delete: :cascade
  hosi_ref :status_id, on_delete: :cascade

  schema "like" do
    hosi_attributes Ecto.Hosi.Test.AP, type: :type
    field :liker_id, :id, source: :"like/by"
    field :status_id, :id
  end
end

# shares `ap/attributed-to` and `ap/published` with a note, and is not one
defmodule Ecto.Hosi.Test.Boost do
  use Ecto.Schema
  use Ecto.Hosi.View

  hosi_where type: "boost"
  hosi_owner :attributed_to_id

  schema "boost" do
    hosi_attributes Ecto.Hosi.Test.AP,
      type: :type,
      attributed_to_id: :attributed_to,
      published_at: :published
  end
end

# holds on to the account it is about
defmodule Ecto.Hosi.Test.Report do
  use Ecto.Schema
  use Ecto.Hosi.View

  hosi_where type: "report"
  hosi_owner :nobody
  hosi_ref :about_id, on_delete: :restrict, name: "report_about_fk"

  schema "report" do
    hosi_attributes Ecto.Hosi.Test.AP, type: :type
    field :about_id, :id
  end
end

defmodule Ecto.Hosi.Test.Tombstone do
  use Ecto.Schema
  use Ecto.Hosi.View

  hosi_where type: "tombstone"
  hosi_owner :nobody

  schema "tombstone" do
    hosi_attributes Ecto.Hosi.Test.AP,
      type: :type,
      former_type: :former_type,
      uri: :uri,
      deleted_at: :deleted

    field :owner_id, :id, source: :"hosi/owner"
    field :path, :string, source: :"s3/path"
  end
end

# a file, and no tombstone: never removed outright
defmodule Ecto.Hosi.Test.Upload do
  use Ecto.Schema
  use Ecto.Hosi.View

  hosi_where :upload_path
  hosi_owner :nobody
  hosi_file :upload_path

  schema "upload" do
    field :upload_path, :string
  end
end

defmodule Ecto.Hosi.Test.Social do
  alias Ecto.Hosi.Test, as: T

  def schemas do
    [T.Actor, T.Profile, T.Status, T.Pinned, T.Media, T.Following, T.Like, T.Boost, T.Report] ++
      [T.Tombstone, T.Upload]
  end
end
