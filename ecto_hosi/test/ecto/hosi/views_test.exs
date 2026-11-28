defmodule Ecto.Hosi.ViewsTest do
  # Facts, not tables: views of shared attributes, and what create, update and delete mean there.
  use Ecto.Hosi.Case, async: true

  import Ecto.Hosi.Query

  alias Ecto.Changeset

  alias Ecto.Hosi.Test.{
    Actor,
    Boost,
    Following,
    Like,
    Media,
    Pinned,
    Social,
    Status,
    Tombstone,
    Upload
  }

  setup do
    start_repo(schemas: Social.schemas())
    :ok
  end

  defp actor!(name \\ "ada"),
    do: Repo.insert!(%Actor{username: name, uri: "https://x/users/#{name}"})

  defp status!(actor, attrs \\ []) do
    n = System.unique_integer([:positive])

    base = [
      attributed_to_id: actor.id,
      uri: "https://x/notes/#{n}",
      uuid: Ecto.UUID.generate(version: 7),
      content: "hello",
      published_at: ~U[2026-01-01 00:00:00.000000Z]
    ]

    Repo.insert!(struct!(Status, Keyword.merge(base, attrs)))
  end

  defp facts_of(id), do: for({{^id, a}, v} <- facts(), into: %{}, do: {a, Enum.sort(v)})
  defp pin(id, at \\ DateTime.utc_now()), do: Repo.update(Changeset.change(%Pinned{id: id}, pinned_at: at))

  describe "creating through a view" do
    test "writes what hosi_where asks, the maker as hosi/owner, and the attributes by name" do
      a = actor!()
      s = status!(a, tags: ["b", "a", "b"])
      f = facts_of(s.id)

      assert f[:"ap/type"] == [:note]
      assert f[:"hosi/owner"] == [a.id]
      assert f[:"ap/attributed-to"] == [a.id]
      assert f[:content] == ["hello"]
      assert f[:visibility] == [:public]
      assert f[:tags] == ["a", "b"]
      assert [{:uuid, uuid}] = f[:"ap/uuid"]
      assert uuid == s.uuid
      refute Map.has_key?(f, :"hosi/owner-of")
      assert facts_of(a.id) |> Map.has_key?(:"hosi/owner") == false
    end

    test "needs the maker" do
      assert_raise ArgumentError, ~r/owned by its maker.*attributed_to_id/s, fn ->
        Repo.insert(%Status{content: "x"})
      end
    end

    test "another value than hosi_where writes is refused" do
      a = actor!()

      assert_raise ArgumentError, ~r/has :ap\/type "note".*"person"/s, fn ->
        Repo.insert(%Status{attributed_to_id: a.id, type: "person"})
      end
    end

    test "a field that only has to be there has to be given" do
      assert_raise ArgumentError, ~r/:pinned_at.*hosi_where/s, fn -> Repo.insert(%Pinned{content: "x"}) end
    end

    test "a unique value is held across views, as one attribute" do
      a = actor!()
      s = status!(a, uri: "https://x/same")

      assert {:error, cs} =
               %Status{attributed_to_id: a.id}
               |> Status.changeset(%{uri: "https://x/same"})
               |> Repo.insert()

      assert cs.errors[:uri]

      # the actor view has the same attribute, so the same rule
      assert_raise Ecto.ConstraintError, ~r/actor_ap\/uri_index/, fn ->
        Repo.insert(%Actor{username: "bo", uri: s.uri})
      end
    end
  end

  describe "one record, several views" do
    test "an update through another view adds facts, and a nil takes them away" do
      a = actor!()
      s = status!(a)
      assert Repo.get(Pinned, s.id) == nil

      assert {:ok, _} = pin(s.id)
      pinned = Repo.get!(Pinned, s.id)
      assert pinned.content == "hello"
      assert Repo.get!(Status, s.id).content == "hello"
      assert [%{id: id}] = Repo.all(Pinned)
      assert id == s.id

      {:ok, _} = pinned |> Changeset.change(pinned_at: nil) |> Repo.update()
      assert Repo.get(Pinned, s.id) == nil
      assert Repo.get!(Status, s.id).content == "hello"
    end

    test "queries answer only the records whose facts say so" do
      a = actor!()
      s = status!(a)
      Repo.insert!(%Boost{attributed_to_id: a.id, published_at: ~U[2026-01-02 00:00:00.000000Z]})

      assert Repo.all(from s in Status, select: s.id) == [s.id]
      assert Repo.get(Status, a.id) == nil
      assert Repo.all(from s in Status, where: s.attributed_to_id == ^a.id, select: s.id) == [s.id]
      assert Repo.aggregate(Status, :count) == 1
      assert Repo.aggregate(Boost, :count) == 1
    end

    test "a view of a record type changes only records of that type" do
      a = actor!()

      assert_raise Ecto.StaleEntryError, fn ->
        Repo.update(Changeset.change(%Status{id: a.id}, content: "not a note"))
      end

      assert {0, nil} = Repo.update_all(from(s in Status, where: s.id == ^a.id), set: [content: "x"])
      refute Map.has_key?(facts_of(a.id), :content)
    end

    test "an update of an id that holds nothing is stale" do
      assert_raise Ecto.StaleEntryError, fn -> pin(999_999) end
      assert facts_of(999_999) == %{}
    end

    test "an update cannot change the record type, or a final attribute" do
      a = actor!()
      s = status!(a)

      assert_raise Ecto.Hosi.Error, ~r/:ap\/type says what a record is/, fn ->
        Repo.update(Changeset.change(s, type: "person"))
      end

      assert_raise Ecto.Hosi.Error, ~r/:ap\/uri is final/, fn ->
        Repo.update(Changeset.change(s, uri: "https://x/moved"))
      end

      # the same value is no change
      assert {:ok, _} = s |> Changeset.change() |> Changeset.force_change(:uri, s.uri) |> Repo.update()
      assert Repo.get!(Status, s.id).uri == s.uri
    end
  end

  describe "many values and references to any record" do
    test "a set: found by one of them, changed by difference" do
      a = actor!()
      s = status!(a, tags: ["x", "y"])
      other = status!(a, tags: ["z"])
      none = status!(a)

      assert Repo.all(from s in Status, where: "x" in s.tags, select: s.id) == [s.id]
      assert Repo.all(from s in Status, where: is_nil(s.tags), select: s.id) == [none.id]
      assert Repo.get!(Status, none.id).tags == []

      assert_raise ArgumentError, ~r/many values is a set/, fn ->
        Repo.all(from s in Status, where: s.tags == ^["x"])
      end

      {:ok, _} = s |> Changeset.change(tags: ["y", "w"]) |> Repo.update()
      assert facts_of(s.id)[:tags] == ["w", "y"]
      assert Repo.get!(Status, s.id).tags == ["w", "y"]
      assert Repo.get!(Status, other.id).tags == ["z"]

      history = for %{attr: :tags, kind: k, val: v} <- Repo.history(s.id), do: {k, v}
      assert {:delete, "x"} in history
      refute {:delete, "y"} in history
    end

    test "a unique attribute with many values holds each of them once" do
      a = actor!()
      status!(a, alt_ids: ["u:1", "u:2"])

      assert {:error, cs} =
               %Status{attributed_to_id: a.id}
               |> Status.changeset(%{alt_ids: ["u:3", "u:2"]})
               |> Repo.insert()

      assert cs.errors[:alt_ids]
      assert Repo.aggregate(Status, :count) == 1
    end

    test "references with many values are nilified one by one" do
      a = actor!()
      [b, c] = [actor!("b"), actor!("c")]
      s = status!(a, mention_ids: [b.id, c.id])

      {:ok, _} = Repo.delete(b)
      assert Repo.get!(Status, s.id).mention_ids == [c.id]
    end

    test "a reference that keeps pointing at a tombstone" do
      a = actor!()
      s = status!(a)
      reply = status!(a, reply_to_id: s.id)

      {:ok, _} = Repo.delete(s)
      assert Repo.get!(Status, reply.id).reply_to_id == s.id
      assert Repo.get(Status, s.id) == nil
      assert Repo.get!(Tombstone, s.id).uri == s.uri
    end
  end

  describe "UUID fields" do
    test "are UUIDs in hosigraph and text in Elixir" do
      a = actor!()
      s = status!(a)
      uuid = s.uuid

      assert Repo.get_by!(Status, uuid: uuid).id == s.id
      assert Repo.get_by!(Status, uuid: String.upcase(uuid)).id == s.id
      assert Repo.all(from s in Status, where: s.uuid in ^[uuid], select: s.uuid) == [uuid]

      assert Repo.datalog(datalog(find: [s], where: [match(Status, id: s, uuid: ^uuid)])) == [[s.id]]

      assert_raise ArgumentError, ~r/:ap\/uuid is a UUID attribute/, fn ->
        Repo.datalog(%{q: "[:find ?e :in $ ?u :where [?e :ap/uuid ?u]]", inputs: [[:scalar, uuid]]})
      end

      assert Repo.datalog(%{q: "[:find ?e :where [?e :ap/uuid #uuid \"#{uuid}\"]]", inputs: []}) == [[s.id]]
      assert [%{value: ^uuid}] = Repo.history(s).uuid
    end
  end

  describe "delete" do
    test "through the type view leaves the tombstone" do
      a = actor!()
      s = status!(a, tags: ["t"], alt_ids: ["u:9"])
      pin(s.id)

      assert {:ok, _} = Repo.delete(s)

      f = facts_of(s.id)
      assert f[:"ap/type"] == [:tombstone]
      assert f[:"ap/former-type"] == [:note]
      assert f[:"ap/uri"] == [s.uri]
      assert f[:"hosi/owner"] == [a.id]
      assert [_] = f[:"ap/deleted"]

      for gone <- [:content, :tags, :pinned_at, :visibility, :"ap/published"] do
        refute Map.has_key?(f, gone), "#{gone} was left"
      end

      # `ap/uri` stays taken; `ap/alt-id` was not listed to be kept
      refute Map.has_key?(f, :"ap/alt-id")
      assert Repo.get(Status, s.id) == nil
      assert Repo.get(Pinned, s.id) == nil
      assert %Tombstone{former_type: "note", deleted_at: %DateTime{}} = Repo.get!(Tombstone, s.id)
      assert_raise Ecto.StaleEntryError, fn -> Repo.delete(s) end
    end

    test "through a view that does not choose its type is refused, and says the two ways" do
      a = actor!()
      s = status!(a)
      pin(s.id)
      before = facts_of(s.id)

      error = assert_raise Ecto.Hosi.Error, fn -> Repo.delete(Repo.get!(Pinned, s.id)) end

      assert error.message =~ "only a view of that type deletes it"
      assert error.message =~ "Ecto.Hosi.Test.Status"
      assert error.message =~ "update the view's fields to nil"
      assert facts_of(s.id) == before
    end

    test "a record with no type is removed only when the view shows all of it" do
      p = Repo.insert!(%Pinned{pinned_at: DateTime.utc_now()})
      assert {:ok, _} = Repo.delete(p)
      assert facts_of(p.id) == %{}

      p = Repo.insert!(%Pinned{pinned_at: DateTime.utc_now()})
      direct_write([[:create, p.id, :note_to_self, "keep"]])
      assert_raise Ecto.Hosi.Error, ~r/:note_to_self/, fn -> Repo.delete(p) end
    end

    test "cascades leave the tombstone of each type, which keeps the file" do
      a = actor!()
      s = status!(a)
      m = Repo.insert!(%Media{attributed_to_id: a.id, status_id: s.id, path: "files/1.png"})
      like = Repo.insert!(%Like{liker_id: a.id, status_id: s.id})

      assert {1, nil} = Repo.delete_all(from s2 in Status, where: s2.id == ^s.id)

      assert %Tombstone{former_type: "media", path: "files/1.png", owner_id: owner} =
               Repo.get!(Tombstone, m.id)

      assert owner == a.id
      assert facts_of(like.id) == %{}
    end

    test "a record that names a file is never removed outright" do
      u = Repo.insert!(%Upload{upload_path: "files/2.png"})
      assert_raise Ecto.Hosi.Error, ~r/:upload_path.*tombstone/s, fn -> Repo.delete(u) end
      assert Repo.get(Upload, u.id)
    end
  end

  describe "write_batch" do
    test "a later step adds facts to a new record, through another view, in the same write" do
      a = actor!()
      now = DateTime.utc_now()

      assert {:ok, %{status: s, pin: p}} =
               Repo.write_batch(
                 insert:
                   {:status,
                    Changeset.change(%Status{}, attributed_to_id: a.id, content: "first")},
                 update: {:pin, Changeset.change(%Pinned{}, pinned_at: now), record: :status}
               )

      assert p.id == s.id
      assert Repo.get!(Pinned, s.id).content == "first"
      assert facts_of(s.id)[:"ap/type"] == [:note]
      seqs = Repo.history(s.id) |> Enum.map(& &1.seq)
      assert Enum.max(seqs) - Enum.min(seqs) == length(seqs) - 1
    end

    test "refs fill the maker, and references with many values" do
      assert {:ok, %{me: me, friend: friend, status: s}} =
               Repo.write_batch(
                 insert: {:me, Changeset.change(%Actor{}, username: "me")},
                 insert: {:friend, Changeset.change(%Actor{}, username: "friend")},
                 insert:
                   {:status, Changeset.change(%Status{}, content: "hi"),
                    refs: [attributed_to_id: :me, mention_ids: [:friend]]}
               )

      assert s.attributed_to_id == me.id
      assert s.mention_ids == [friend.id]
      assert facts_of(s.id)[:"hosi/owner"] == [me.id]
      assert Repo.get!(Status, s.id).mention_ids == [friend.id]
    end

    test "a final value or the record type cannot be changed by the later step either" do
      a = actor!()

      assert_raise Ecto.Hosi.Error, ~r/:ap\/uri is final/, fn ->
        Repo.write_batch(
          insert: {:s, Changeset.change(%Status{}, attributed_to_id: a.id, uri: "u:1")},
          update: {:t, Changeset.change(%Status{}, uri: "u:2"), record: :s}
        )
      end

      assert Repo.aggregate(Status, :count) == 0
    end

    test "a taken unique value of a later step is that step's error" do
      a = actor!()
      status!(a, alt_ids: ["u:taken"])

      assert {:error, :more, %Changeset{} = cs} =
               Repo.write_batch(
                 insert: {:s, Changeset.change(%Status{}, attributed_to_id: a.id)},
                 update:
                   {:more, Status.changeset(%Status{}, %{alt_ids: ["u:taken"]}), record: :s}
               )

      assert cs.errors[:alt_ids]
      assert Repo.aggregate(Status, :count) == 1
    end

    test "two steps cannot change one record that exists; a Multi then goes step by step" do
      a = actor!()
      s = status!(a, tags: ["a"])
      one = Changeset.change(s, tags: ["b"])
      two = Changeset.change(s, content: "second")

      assert_raise ArgumentError, ~r/:one and :two both change record #{s.id}/, fn ->
        Repo.write_batch(update: {:one, one}, update: {:two, two})
      end

      assert {:ok, _} =
               Ecto.Multi.new()
               |> Ecto.Multi.update(:one, one)
               |> Ecto.Multi.update(:two, two)
               |> Repo.transaction()

      assert %{tags: ["b"], content: "second"} = Repo.get!(Status, s.id)
    end

    test "a delete of one parent and of another reach a shared child" do
      a = actor!()
      [b, c] = [actor!("b"), actor!("c")]
      s = status!(a, mention_ids: [b.id, c.id])

      assert {:ok, _} = Repo.write_batch(delete: {:b, b}, delete: {:c, c})
      assert Repo.get!(Status, s.id).mention_ids == []
    end

    test "unknown step options are refused" do
      assert_raise ArgumentError, ~r/an update step takes \[:record\], not \[:refs\]/, fn ->
        Repo.write_batch(update: {:x, Changeset.change(%Pinned{id: 1}), refs: []})
      end
    end
  end

  describe "write_facts" do
    test "writes values of the declared types, and keeps the rules" do
      a = actor!()
      s = status!(a)

      assert {:ok, ids} =
               Repo.write_facts([
                 [:create, s.id, :"ap/alt-id", "u:a"],
                 [:create, s.id, :"ap/alt-id", "u:b"],
                 [:create, [:tmp, 1], :"hosi/owner", a.id],
                 [:create, [:tmp, 1], :"ap/published", ~U[2026-02-03 00:00:00.000000Z]],
                 [:create, [:tmp, 1], :"ap/type", "boost"],
                 [:create, [:tmp, 1], :"ap/attributed-to", a.id]
               ])

      assert Repo.get!(Status, s.id).alt_ids == ["u:a", "u:b"]
      assert Repo.get!(Boost, ids[1]).published_at == ~U[2026-02-03 00:00:00.000000Z]
      assert facts_of(ids[1])[:"hosi/owner"] == [a.id]

      assert {:ok, _} = Repo.write_facts([[:delete, s.id, :"ap/alt-id", "u:a"]])
      assert Repo.get!(Status, s.id).alt_ids == ["u:b"]
    end

    test "refuses what the views refuse" do
      a = actor!()
      s = status!(a)
      other = status!(a, alt_ids: ["u:x"])

      assert {:error, {:unique, :"ap/alt-id"}} =
               Repo.write_facts([[:create, s.id, :"ap/alt-id", "u:x"]])

      assert {:error, {:unique, :"ap/uri"}} =
               Repo.write_facts([[:create, [:tmp, 1], :"ap/uri", other.uri]])

      assert_raise Ecto.Hosi.Error, ~r/final/, fn ->
        Repo.write_facts([[:create, s.id, :"ap/uri", "u:new"]])
      end

      assert_raise Ecto.Hosi.Error, ~r/record is/, fn ->
        Repo.write_facts([[:create, s.id, :"ap/type", "person"]])
      end

      assert_raise ArgumentError, ~r/not one of the values/, fn ->
        Repo.write_facts([[:create, s.id, :visibility, "secret"]])
      end

      assert_raise ArgumentError, ~r/hosi\/owner is the maker/, fn ->
        Repo.write_facts([[:create, s.id, :"hosi/owner", a.id]])
      end

      assert_raise ArgumentError, ~r/adapter's or Datalevin's own/, fn ->
        Repo.write_facts([[:create, s.id, :"hosi/uq-x", "y"]])
      end

      assert_raise ArgumentError, ~r/Did you mean :ap\/uri/, fn ->
        Repo.write_facts([[:create, s.id, :"ap/url", "y"]])
      end

      assert_raise ArgumentError, ~r/is not a value of :ap\/published/, fn ->
        Repo.write_facts([[:create, s.id, :"ap/published", "yesterday"]])
      end

      assert {:error, {:stale, 999_999}} = Repo.write_facts([[:create, 999_999, :content, "x"]])
      assert facts_of(s.id)[:"ap/uri"] == [s.uri]
    end
  end

  describe "reading the hosigraph side" do
    test "facts and history, by field" do
      a = actor!()
      s = status!(a)
      {:ok, _} = s |> Changeset.change(content: "edited", visibility: "followers") |> Repo.update()

      assert Repo.facts(s)[:visibility] == [:followers]
      assert Repo.facts(s.id) == Repo.facts(s)
      assert Repo.facts(999_999) == %{}

      history = Repo.history(s)
      assert Enum.map(history.content, &{&1.kind, &1.value}) == [create: "hello", create: "edited"]
      assert Enum.map(history.visibility, & &1.value) == ["public", "followers"]
      assert [%{value: %DateTime{}}] = history.published_at
      # (an attribute that is not a field of the view is under its own name)
      assert [%{value: owner}] = history[:"hosi/owner"]
      assert owner == a.id
    end

    test "datalog loads structs, and leaves out what the view does not show" do
      a = actor!()
      s = status!(a)
      gone = status!(a)
      Repo.delete!(gone)
      owned = %{q: "[:find ?e :in $ ?a :where [?e :hosi/owner ?a]]", inputs: [[:scalar, a.id]]}

      assert [%Status{id: id, content: "hello", tags: []}] = Repo.datalog(owned, load: Status)
      assert id == s.id

      assert [[%Actor{username: "ada"}, %Status{}, "hello"]] =
               Repo.datalog(
                 %{q: "[:find ?a ?e ?c :where [?e :ap/attributed-to ?a] [?e :content ?c]]", inputs: []},
                 load: [Actor, Status, nil]
               )

      assert_raise ArgumentError, ~r/names 1 column\(s\), and the query finds 3/, fn ->
        Repo.datalog(%{q: "[:find ?a ?e ?c :where [?e :ap/attributed-to ?a] [?e :content ?c]]", inputs: []}, load: Status)
      end
    end

    test "a match asks for the facts of the view" do
      a = actor!()
      s = status!(a)
      Repo.insert!(%Boost{attributed_to_id: a.id})

      assert Repo.datalog(datalog(find: [e], where: [match(Status, id: e, attributed_to_id: ^a.id)])) ==
               [[s.id]]
    end

    test "highest_in and highest answer only what the view shows" do
      a = actor!()
      b = actor!("b")
      t = fn n -> DateTime.add(~U[2026-01-01 00:00:00.000000Z], n, :second) end
      s1 = status!(a, published_at: t.(1))
      s2 = status!(b, published_at: t.(2))
      for n <- 3..70, do: Repo.insert!(%Boost{attributed_to_id: a.id, published_at: t.(n)})
      s3 = status!(a, published_at: t.(0))

      keys = [a.id, b.id]
      assert [x, y] = Repo.highest_in(Status, {:attributed_to_id, :published_at}, keys, 2)
      assert {x.id, x.key, x.at} == {s2.id, b.id, t.(2)}
      assert y.id == s1.id

      assert [%{id: last}] =
               Repo.highest_in(Status, {:attributed_to_id, :published_at}, keys, 2, after: y)

      assert last == s3.id

      {page, place} = Repo.highest(:"ap/published", 3, load: Status)
      assert page == []
      assert place.value == t.(68)
      {all, _} = Repo.highest(:"ap/published", 100, load: Status, after: place)
      assert Enum.map(all, & &1.id) == [s2.id, s1.id, s3.id]

      assert_raise ArgumentError, ~r/declares hosi_tuple/, fn ->
        Repo.highest_in(Boost, {:attributed_to_id, :type}, keys, 1)
      end
    end

    test "explain says the Datalog, and when everything is read" do
      a = actor!()

      assert %{datalog: [%{q: q, inputs: [[:scalar, "hello"]]}], warnings: []} =
               Repo.explain(from s in Status, where: s.content == "hello")

      assert q == "[:find ?e :in $ ?p0 :where [?e :ap/type :note] [?e :content ?p0]]"

      assert %{warnings: [warning]} = Repo.explain(Status)
      assert warning =~ "reads every record Ecto.Hosi.Test.Status shows"

      assert %{datalog: [], warnings: []} = Repo.explain(from s in Status, where: s.id == ^a.id)

      join =
        from s in Status,
          left_join: l in Like,
          on: l.status_id == s.id,
          where: like(s.content, "h%"),
          select: {s.id, l.id}

      assert %{datalog: [_], elixir: elixir, warnings: [warning]} = Repo.explain(join)
      assert Enum.any?(elixir, &(&1 =~ "left join: reads the Ecto.Hosi.Test.Like records by status_id"))
      assert warning =~ "no condition of the query goes into Datalog"
      assert Repo.all(Status) == []
    end
  end

  describe "follows" do
    test "one pair once" do
      [a, b] = [actor!(), actor!("b")]
      {:ok, _} = %Following{} |> Following.changeset(%{from_id: a.id, to_id: b.id}) |> Repo.insert()

      assert {:error, cs} =
               %Following{} |> Following.changeset(%{from_id: a.id, to_id: b.id}) |> Repo.insert()

      assert cs.errors[:from_id]
    end
  end
end
