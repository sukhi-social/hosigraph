# ecto_hosi: a guide

Read it in order. Each part needs only the ones before it.

0. [Set up](#0-set-up)
1. [Facts and views](#1-facts-and-views)
2. [The ladder: Ecto, the Datalog behind it, Datalog](#2-the-ladder-ecto-the-datalog-behind-it-datalog)
3. [Create, update, delete, erase](#3-create-update-delete-erase)
4. [Transactions are not SQL transactions](#4-transactions-are-not-sql-transactions)
5. [The limits](#5-the-limits)

## 0. Set up

You need two things besides the packages:

* **libhosi**, hosigraph's compiled core. Prebuilt downloads exist only for releases that publish
  them; from a checkout, build it with GHC 9.10.3: `cabal build hosigraph-abi:flib:hosi`
  (see [hosigraph-ex](../../hosigraph-ex/README.md#getting-libhosi)).
* **Datalevin** 1.2.10: JDK 21 or newer with the standalone jar, or the native `dtlv`.

```elixir
defmodule MyApp.Repo do
  use Ecto.Repo, otp_app: :my_app, adapter: Ecto.Adapters.Hosi
end
```

```elixir
config :my_app, MyApp.Repo,
  dir: "/var/lib/my_app/hosi",
  schemas: [MyApp.Account, MyApp.Note, MyApp.Pinned, MyApp.Tombstone, MyApp.Follow],
  khana: [jvm: "/opt/datalevin/datalevin-1.2.10-standalone.jar"]
  # khana: [jvm: "/path/datalevin.jar", java: "/usr/lib/jvm/jdk-21/bin/java"]
  # khana: [dtlv: "/opt/datalevin/dtlv"]
```

Then ask whether the pieces are there, and add the Repo to your supervision tree:

```sh
mix hosi.doctor
```

(*khana* is hosigraph's word for "now": the Datalevin copy of the current facts.)

## 1. Facts and views

hosigraph keeps **facts**: `[record attribute value]`.

```text
[17  :ap/type   :note]
[17  :ap/uri    "https://example.com/notes/1"]
[17  :content   "hello"]
[17  :pinned_at 1767225600000000]
```

A **record** is an integer id and the facts it has. There are no tables.

An Ecto schema is a **view** of facts. It says which facts make a record show up, and which
attributes its fields read and write. One record can show up in several views: record 17 above is a
note, and it is also something pinned.

### Shared attributes

Attributes that several views use are declared once, in a vocabulary:

```elixir
defmodule MyApp.ActivityPub do
  use Ecto.Hosi.Vocabulary, prefix: "ap"

  attribute :type, :keyword, values: ["person", "note", "follow", "tombstone"], record_type: true
  attribute :former_type, :keyword, values: ["person", "note"]
  attribute :uuid, Ecto.UUID, unique: true, final: true
  attribute :uri, :string, unique: true, final: true
  attribute :alt_id, :string, unique: true, many: true
  attribute :attributed_to, :ref, on_delete: :keep, final: true
  attribute :published, :utc_datetime_usec
  attribute :deleted, :utc_datetime_usec
end
```

`:former_type` is the attribute `ap/former-type`. Only the vocabulary may name attributes under
`ap/`.

### Views

```elixir
defmodule MyApp.Account do
  use Ecto.Schema
  use Ecto.Hosi.View

  hosi_where type: "person"          # a record is an account when ap/type is :person
  hosi_owner :nobody
  hosi_unique :username
  hosi_tombstone keep: [:uri, :username],
                 set: [type: "tombstone", former_type: "person"],
                 mark: :deleted_at

  schema "account" do                # Ecto wants a name; it is used in constraint names only
    hosi_attributes MyApp.ActivityPub,
      type: :type, former_type: :former_type, uri: :uri, deleted_at: :deleted

    field :username, :string         # the attribute `username`
  end
end

defmodule MyApp.Note do
  use Ecto.Schema
  use Ecto.Hosi.View
  import Ecto.Changeset

  hosi_where type: "note"
  hosi_owner :author_id              # the maker: erased with that account
  hosi_tuple :author_id, :published_at
  hosi_tombstone keep: [:uuid, :uri, :author_id],
                 set: [type: "tombstone", former_type: "note"],
                 mark: :deleted_at

  hosi_keyword :visibility, ["public", "followers"]
  hosi_ref :reply_to_id, on_delete: :keep
  hosi_many :tags

  schema "note" do
    hosi_attributes MyApp.ActivityPub,
      type: :type, former_type: :former_type, uuid: :uuid, uri: :uri,
      author_id: :attributed_to, published_at: :published, deleted_at: :deleted

    field :content, :string
    field :visibility, :string, default: "public"
    field :reply_to_id, :id
    field :tags, {:array, :string}
  end

  def changeset(note, attrs) do
    note
    |> cast(attrs, [:uuid, :uri, :content, :visibility, :reply_to_id, :tags, :published_at])
    |> unique_constraint(:uri, name: "note_ap/uri_index")
  end
end

# whatever is pinned, notes or not
defmodule MyApp.Pinned do
  use Ecto.Schema
  use Ecto.Hosi.View

  hosi_where :pinned_at              # a record shows up when it has this fact
  hosi_owner :nobody

  schema "pinned" do
    field :pinned_at, :utc_datetime_usec
    field :content, :string
  end
end

defmodule MyApp.Tombstone do
  use Ecto.Schema
  use Ecto.Hosi.View

  hosi_where type: "tombstone"
  hosi_owner :nobody

  schema "tombstone" do
    hosi_attributes MyApp.ActivityPub,
      type: :type, former_type: :former_type, uri: :uri, deleted_at: :deleted
  end
end

defmodule MyApp.Follow do
  use Ecto.Schema
  use Ecto.Hosi.View

  hosi_where type: "follow"
  hosi_owner :from_id
  hosi_ref :from_id, on_delete: :cascade
  hosi_ref :to_id, on_delete: :cascade
  hosi_unique [:from_id, :to_id], tuple: true

  schema "follow" do
    hosi_attributes MyApp.ActivityPub, type: :type
    field :from_id, :id, source: :"follow/from"
    field :to_id, :id, source: :"follow/to"
  end
end
```

Every view says two things, or the Repo does not start:

* **`hosi_where`**: which facts make a record show up. `type: "note"` asks for a value;
  `:pinned_at` only that the fact is there. A record made through the view gets those facts.
* **`hosi_owner`**: who made the records. It writes the fact `hosi/owner`, which
  `Repo.erase_account` erases by. `:nobody` says that no one does.

**A field is an attribute.** `field :content, :string` is the attribute `content`. Nothing is put
in front of it. Use `source:` for another name (`source: :"s3/path"`), or a vocabulary. Two views
that name one attribute share it, so give two meanings two names. `:id` is hosigraph's own number
for the record; an id that you give to clients is an attribute of its own (`ap/uri` here).

### Field types

| Ecto type | Kept as |
|---|---|
| `:string`, `:integer`, `:boolean`, `:float` | string, long, boolean, double |
| `:utc_datetime*`, `:naive_datetime*`, `:date` | long (microseconds; days) |
| `:decimal`, `:time` | string |
| `Ecto.UUID` | uuid |
| `belongs_to` key, `hosi_ref`, a vocabulary `:ref` | reference to any record |
| a string with `hosi_keyword` | keyword (a closed list) |
| `{:array, t}` with `hosi_many` | many values: a set, so order and duplicates are not kept |
| other `{:array, t}`, `:map`, `:binary` | one string (JSON, JSON, Base64): matched whole, not searched inside |

A `nil` field is no fact.

### Rules belong to attributes

A rule holds for every record that has the attribute, whichever view wrote it. Declare it once.

| Rule | Meaning |
|---|---|
| `hosi_unique :f` | no two records hold the same value |
| `hosi_unique [:a, :b], tuple: true` | the combination, kept by hosigraph; a record with one must have both |
| `hosi_unique [:a, :b]`, `lower:`, `where:` | kept by the adapter: a direct hosigraph write passes it by |
| `hosi_final :f` | once set, not changed or removed (erase still removes it) |
| `hosi_record_type :f` | says what a record is: an update cannot change it |
| `hosi_ref :f, on_delete:` | what a delete of the target does: `:cascade`, `:nilify`, `:restrict`, `:keep` |
| `hosi_file :f` | names a file: such a record is not removed outright |

`mix hosi.attributes` prints every attribute, its type and rules, and the views that name it.

## 2. The ladder: Ecto, the Datalog behind it, Datalog

### Step 1: write Ecto

```elixir
import Ecto.Query

MyApp.Repo.all(from n in MyApp.Note, where: n.visibility == "public" and "elixir" in n.tags)
MyApp.Repo.get_by(MyApp.Note, uri: "https://example.com/notes/1")
```

`where`, `join`, `group_by`, aggregates, `order_by`, `limit`, `preload`, `update_all`,
`delete_all`, `insert_all` and `on_conflict` work as far as the facts can answer them honestly;
what cannot be done raises and says so. Two things differ from SQL:

* `not` means "there is no such fact": `where: not (n.content == "x")` also finds records with no
  content.
* A query always asks for the view's facts first: `from n in MyApp.Note` is "records whose
  `ap/type` is `:note`".

### Step 2: see what it became

```elixir
MyApp.Repo.explain(from n in MyApp.Note, where: n.content == "hi")
#=> %{datalog: [%{q: "[:find ?e :in $ ?p0 :where [?e :ap/type :note] [?e :content ?p0]]",
#                 inputs: [[:scalar, "hi"]]}],
#     elixir: ["reads the matching records to build the result"],
#     warnings: []}
```

`warnings` says when every record of a view is read.

### Step 3: write Datalog

With `match`, which knows the views:

```elixir
defmodule MyApp.Rules do
  use Ecto.Hosi.Rules

  # a reaches b if a follows b, or follows someone who reaches b
  rule :reaches, [a, b] do
    match MyApp.Follow, from_id: a, to_id: b
  end

  rule :reaches, [a, c] do
    match MyApp.Follow, from_id: a, to_id: b
    reaches(b, c)
  end
end

import Ecto.Hosi.Query

MyApp.Repo.datalog(datalog(find: [b], where: [reaches(^me, b)], rules: MyApp.Rules))
#=> [[12], [47]]          a set: in no particular order
```

Or as hosigraph takes it. `load:` answers structs, and leaves out what a view does not show:

```elixir
MyApp.Repo.datalog(
  %{q: "[:find ?e :in $ ?a :where [?e :ap/attributed-to ?a]]", inputs: [[:scalar, me]]},
  load: MyApp.Note
)
#=> [%MyApp.Note{}, ...]
```

In raw Datalog you write what is stored: a keyword is `:public`, not `"public"`; a UUID is
`#uuid "…"` in the text or `{:uuid, text}` as an input; a time is microseconds. A misspelt
attribute, or a string where a keyword or a UUID is kept, raises (`check: false` skips that).

### Newest first

`order_by` and `limit` are applied after every matching record is read. These two walk an index
instead:

```elixir
# the newest values of one attribute: {records, last}; the next page: after: last
MyApp.Repo.highest(:"ap/published", 20, load: MyApp.Note)

# the newest notes of some authors (the view's hosi_tuple)
MyApp.Repo.highest_in(MyApp.Note, {:author_id, :published_at}, followed_ids, 20)
#=> [%{id: 31, key: 12, at: ~U[...]}, ...]     the next page: after: the last place
```

With `load:`, `highest` may answer fewer than asked: the index holds every record with the
attribute, and those the view does not show are left out. Go on from `last`.

### One record

```elixir
MyApp.Repo.facts(note)    #=> %{"ap/type": [:note], content: ["hello"], ...}   as kept
MyApp.Repo.history(note)  #=> %{content: [%{seq: 4, kind: :create, value: "hello"}, ...], ...}
MyApp.Repo.history(17)    #=> every event, as hosigraph gives them
```

History keeps order (`seq`), not time. A new value of a single-valued attribute is a `:create`;
there is no `:delete` of the value it replaces.

## 3. Create, update, delete, erase

| Action | Id | In history |
|---|---|---|
| create | new | new facts |
| update | the same | facts added and removed |
| delete | the same (a tombstone), or gone | facts added and removed |
| erase | gone | gone |

### Create

`insert` and `insert_all` write the view's `hosi_where` facts, the owner, and the fields that have
a value.

Several records in one atomic write, pointing at each other, with facts from several views:

```elixir
MyApp.Repo.write_batch(
  insert: {:note, MyApp.Note.changeset(%MyApp.Note{author_id: me}, %{content: "a"})},
  insert: {:reply, MyApp.Note.changeset(%MyApp.Note{author_id: me}, %{content: "b"}),
           refs: [reply_to_id: :note]},
  update: {:pin, Ecto.Changeset.change(%MyApp.Pinned{}, pinned_at: DateTime.utc_now()),
           record: :note}
)
#=> {:ok, %{note: %MyApp.Note{}, reply: %MyApp.Note{}, pin: %MyApp.Pinned{}}}
#   or {:error, step_name, changeset | :stale | ...}
```

* `refs:` fills a reference with the id of a record made earlier in the batch.
* `record:` gives a new record more facts, through any view. Only the changeset's changes are
  written, so the facts that make it show up in that view have to be among them.
* Each step on a record that exists is worked out from what it holds **before** the batch, so two
  steps cannot change one record.

`write_facts` writes facts without a view, and still keeps the rules of the attributes:

```elixir
MyApp.Repo.write_facts([[:create, note.id, :"ap/alt-id", "https://old.example/n/1"]])
#=> {:ok, %{}}  |  {:error, {:unique, :"ap/alt-id"}}
```

### Update

An update through any view adds, changes or removes facts. A `nil` removes one.

```elixir
# pin a note: it now shows up in Pinned too
MyApp.Repo.update(Ecto.Changeset.change(%MyApp.Pinned{id: note.id}, pinned_at: DateTime.utc_now()))
```

A record leaves a view when its facts change; there is nothing to attach or detach. Refused:

* an id that holds nothing (`Ecto.StaleEntryError`);
* through a view of a record type, a record of another type (stale);
* a change of the record type, or of a `final` attribute.

### Delete

A delete says what it removes. Through view V, record R may be deleted if

* R has a record type and V is a view of that type; or
* R has no record type, and no facts that V does not show.

Otherwise it is refused, so that nobody removes facts they did not see.

| You want | Call | What happens |
|---|---|---|
| unpin | update through Pinned: `pinned_at: nil` | the note stays |
| delete the note | `Repo.delete(note)` | the tombstone: `ap/type` becomes `:tombstone`, content and pin are gone, `keep:` stays |
| (a mistake) | `Repo.delete(pinned)` for a note | refused; the error names the two calls above |
| delete a record that is only pinned | `Repo.delete(pinned)` | removed |

A record type with a `hosi_tombstone` leaves the tombstone; without one, the facts are removed.
`final` and `hosi_file` attributes and `hosi/owner` stay in a tombstone anyway. References follow
their `on_delete`, in the same atomic write.

**The old content stays in history and on disk until it is erased.** If it has to go sooner, erase
the record and run `clean`.

### Erase

Erase removes records for real, history included. It cannot be undone.

```elixir
MyApp.Repo.erase([[:ids, [id]]])   # these records
MyApp.Repo.clean()                 # let the bytes leave the disk; again while :"records-left" > 0
```

For an account:

```elixir
MyApp.Repo.erase_account(account, delete_files: fn paths -> MyApp.Files.delete_all(paths) end)
#=> {:ok, %{erased: [...], left: []}}
```

It erases every record whose `hosi/owner` is the account, first handing the file names to
`delete_files:`, follows other records' references to them (`on_delete`), and keeps the account as
its tombstone. It can be run again after an interruption.

For that to be safe, write your data this way:

1. Every record an account makes has the account as its maker (`hosi_owner`): a note its author, a
   like the liker, a follow the follower.
2. The account's own record holds identity only (type, URI, name). Profile and keys are records it
   owns: erase removes whole records.
3. Records with files declare `hosi_file` and a tombstone.

What others wrote stays (a reply points at nothing). Erase does not reach backups or other servers.

## 4. Transactions are not SQL transactions

hosigraph writes **one batch** atomically. It has no begin and commit around many steps.

* `Repo.write_batch/2` is one batch: all or nothing, also if the node stops.
* An `Ecto.Multi` of known changesets (no function, no `Multi.run`, no options) is turned into one
  batch.
* Any other `Repo.transaction` writes at once and **undoes on rollback**. Others see the writes
  before the commit; a crash between them loses the rollback; the undone writes stay in history;
  `erase` is not undone.

Writes are guarded by conditions on what was read, so a write that lost a race is `stale` rather
than silently applied.

A write waits `timeout:`, or the Repo's `:timeout` (15 s). When it runs out, the result is
**unknown**: hosigraph goes on with the write. Look before you try again.

## 5. The limits

**hosigraph's**

* One process opens a database.
* History keeps order, not time.
* History has no "update", and does not name the action. Write a fact such as `ap/updated` if you
  need it.
* Datalog asks only "now". The past is read one record at a time.
* There is no time type: times are numbers.
* A tuple's `nil` is a value, not SQL's NULL.
* Datalog's `not` is about a variable that the rest of the query binds.
* A rule with a `not` that is only about its own arguments cannot be given a value there
  (`unfollowed(^me)` fails with "Join variables should not be empty"). Ask with a variable and
  compare: `unfollowed(a), a == ^me`. (Seen with Datalevin 1.2.10.)
* Datalevin cannot compile a query of a few thousand clauses. (A list handed in as an input is no
  clause, and may be long.)

**The adapter's**

* No subqueries, windows, `union`, `lock`, `with_cte` or `stream`; use Datalog.
* No reading or writing by name (`from "name"`): a name says nothing about the facts.
* No migrations: hosigraph's schema is made from your views when the Repo starts. Changing the type
  of an attribute that has data is yours to move.
* A read waits 5 s, also the reads a write makes first (a very large `update_all`).
* Rolling back a large step-by-step transaction is slow (it grows with the square of its size).
* `on_conflict` looks the record up and then writes: put the target on a unique attribute, and
  hosigraph guards the race.
* Rules the adapter keeps (combinations without `tuple: true`, `final`, record types, delete
  rules, tombstones) are not kept by a direct hosigraph write. Use `write_facts` for those.

Each error says whose limit it is.

## Testing your app

`hosi: Ecto.Hosi.Fake` in the Repo's configuration runs it in memory, with no libhosi and no
Datalevin. It is a stand-in: test the important paths against the real engine too.
