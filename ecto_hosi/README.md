# ecto_hosi

An [Ecto](https://hex.pm/packages/ecto) adapter for [hosigraph](https://github.com/sukhi-social/hosigraph),
a database where an append-only event log is the truth and "now" is a Datalevin (Datalog) copy folded from it.

You keep `Repo`, schemas, changesets and `preload`, and you get **facts instead of tables**,
**Datalog with recursive rules**, the **history** of any record, and **real erasure**.
日本語: [README.ja.md](README.ja.md)

> Not on Hex. Tested on Linux aarch64 only. This version is not released yet, and it does not read
> data that 0.2.0 wrote (see the [changelog](../CHANGELOG.md)).

## Install

```elixir
# mix.exs
{:ecto_hosi, github: "sukhi-social/hosigraph", sparse: "ecto_hosi"}
```

You also need:

* **libhosi**, hosigraph's compiled core. Prebuilt downloads exist only for releases that publish them;
  from a checkout, build it with GHC 9.10.3 + cabal (`cabal build hosigraph-abi:flib:hosi`) and compile
  your app with `HOSI_BUILD=source HOSI_LIB_DIR=<dir of libhosi.so> HOSI_INCLUDE_DIR=<hosigraph-abi/gen/include>`
  ([hosigraph-ex](../hosigraph-ex/README.md#getting-libhosi); the `HOSI_LIB_DIR` route was not tried here).
* **Datalevin** 1.2.10: JDK 21+ with the standalone jar, or the native `dtlv`.

## A taste

```elixir
defmodule MyApp.Repo do
  use Ecto.Repo, otp_app: :my_app, adapter: Ecto.Adapters.Hosi
end

config :my_app, MyApp.Repo,
  dir: "/var/lib/my_app/hosi",
  schemas: [MyApp.Note, MyApp.Pinned],
  khana: [jvm: "/opt/datalevin/datalevin-1.2.10-standalone.jar"]

# a schema is a view of facts
defmodule MyApp.Note do
  use Ecto.Schema
  use Ecto.Hosi.View

  hosi_where type: "note"          # a record is a note when `type` is :note
  hosi_owner :author_id            # its maker: erased with that account
  hosi_keyword :type, ["note"]
  hosi_record_type :type

  schema "note" do                 # Ecto needs a name; nothing is put in front of the fields
    field :type, :string, default: "note"
    field :author_id, :id
    field :content, :string        # the attribute `content`
  end
end

defmodule MyApp.Pinned do
  use Ecto.Schema
  use Ecto.Hosi.View

  hosi_where :pinned_at            # whatever has this fact
  hosi_owner :nobody

  schema "pinned" do
    field :pinned_at, :utc_datetime_usec
    field :content, :string        # the same attribute as the note's
  end
end

note = MyApp.Repo.insert!(%MyApp.Note{author_id: me, content: "hello"})
#=> facts: [id :type :note] [id :author_id me] [id :content "hello"] [id :hosi/owner me]

MyApp.Repo.update!(Ecto.Changeset.change(%MyApp.Pinned{id: note.id}, pinned_at: DateTime.utc_now()))
MyApp.Repo.get!(MyApp.Pinned, note.id).content   #=> "hello"
MyApp.Repo.explain(from n in MyApp.Note, where: n.content == "hello")
```

Run `mix hosi.doctor` to check that the pieces are there, and read the **[guide](docs/guide.md)**:
facts and views, from Ecto to Datalog, create / update / delete / erase, transactions, and the limits.

## What is there

* **Ecto:** `insert` `update` (+`optimistic_lock`) `delete` `insert_all` (+`on_conflict`) `update_all`
  `delete_all` `get` `get_by` `all` `one` `exists?` `aggregate` `preload`, joins, `group_by`,
  aggregates, `unique_constraint`, `foreign_key_constraint`, `Repo.transaction` and `Ecto.Multi`.
* **Views and rules:** shared vocabularies, many values, references to any record, keywords, UUIDs,
  unique / final / record-type / file rules, `on_delete`, tombstones.
* **Added to the Repo:** `explain` · `datalog` (rules, `load:`) · `highest` · `highest_in` · `facts` ·
  `history` · `write_batch` · `write_facts` · `erase` · `erase_account` · `clean`
* **Tasks:** `mix hosi.doctor`, `mix hosi.attributes`

## Different from SQL

* A record shows up in a view because of its facts; an update through any view changes facts.
* `delete` says what it removes, and refuses to remove facts the view does not show.
* `not` means "there is no such fact".
* Transactions are not SQL transactions: one batch is atomic; the rest is undone on rollback.
* No subqueries, windows, `union`, `lock`, `with_cte`, `stream`, queries by name, or migrations.

## Config keys

`:dir`, `:schemas` (the views), `:khana` (`[jvm: jar]`, `[jvm: jar, java: path]` or `[dtlv: path]`),
`:open` (extra open options of hosigraph), `:timeout` (how long a write waits, default 15000),
`:precheck_unique` (default `true`), `:join` (a second Repo over the same database), `:hosi` (the module
that talks to hosigraph, default `Hosi`).

## Testing your app

`config :my_app, MyApp.Repo, hosi: Ecto.Hosi.Fake` runs the Repo in memory with no libhosi or Datalevin.
It refuses what hosigraph refuses of the usual calls, but it is a stand-in: test important paths against
the real engine too.

## Development

```sh
HOSI_BUILD=none mix test      # on the stand-in

# on the real engine (HOSI_DTLV=<dtlv> instead of HOSI_JAR for the native khana)
HOSI_REAL=1 HOSI_BUILD=source HOSI_JAR=<datalevin.jar> mix test
```

After switching between the two, `mix deps.compile hosi --force`.
Run on the real engine with GHC 9.10.3, Datalevin 1.2.10 as a JVM, and Ecto 3.14.3. Not run: macOS,
the native `dtlv`, Ecto 3.12/3.13, large data.
