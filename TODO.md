# TODO

What is left to do. What hosigraph does now is in [the guide](docs/guide.md); how it came to be is in git.

Everything here is for 0.3.0. Which part is *in* 0.3.0, and which only waits, is not decided; the S3 part alone is twelve steps.

## Compare cases in PostgreSQL and in hosigraph, and write down how to design the query and the data

**Why.** The queries of 0.2 came from one question, "how would sukhi's timeline be done here?", and each answer was found by measuring (a `:limit` that does not make a query cheaper; a walk that is 0.7 ms or 900 ms depending on who is followed; a tuple that is not unique).
A reader who knows SQL has no map from "I would make this table, this index, this join" to "I would write this schema, this tuple, this call", and nobody has written down where hosigraph is the *worse* choice.

**What it would be.** A set of cases. For each, on both sides:

1. the data: tables and indexes in PostgreSQL; attributes, tuples and identity in hosigraph;
2. the question: the SQL; the query or the call;
3. what it costs, on the same made-up data (sizes, the machine, the versions and the settings written down; medians, with the input changed each time, since both sides keep answers);
4. what it cannot do, or what to watch (the cost of a write, memory, ties and paging, how it grows);
5. **the rule it shows**, in one sentence.

**Cases to start from** (all met already; the list may change):

* The timeline of the people one follows, the newest 20, a page after a page (PostgreSQL: an index on `(author, created_at desc, id desc)` and `author = any(...)`; hosigraph: `highestIn` on a tuple, against `:order-by :limit`).
* "Does A follow B?" (a unique index; an `identity` tuple), and following without looking first.
* A search box, in English and in Japanese (`tsvector` and GIN, `pg_trgm`; `fulltext`, the JVM khana, or any khana with a tokenizer).
* The counts on a profile (`count(*)`, or a counter kept by hand; what hosigraph has instead is not known yet: that is part of the finding).
* Erasing an account (`delete`, vacuum, and the backups; `erase` and `clean`, and what is left, and where).
* The history of one entity (an audit table, or a temporal table; `history`).
* **A report that PostgreSQL does better** (group by across all rows, a window function, a join over big tables). At least one case of this kind has to be in, with the number, or the document is an advertisement.
* A write that must be on the disk before it is answered (`commit` and `write` with `fsync`), one at a time and many at once.

**Fairness.** PostgreSQL is set up the way someone who knows it would (the indexes it wants, `analyze`, a note on `shared_buffers`), not as hosigraph's way copied into SQL. Warm and cold are both given. The JVM's start is given, not hidden. The data is made up, not a copy of a real server.

**Open (for nyanrus):** where it lives (`docs/`, a new file, beside `design-notes.md`?); the PostgreSQL version; the scripts go in `scripts/` like `bench.hs` and not in the cabal package (a guess); whether someone who knows PostgreSQL better reads the SQL side; whether the cases come from sukhi's real queries (by name only, not its data).

**Done when:** each case has both sides with numbers and its rule; a case where hosigraph is the worse choice is in; and the document says what was not measured.

## Open for nyanrus

* **How strongly to say "embedded".** The khana needs a program outside the process. The README and the guide say it plainly: embedded, and it brings Datalevin along **(a)**.
  The other way **(b)** is to wait for a khana inside the process; then the release moves far away, and nothing in the repository starts that work.
  If the first users are sukhi and tacchaka-matrix, (a) probably promises enough; that is a guess, not something asked.
* **The release.** Nothing is tagged, uploaded or pushed as a tag. The versions now: `hosigraph` 0.2.0.0; `takka` 0.1.0.1 (it has a constructor more, `VTuple`; the PVP would say 0.2, and `setu` keeps the bound `takka <0.2`, which would then change);
  `hosigraph-abi` and `setu` 0.0.1.0; `hosigraph-ex` 0.0.1. Whether they move with the library is open.
* **The C door.** `libhosi` was seen working on macOS only, and has its own version. Does the library's promise cover it, or not yet?
* **Which attributes mean "owned by" in sukhi.** They are an argument of each `EraseOwnedBy`, so hosigraph needs no setting; a separate S3 object for each owner, and a dictionary for the cold data, are proposed in the S3 plan and not built.

## Would help

* **A shared lock for readers.** A world has one opener: a second process that only wants to read (a backup, an inspector, a second reader of the same world) gets `WorldBusy`. A `LOCK_SH` on the same `events/lock` is possible with the same `flock`: readers share it, the writer takes it exclusively.
  Open: a reader would see a world whose owner may seal or erase underneath it (a chunk directory swapped, a file gone), so it needs a read that survives that, or a promise that its answers are for one manifest. Waits for a user who needs it.
* **A backup call.** The khana is derived, so `events/` alone is a backup. Open: may it be copied while writes go on (seal first, under the lock?), or only when the `Hosi` is closed?
* **Many threads.** Not tested: `clean` from two threads at once, many readers while `seal` or `erase` runs, and "the world changed between the read and the lock" in a rewrite.
* **A `verify` that walks both khanas in one `dtlv` process.** It asks for a page at a time, and each page is a `dtlv` process (about 110 of them on real data; the 65 s is 34 s of rebuild and about 30 s of this walk, not measured call by call). The JVM keeps one process and takes 22 s. `verify` is rare work.
* **A time limit for a call to the resident khana.** `KhanaDtlvRepl` (and the JVM) wait for as long as the process is alive and silent: a form that never ends, or a process that hangs, holds the call (and the lock) for good. A limit would end the process and fail that one call, as a death does.
* **A `dtlv repl` left behind.** When a process that used `KhanaDtlvRepl` ends without `closeHosi` (it is killed, say), the repl is not ended by the closed pipe while a database is open in it: it stays, with its memory (about 50 to 110 MB) and the LMDB files open. Not tried: a form that closes every connection when the pipe closes, or a repl that watches its parent.
* **`KhanaDtlvRepl` against a power cut, and with the JVM side by side.** Only `kill -9` was tried (200 acknowledged writes, all kept). The measurement of the repl was not taken beside the JVM's (no JDK 21 at hand), the tests that write with the JVM and read with the repl do not exist, and CI does not run the native `dtlv` at all.
* **Linux, for real.** CI runs on ubuntu with the JVM khana only; the native `dtlv` is not run there, and the `abi` job has been seen to pass on macOS only. Windows has no `System.Posix`.
* **The khana backend that fails, as a kind of error.** It is a `DtlvError` (a bare string, not a `HosiError`), thrown in about a dozen places, and some of them are the caller's fault: an unreadable query, a value that has no EDN form. The C door tells it `HOSI_ERR_INTERNAL`.
  To make it a kind (say `KhanaFailed`) the caller's own mistakes there must be told apart first.
* **A tail mark.** `tail.log` has no header; its first four bytes, `HOSI`, are reserved for a later format. A mark that tells a tail from a file that is not one can be added later, beginning with those four bytes.
* **Measured above this size.** Nothing was measured above 530,000 events, on a disk that waits for `fsync` (on macOS it does not), or on Linux.

## What the questions of 0.2 leave

* **Making `highest` and `highestIn` stable.** They are under "Experimental" in 0.2, because `highestIn` may grow (an order part that is not a long, the other direction) and its call may change with it. They move up when those are settled.
* **`highestIn`:**
  * **the floor of 2 to 6 ms for every call** (through hosigraph, JVM khana: 400 keys with 400 notes each take 3.9 ms; the same walk written as compiled Clojure took 0.27 ms in a spike). Probably the script, which the khana reads and runs afresh each time, and the key list sent as text. Open: define the walk once in the khana and call it; look at where the time goes first;
  * what a note's *write* costs with the tuple, against without it (the tuple is one more datom and one more index entry for each note);
  * more than 5,000 keys at once (the cost is one seek for each key; a very long list is a long call), and the call through the C door and from Elixir (only the shape was tested there, on a small world);
  * an order part that is not a long (a double, a string), and the other way, smallest first (`lowestIn`);
  * **a filter that is not "the key is one of these"** (muted, hidden, followers only): the way is "ask for more than you need, drop what fails, and go on from the last fact looked at". How that does when the filter drops most of what it sees is not measured.
* **`highest`:** the walk upwards (`lowest`: `seek-datoms` on `:ave`, a few lines when it is wanted); a long string as the walked attribute was not tried.
* **Fulltext without the JVM.** The analyzer is an `inter-fn`, which the native `dtlv` cannot run (`Can only freeze an inter-fn`), so without a tokenizer the native khana refuses the schema. Also not tried: a phrase search, other analyzers, an attribute name that is not `ns/name`.
* **The tokenizer (`hcTokenizer`), what is left:**
  * **A faster AND that gives every answer.** A search of several words is the best `:top` of Datalevin's OR, checked for every word (the guide): cheap, and it can give fewer than the `:top`, or none, when the notes that have every word are not among the best ones; a `:top` as big as the notes gives them all and scores every note that has any one word (1.2 to 2.7 s on 50,000 notes). Two ways, neither tried: ask again with a wider `:top` until there are enough or no more; or keep one fact for each word of a text (a many-valued, indexed attribute beside `<attribute>.words`), so that "all of these words" is an index lookup on the rarest word, with no scoring, and the OR with its order stays for `:top`. Another way, for a tokenizer that can: give A and C words in one index, and cut the search as C.
  * **The JVM khana with a tokenizer** is written, with its tests, and was never run (no JDK here). Whether its default analyzer takes the `<attribute>.words` attributes as the native one does is not known.
  * **The rough edges of the words attribute:** it shows in `entity`, `datoms` and the counts, `?a`/`?v` of a `fulltext` call are the words, and the default analyzer of Datalevin still drops English stop words from them.
  * **An option of the C door** (`:tokenizer`): a `Tokenizer` is Haskell, so what the door could take is a way to name one the library knows.
  * **A cost not measured:** a write that has a long text (the tokenizer is asked once for each batch); a world with 100,000 posts made again (the words are made once for each text, in batches).
  * What is left of the Sudachi one (more of the real-post check, the program's version in the mark) is in the README of `example/hosi-mikke`.
* **Tuples.** Not checked at open: a tuple of an attribute that is not in the schema, or of itself (Datalevin refuses `many` itself, when the schema is made). A pull of a tuple attribute was not tried.

## What sukhi's use of `ecto_hosi` asks of hosigraph

Written 2026-11-26, from putting `hosi_keyword` (a string field kept as a keyword) and `hosi_unique … tuple: true` (a combination kept as a tuple) into `ecto_hosi`, and those into sukhi's `Boost`, `Reaction` and `Note.visibility`.
Both ride on what hosigraph has today, so **nothing here blocks them**. Each item says what was seen and what was only read or guessed. "On the jar" means the Datalevin 1.2.10 standalone jar run by itself, not through hosigraph.

* **A keyword attribute takes a bare string.** Seen through `hosigraph-ex` and the JVM khana: a `create` of `"closed"` (a string, no colon) on an attribute whose type is `keyword` was accepted and read back as `:closed`.
  The guide says only that a string that starts with `:` is a keyword (the C door's JSON). Open: is it meant? Then say so in the guide and test it; if not, refuse a string where the schema says keyword.
  `ecto_hosi` does not lean on it: it makes the keyword itself and checks the value against a list of its own.
* **A refusal of a unique value, as a kind.** `WriteRejected` carries Datalevin's text (`Cannot add #datalevin/Datom [e :link/uq-… [1 2]] because of unique constraint: <holder>`), and `ecto_hosi` finds which attribute it was by looking for the attribute's name in it.
  That works (seen on 1.2.10 with the JVM khana; the message of a native `dtlv` was not looked at), but each host would have to do the same. Open: a constructor, or fields on `WriteRejected`, for "this attribute, held by this entity", read out in one place.
  Related to "The khana backend that fails, as a kind of error", above.
* **`unique` that is not `identity`, on a tuple.** The guide and the tests use `identity (tuple …)`. `ecto_hosi` writes `unique: :value` on a tuple through the C door, and it was seen to work on 1.2.10 (long and long, ref and ref, ref and ref and string):
  a second equal pair is refused, and `[?e :attr [a b]]` finds the pair. hosigraph's own tests have no `uniqueValue (tuple …)`. Add one, and say in the guide whether the two differ for a pair that must be "held once" (not looked at).
* **A tuple that is not made while a part is missing (SQL's NULL).** A tuple holds `[a nil]`, and two entities with the same `a` and no `b` are the same pair (the guide says so). So `ecto_hosi` refuses a write that leaves a field of a `tuple: true` rule nil, and keeps the rules that need SQL's NULL as a JSON text in an attribute of their own, as before
  (in sukhi: `custom_emojis (shortcode, domain)`, where a local emoji has no domain, and `notifications (account, from_account, type, note)`, where the note is optional).
  Not looked at: whether Datalevin can leave a tuple out while a part is nil. If it cannot, there is nothing to do here.
* **Types the khana cannot hold: instant, uuid, bytes, bigdec, idoc.** On the jar all of them write and read, and an instant given as `#inst` text is read. None of them is reachable through hosigraph: its `Value` has five constructors (the value tags are 0 to 4), its `ValueType` seven, and `Takka.Edn` reads no tagged literal (`#inst`, `#uuid`). What each would cost, as far as it was looked at:
  * **a new value tag** in `putValue` and `getValueBody`, which the tail log and the chunks share, for instant, uuid, bytes and bigdec. A file with the new tag cannot be read by an older build (0.2 promises the other way: that what older builds wrote is read), and `Takka.Edn` has to learn the tagged literals;
  * **an instant is milliseconds** (`java.util.Date`; `.123` was kept), where sukhi keeps microseconds in a long (the `_usec` fields of at least ten schemas), and `highestIn` takes a long as its order part. An instant as an order wants "an order part that is not a long" (above) first;
  * **idoc needs no new tag if it is JSON.** `:db/valueType :db.type/idoc` with `:db/idocFormat :json` took the JSON text that `ecto_hosi` already writes for its maps and lists, and `idoc-match` found inside it (the keys of a JSON document are strings: `{"shortcode" "inu"}`). A schema type could keep the value a `VStr`.
    Seen on the jar: what is read back is a parsed structure with string keys, which `Takka.Edn` does not read as a value (it reads scalars, and a vector as a tuple); `[?e :attr ?v]` is an error for an idoc (`:db/noindex true` is its default since 1.2.8: read by entity or pull, or search with `idoc-match`); a new value replaces the whole document;
  * **nobody asks for these now.** sukhi keeps times as longs, and keeps its maps as JSON text that it never searches inside. They are written down so that whoever asks next has the cost in front of them.
* **`:db/isComponent`** is not passed (`Khana/Dtlv.hs` passes `valueType`, `cardinality`, `unique`, `tupleAttrs` and `fulltext`). On the jar it works: retracting the parent takes its components, and a ref shared with other entities stays.
  *(A guess:)* it would do little here. The events are the truth, and a cascade done by the khana is not an event, so a khana and one folded again from the events would differ. `ecto_hosi` does the cascade itself and writes the deletes. What it could still give is a `pull` that goes into components. Do nothing until a host asks.
* **A world written before its schema changed.** Not tried. `ecto_hosi` now gives a `hosi_keyword` field the type `keyword` where it was `string`, and a `tuple: true` rule a tuple attribute where it had a JSON text; the old facts stay in the events.
  To try before anyone with data they care about switches: does a world with `"closed"` as a string open under `keyword` (the guide says `SchemaRefused` for "a type that clashes with the data"), and what does a fold do with the facts of an attribute that the schema no longer names?
  (sukhi's `docs/STORE.md` says nothing is carried over from the earlier stores, so it may not matter yet.)

## Not built, no plan

Distribution and sync, metrics and capacity forecasts, anonymous metrics, JSON-LD import, thaw and compression of cold chunks, merging of cold chunks, a timer inside hosigraph, compression of chunk values (a general compressor on top of chunk version 2 did not make it smaller),
a dictionary for IRIs from outside the ID space, pruning horizons. Windows, and a directory on a network file system.

## S3, against RustFS

**Decided (2026-11-12, nyanrus):** the object store we build and test against is **RustFS**
([github.com/rustfs/rustfs](https://github.com/rustfs/rustfs); Apache-2.0, S3-compatible, written in Rust).
**Not decided:** how hosigraph talks to it (see item 1).

### What we know about RustFS (read from its docs on 2026-11-12; nothing has been run here yet)

* S3 API on port **9000**, web console on **9001**.
* **Path-style** addressing is the default: `http://host:9000/<bucket>/<key>`. Virtual-hosted style needs
  `RUSTFS_SERVER_DOMAINS`, wildcard DNS and a TLS certificate. We use path-style only.
* **AWS Signature V4.** Region is `RUSTFS_REGION`, default `us-east-1`.
* The docs list put, get, head, copy, delete, multi-delete, `ListObjects`/`ListObjectsV2`, multipart, versioning, and "range reads".
* The docs **do not say** how range reads behave (syntax, multi-range, edge cases). We only ever send one range, `Range: bytes=a-b`;
  the spike below has to confirm it.
* The docs say RustFS is **not identical to AWS S3 or MinIO** and tell you to validate the release you use.
* Credentials: one source says `rustfsadmin` / `rustfsadmin`; set `RUSTFS_ACCESS_KEY` and `RUSTFS_SECRET_KEY` ourselves in every test.
  Never write a key in the manifest, the repository or the README.
* Docker: image `rustfs/rustfs`, runs as UID/GID 10001, so a bind-mounted data directory must be owned by 10001.
  Pin a version tag in tests, not `latest`.
* On this Mac: `podman` is at `/opt/podman/bin/podman` (whether a machine is running is not checked); there is no `docker` and no `rustfs` binary.
  `curl` is 8.7.1 and knows `--aws-sigv4`.

### 0. Spike first (small, nothing committed)

Start RustFS (podman), make a bucket, then try by hand and write down what really happens:

* PUT a file; GET the whole file; HEAD for the size.
* GET with `Range: bytes=100-199`: is it `206`, is `Content-Range` right, are the bytes exact? A range past the end? `bytes=0-0`?
* GET a key that is not there (status, body); wrong secret key (status, body); a bucket that is not there.
* `ListObjectsV2` with a prefix (needed later for cleaning up chunks that are not in the manifest).
* With `curl --aws-sigv4 "aws:amz:us-east-1:s3" --user ...`: does a PUT need `x-amz-content-sha256` set by hand (e.g. `UNSIGNED-PAYLOAD`) or does curl add it?
* How long one range GET takes on localhost (only to have a floor; it says nothing about a real network).

The answers decide items 1 and 4. Put them in `docs/design-notes.md` when known.

### 1. Open decision: the transport

| | what | cost |
|---|---|---|
| **A. `curl` subprocess** (the leaning) | `--aws-sigv4` signs, `-r` asks for a range; one process per request | no new Haskell package, no C; needs `curl` on the machine; ~ms of process start for each request |
| B. HTTP library + our own SigV4 | one connection kept open | today the package needs only `aeson` besides GHC's own libraries and has no C; an HTTP + TLS stack usually brings a large tree of packages (the crypto ones often with C inside), and our own signing code to test |
| C. plain HTTP by hand over `network` | same | the same signing code as B, and no TLS (fine for localhost, not for a real deployment) |

Whatever we choose, it sits behind the interface in item 2, so it can change later.

### 2. An object-store interface (`Hosigraph.ObjectStore`)

A small record, like `ChunkSource`: `put key bytes`, `getRange key offset len`, `getWhole key`, `headSize key`, `list prefix`.
(`delete` waits for the S3 side of Erase.) Errors keep three things apart: **the object is not there**, **the store cannot be reached**, **we are not allowed**.

### 3. A fake bucket for tests (no network)

A directory-backed (or in-memory) store with the same interface that **counts requests and bytes**.
Everything below is first tested on it, and it always runs. This is where "a question about the past skips the chunks it does not need" is proved with numbers.

### 4. The RustFS client

Path-style, SigV4, endpoint / region / bucket / key prefix in `HosiConfig`; credentials from the environment or the caller, never stored.
Single PUT first; add multipart only when a chunk is big enough to need it (the limits are not in RustFS's docs: find them in the spike).

### 5. Upload

After `seal`: put `chunk.hosi` and `filter.bin` (not `mphf.idx`: the block table is enough on the deep path, and it was bigger than the data in our measurement).
Order: **data first, manifest last**, so a crash leaves an object that nobody points at (like a chunk that is not in the manifest today).
Compare the size and sha256 with the manifest before the manifest says "remote".
**Question for nyanrus:** after a verified upload, does the local copy stay (a cache) or go (remote only)? Freezing probably decides; for now keep it.

### 6. Manifest: where a chunk lives

A chunk entry gains a place (local, or remote with bucket and key prefix). Old manifests must still read as "local".
Check how the manifest hash and signature cover it. Cleaning of objects the manifest does not list: `list prefix`, then delete (once the store has `delete`).

### 7. Events picks a source for each chunk

`sourceFor :: ChunkEntry -> ChunkSource`. A remote chunk's filter is one whole GET (~11 KB for 4,000 ids). Open question: keep filters on local disk
so that a restart does not fetch all of them again? Also the known gap: filters are cached with no limit.

### 8. When the store is unreachable

`history` must **fail**, never return a shorter list that looks complete. Add a `HosiError` for it. Timeouts. Retry with backoff for 5xx and connection errors;
no retry for 403 and 404. Test this with the fake bucket (it can be told to fail).

### 9. `verify` for remote chunks

Cheap by default (size and `headSize`), a deep option that downloads and checks the sha256 and the filter.

### 10. A test against a real RustFS (gated by environment, like `HOSIGRAPH_DTLV`)

For example `HOSIGRAPH_S3_ENDPOINT`, `HOSIGRAPH_S3_BUCKET`, `HOSIGRAPH_S3_ACCESS_KEY`, `HOSIGRAPH_S3_SECRET_KEY`. It checks: upload then read back, the range reads give
exact bytes, open = 4 ranges, one lookup = 1 range, a missing object and a wrong key give the right error. In CI: a `rustfs/rustfs:<pinned tag>` service container on ubuntu-24.04
(mind UID 10001). Locally: podman.

### 11. Measure and write it down

Open and lookup against RustFS on localhost, with the request counts from item 3. Localhost is a floor, not a forecast: say so in the README.

### 12. Docs

README: how to configure the store; how to start RustFS for the tests. `docs/design-notes.md`: what the spike found, and where RustFS differs from the S3 the design document pictures.

### Not now

* Erase and bucket versioning: the design document says an Erase is complete only after the old versions are deleted too (line 123 of the design document). Check what RustFS does with versioning then.
* Freezing / cold chunks, garbage collection of remote chunks, lifecycle rules.
* Expanding RustFS: a single-node single-drive deployment cannot be grown in place (its docs say so). Not our problem for tests; it matters for a real deployment.

## Backfill: live serving

`backfillSince` (see [the guide](docs/guide.md#reading-the-past-into-a-khana-newest-first)) builds a khana beside the world, newest write first, and says which creation months are whole.
What is not built is using it *while it is still being built*, as the world's own khana. This is not the S3 part above. Nothing here is decided; these are the open points.

* **Writes that arrive meanwhile.** A write is newer than every event the backfill has not read. One way: apply it to the backfill khana as the forward fold does (a Create replaces, a Delete takes out),
  and also mark the (entity, attribute) or the value as settled in the backfill's state (`Hosigraph.Backfill.Back`), so that an older event read later cannot override it. This needs a test of its own,
  with a write between every two steps, against folding everything oldest first.
* **A khana that is only part of the world.** With a month later than the oldest, the khana lacks the older entities. Can it be the world's own at all? `getEntity` of an old id would answer "nothing", which is
  a wrong answer, not a missing one. Either the world's khana is only ever the whole one (and the partial one is a separate thing you ask by name), or an answer must say which months it covers.
* **Swapping it in.** When it is whole, it needs the folded seq (the last seq) and the schema mark, written the way `buildKhana` writes them, and then the place of the old khana (`installFreshKhana`).
* **The lock.** The events lock is held for the whole reading (about 107 s for 528,000 events), so writes wait. Letting them in means a snapshot of the manifest, the erase records and the log taken under the lock,
  and chunks that stay readable while `clean` or `freeze` retire one (they do not, today: a chunk that goes away halfway makes the reading fail).
* **The order.** The reading is by write order (newest first). Reading by month (the newest month first, all its chunks) also works and makes each month whole as soon as it is read; see note 10, item 1, of
  `docs/design-notes.md`. Only `eventsBackEach` would change.
* **Speed.** The time was nearly all `dtlv`, one process for each transaction of 2,000 facts (the program's own CPU time was 2.8 s of 107 s on the real world). `KhanaDtlv` (`dtlv exec`) is gone, so a transaction no longer
  pays for a start: 200,000 facts in 1.8 s through the repl against 4.1 s through `exec` (2,000 a transaction; a larger one was slower through the repl: 2.1 s at 20,000). Not measured on the real world (528,000 events).

