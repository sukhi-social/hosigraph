# hosigraph, the long guide ⭐

This is the whole guide. The short, friendly introduction is the [README](../README.md).

A small embedded database that **keeps the past as events**, and keeps **"now" as a copy you can throw away**.

This is the **Phase 1 prototype** of the [design document v0.2.0-draft](hosiDB-design-v0.2.0-draft.md),
with the first half of **Phase 2** on top: the Binary Fuse filter and the "deep path" that reads a chunk by ranges (see "Phase 2 so far"),
and the local part of **Phase 3**: Erase, cleaning and freezing (see "Phase 3 so far").
**S3 itself is not here yet.**
The project used to be called **hosiDB**, and the design document still uses that name. The code is in Haskell.

## The idea

* Every change is an **event**: a `Create` or a `Delete` of one fact. Events are only added, never edited.
  **The events are the truth.**
* **"Now"** is a Datalog database called the **khana**. hosigraph builds it by folding the events.
  If you lose it, or you do not trust it, throw it away. The events build it again.
* So there are two ways to read:
  * **now** → ask the khana (`getEntity`, `query`)
  * **the past** → ask the events about one ID (`history`)

```
 write ──► events/tail.log ──seal──► events/chunks/c000001-create-2026-11/{chunk.hosi, mphf.idx, filter.bin}
                │                                  │      (events/manifest lists the chunks)
                └───────────── fold ───────────────┴──────────► khana/  (now, Datalog)
```

The goal of Phase 1 is small: **write, seal, restart, and get the same answers.**
The tests stop the system in 5 places and check the result.
They also test a khana that is behind the events and a khana that is ahead of them.

## What the names mean

| Name | Meaning |
|---|---|
| **hosi** (星, "star") | One fact. |
| **graph** | The links between facts (subject, predicate, object). |
| **khana** (刹那, setsuna) | Pali for "a moment" (Sanskrit: kṣaṇa). In hosigraph, it is **"now"**: the events folded into one moment. The design document calls it the **core material** (芯材). |
| **Events** | The part that holds the truth: the tail log, the chunks, the manifest, and "find the events of one ID". The name is a placeholder (`Sky` was another candidate). |
| **Probe** | The check for a point lookup. It is a pun: `Hosi` + `Khana` sounds like "hoshi kana", "is it a star?" |

`historyTrace` asks the chunk's filter first (see "Phase 2 so far"); a chunk that the filter turns away is never opened.
After the MPHF (see "How the files are laid out") says "it is here", Probe asks the slot twice. It answers one of four things:

* `Star`: the key matches.
* `LookAlike`: the fingerprint matches by chance, but the key is different.
* `NotStar`: the fingerprint is different. (On the deep path, with no MPHF: the id is outside every block's range.)
* `EmptySky`: the slot is empty.

On the deep path `LookAlike` also means "the id is inside a block's range, but that block does not have it".

## Get started

You need `ghc` 9.10 or newer (tested with 9.10.3) and the packages `aeson` and (for the tests) `QuickCheck`. There is no C to compile.
The khana lives in Datalevin, so to use a database you also need either a JDK 21 or newer and the Datalevin jar, or the `dtlv` executable (see below).

Everything is in this repository. [`takka/`](../takka/README.md) holds the words that hosigraph and Datalevin share
(values, schema, EDN, and the syntax of Datalog queries and pull patterns), in plain Haskell. cabal builds it together with hosigraph.
Takka was called Hevin before; "takka" is Pali for reasoning and logic.
(Takka once also had a small database engine on dlmdb, and hosigraph could keep its khana there. That was removed. It is in the git history, commit `2aee1d7`.)

```sh
cabal build all
cabal run hosigraph-demo                 # a walk-through that moves a clock from November to December
cabal test                               # 30 tests that need no Datalevin; more with Datalevin (see below)
```

Tested: GHC 9.10.3 with `cabal build all`, on macOS (arm64). Linux is not tested here;
the GitHub Actions workflow ([`test.yml`](../.github/workflows/test.yml)) runs the tests on ubuntu-24.04, with the JVM.

> **`scripts/build-ghc.sh`** builds with plain `ghc`, for machines where cabal cannot reach Hackage:
> `scripts/build-ghc.sh app` (or `test`). It writes to `build/`. Plain `ghc` must be able to see `aeson` (and `QuickCheck` for the tests).
> **It has not been run to the end yet.** An older version stopped at once because `$target` was never set; that is fixed,
> but on the Mac where this was tried, plain `ghc` could not see `aeson`. Use cabal until then.

### Run the tests against Datalevin

Most tests need a real Datalevin, so tell them where it is. If you do not, they print that they skipped those parts.

```sh
HOSIGRAPH_DTLV=/path/to/dtlv cabal test        # the native dtlv
HOSIGRAPH_DATALEVIN_JAR=/path/to/datalevin-1.2.10-standalone.jar HOSIGRAPH_JAVA=/path/to/java cabal test   # a resident JVM (JDK 21 or newer)
```

| You give | Tests that run |
|---|---|
| nothing | 30 (events, chunks, filters, the deep path, tail log, the EDN writer) |
| `dtlv` | not counted again since Phase 2 (it was 46) |
| the JVM | not counted again since Phase 2 (it was 47) |
| `dtlv` and the JVM | all 100 (including: write with the JVM and read with `dtlv`, and the reverse) |

The same scenarios run on `dtlv` and on the JVM. The tests choose the khana themselves.

The demo needs the JVM: set `HOSIGRAPH_DATALEVIN_JAR` (and `HOSIGRAPH_JAVA` if `java` is not on your path).

## Use it

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Exception (bracket)
import Hosigraph
import qualified Data.Map as M

-- Give the real jar. See "Where 'now' lives" for `dtlv`.
cfg :: HosiConfig
cfg = defaultConfig { hcKhanaBackend = KhanaDatalevinJvm "java" "/path/to/datalevin-1.2.10-standalone.jar" }

main :: IO ()
main = bracket (openHosiWith cfg "/tmp/hosi" schema) closeHosi $ \h -> do
  r <- write h [ Create (Tmp 1) "person/name" (Lit (VStr "Frege"))      -- Tmp 1 becomes a real ID with a time in it
               , Create (Tmp 1) "person/age"  (Lit (VInt 70)) ]
  let frege = wrTemps r M.! 1
  getEntity h frege >>= print                -- now (from the khana)
  query h "[:find ?n :where [?e :person/name ?n]]" [] >>= print    -- Datalog, as Datalevin reads it
  _ <- seal h                                -- freeze the tail log into chunks
  history h frege >>= mapM_ print            -- the past (from the events)
  where schema = M.fromList [("person/name", identity (attr TString)), ("person/age", attr TLong)]
```

`withHosi dir schema` is a shorter way to open, but it uses `defaultConfig`, which wants `datalevin-standalone.jar` in the place where you run, and `java`.

| Call | What it does |
|---|---|
| `write` | Create and Delete operations in one call. **All of them go in, or none.** |
| `writeIf` | `write`, but only if some facts of **now** are still true. The check and the write happen under one lock. See "Writing only if". |
| `writeMany` | Many batches with one append, one fsync, and one khana transaction. |
| `getEntity`, `query`, `queryCells` | Read **now**. |
| `getEntities`, `queryMany`, `queryCellsMany` | Read **now**, many questions at once. |
| `history`, `historyTrace` | Read **the past**: every event of one ID, from the chunks and the tail log. `historyTrace` also shows which chunks it skipped (by the zone map, and by the filter) and what each probe answered. |
| `seal` | Turn the tail log into chunks. `HosiConfig` can also seal by itself, by size or by age. |
| `verify` | Check the manifest signature, the chunk hashes, the footers, and the MPHF lookup of every ID. It also checks that **the khana equals a khana built again from the events.** An empty list means all good. |
| `rebuildKhana` | Throw the khana away and fold the events again. |
| `erase` | Make entities gone, from the events and from "now": some ids, a range of ids, a span of creation time, or what an account owns. See "Phase 3 so far". |
| `clean` | Let what `erase` took out go from the disk: write the chunks again without it, and make the khana a new file. |
| `freeze` | Put a month that nobody has read or written for a while into one cold chunk. Does nothing unless `hcColdAfterMs` is set. |

You can also use `Hosigraph.Events` alone, without a khana. The tests cover this.

## Where "now" lives

The khana is a Datalevin database. There are two ways to reach it:

| | `KhanaDatalevinJvm java jar` (default) | `KhanaDtlv path` |
|---|---|---|
| What it is | Datalevin in **one JVM that stays up** (one JVM for each `Hosi`) | Datalevin through the `dtlv` command, **one process for each operation** |
| One operation | about 0.6 to 0.9 ms (the first call: 1.3 to 1.6 s to start) | about 25 ms (the first call: about 0.4 s) |
| What you need | JDK 21 or newer, and the jar (about 93 MB) | one `dtlv` file |
| `(max ?x)` and `(min ?x)` | work | **do not work** (see "Limits") |
| Files in | `khana-dtlv/` | `khana-dtlv/` |

```haskell
cfg = defaultConfig { hcKhanaBackend = KhanaDtlv "/usr/local/bin/dtlv" }
cfg = defaultConfig { hcKhanaBackend = KhanaDatalevinJvm "java" "/path/to/datalevin-1.2.10-standalone.jar" }
```

* If `dtlv`, `java`, or the jar is missing, `openHosi` stops with `BadArgument` **before it touches anything**.
  The default expects `datalevin-standalone.jar` in the place where you run, so give the real path.
* To switch from one khana to another, just fold the events into the new one. The tests do this in both directions.
* The two Datalevin khanas read the same files. **Do not open them at the same time.** hosigraph does not check the LMDB lock.
* `KhanaDtlv` sends the code on standard input to `dtlv exec`. A write is one `transact!`, so "all or none" holds.
* `KhanaDatalevinJvm` talks to the JVM one line at a time. `closeHosi` ends it, and so does the end of your process.
  If the JVM dies, that one call fails (a write fails with `WriteRejected`), and the next call starts a new JVM.
  Nothing is lost, because the khana can be built again from the events.
* The schema maps to the Datalevin schema (`:db/valueType`, `:db/cardinality`, `:db/unique`). `TAny` has no type.
* Your EDN Datalog goes to Datalevin as it is, and `pull` works in `queryCells`.

## Writing only if

Read-then-write has a hole: between your read and your write another writer can go in, and your write is made
from a "now" that is no longer there (two people press "like" at once, and one like is lost). `writeIf` closes it.

```haskell
data Cond = Holds HosiId Keyword Value   -- this entity has this value for this attribute, now
          | Lacks HosiId Keyword Value   -- ...does not have it
          | Unset HosiId Keyword         -- ...has no value at all for this attribute

writeIf :: Hosi -> [Cond] -> [Op] -> IO (Either [Cond] WriteReport)

-- "add one to the likes", and read again if somebody was first
bump h post = do
  [m] <- getEntities h [post]
  let n = case M.lookup "post/likes" m of Just [VInt k] -> k; _ -> 0
  r <- writeIf h [Holds post "post/likes" (VInt n)] [Create (Id post) "post/likes" (Lit (VInt (n + 1)))]
  either (const (bump h post)) (const (pure ())) r
```

* The conditions are checked with the events lock held, against "now" in the khana (one read, however many
  entities are named), and the batch is written under the same lock. No other writer can come between the check and the write.
* `Right report`: the conditions held and the batch went in, as `write` does.
* `Left conds`: these conditions did not hold (in the order given). **Nothing was written, and no seq was used.**
  Retrying is yours: read again, and call again. hosigraph does not loop for you, because only you know what to do with what you read.
* Which one for which job: `Holds` is compare-and-set (a counter, a status that moves from A to B); `Lacks` is "if I have
  not done this yet" (a value in a many-attribute); `Unset` is "set it up once". "Create it if it is not there" needs none of them
  for a new entity: hosigraph gives the ids, and a `unique` attribute in the schema already refuses a second one.
* Bad input is an exception, as in `write`: an empty batch, or a condition on a reserved id (up to 1), is `BadArgument`.
  If a condition fails and the batch is also bad, you hear about the condition first.
* There is no `Tmp` in a condition: a new entity has no state to ask about.
* `writeMany` has no conditions: a later batch may depend on what an earlier one did.
* The check reads the same "now" that `getEntity` reads, so what you read and what the check sees can only differ
  if somebody wrote in between. That is what `Holds` is for.
* Seen in testing: with `KhanaDtlv`, a read made at the very moment another call writes could fail to open the
  database. The lock only orders the writers; it does not stop an outside read. (The JVM backend was not tried for this.)

Through the C door it is the same call: `write` takes an optional `:when`, a vector of `[:holds id attr v]`,
`[:lacks id attr v]` and `[:unset id attr]`. When one does not hold the answer is the error kind `:conflict`
(status `HOSI_ERR_APP`), and its message is the failed conditions as EDN text. From Elixir: `Hosi.write(db, ops, when: [...])`
(then `{:error, {:app, :conflict, message}}`).

## Faster with groups

Every call to a Datalevin khana is a round trip. To save them, ask and write in groups.

```haskell
es <- getEntities h [a, b, c]                                  -- [Map attribute [values]], in the order you ask
rs <- queryMany h [(q1, []), (q2, [InScalar (VInt 70)])]       -- [rows], in the order you ask
cs <- queryCellsMany h [(pullQ, []), (q1, [])]                 -- pull is also accepted

rs <- writeMany h [ batch1, batch2, batch3 ]                   -- [Either HosiError WriteReport], in order
```

**Reading in groups**

* All answers come from **the same moment**. No other write comes between them.
* If one question fails, the whole call fails and returns nothing. hosigraph refuses a bad query (and `InRules`) **before it sends anything**.
* `dtlv` starts one process, however many questions you ask. A list of 3000 IDs travels as data, so the JVM does not refuse to compile it.

**Writing in groups**

* `writeMany` gives the same result as one `write` for each batch. The batches stay separate: each one has its own `Tmp` names and its own IDs.
* A bad batch (empty, or the reserved ID 1) becomes `Left (BadArgument …)` in its place. The others go in.
* If the khana refuses the whole lot, hosigraph cuts all batches from the log and writes them one by one, as `write` does.
  The good ones go in. Only the refused ones become `Left (WriteRejected …)`. The seq numbers of a refused batch are freed for the next one.
* A seal that falls due happens once, after the last batch. Only that report has `wrSealed`.
* If the system stops in the middle, the log keeps the first batches or none, **never half of a batch.**
* There is no automatic grouping of `write` calls from many threads. That needs a use case with many writers.

hosigraph also groups work inside: when it opens, it folds all unfolded batches of the tail log in **one** transaction,
and it reads `foldedSeq` once. A catch-up of 5 batches starts 3 `dtlv` processes, not 8.

### Measurements

macOS, arm64. One item is a `write` of 2 facts (a name and an age). The benchmark is not part of the project.

| | `KhanaDatalevinJvm` | `KhanaDtlv` |
|---|---|---|
| `write`, one item at a time | 0.9 ms | about 25 ms |
| the same items in one batch (for each item) | 0.05 ms | 1.2 ms |
| `getEntity`, one at a time | 0.6 ms | about 23 ms |
| `getEntities`, 200 together | about 3 ms | about 23 ms |
| 6 queries (one by one → `queryMany`) | 2.6 → 1.9 ms | 156 → 28 ms |
| `stats` | 1 ms | about 24 ms |

* **Grouping helps `KhanaDtlv` most**, because starting a process is almost all of its time.
  A JVM call is cheap already (200 entities: about 40 times faster; 6 queries: about 1.4 times).
* "The same items in one batch" is the **upper limit** for grouped writes. `writeMany` (separate batches) comes close to it.
* The fsync of the tail log makes almost no difference on this Mac (under 0.1 ms). hosigraph calls only `fsync(2)`,
  and on macOS that does not wait for the disk write cache. On systems that really wait, the time is higher. It is not measured.

## Limits

* `query` does not accept `InRules`, because a `Rule` is a structure and not EDN text. Write the rules inside the query.
* **`KhanaDtlv` cannot use `(max ?x)` or `(min ?x)`.** In `dtlv exec` 1.2.10, they return the collection of values, not the maximum or the minimum.
  They seem to resolve to `clojure.core/max` and `min`. `count`, `sum`, and `avg` are correct, and the same jar on a JVM is correct too.
  So the problem is in `exec` of the native `dtlv`. hosigraph returns an error and does not read the result.
  A canary test tells you when upstream fixes it.
* The query parser refuses aggregates such as `distinct` or `(max 2 ?x)` before they reach Datalevin, so no backend can send them.
* NaN and ±Infinity have no EDN form, so hosigraph refuses to write them.
* The order of rows is not guaranteed (a Datalevin answer is a set). Sort the rows before you compare them.
* Tested only with Datalevin 1.2.10 on macOS arm64 (JVM: Temurin 21). The memory use of the resident JVM is not measured.

## How the files are laid out

| Word in the design document | Form here |
|---|---|
| Tail log | `events/tail.log`. One `write` is one record (length + CRC32). If a record is cut, hosigraph keeps what is before the cut and removes the rest. |
| Hot chunk | `chunk.hosi`, `mphf.idx` and `filter.bin` in `events/chunks/<name>/`. One chunk for each kind and (creation) month. Nobody changes it. A chunk from before Phase 2 has no `filter.bin` and is still read. |
| ID | `(milliseconds since 2026-01-01) << 21 \| sequence number` (63 bits). The ID alone tells you the month. |
| MPHF | A *minimal perfect hash*: it gives each known key its own slot, with no collisions. This one is PTHash-style and written for this project (`Hosigraph.Mphf`). Each slot has a 32-bit fingerprint and the position of the body, which `Probe` checks. |
| Manifest | `events/manifest`. Line 1 is JSON. Line 2 is `sha256:`. hosigraph writes a tmp file and replaces the manifest by a rename. A chunk that is not in the manifest is outside the world; hosigraph cleans it at open. A world that has ever erased something is `hosi-manifest/2`, and a build that knows only `/1` refuses it (it must not read it and show what was erased). A world that never erased is still written as `/1`, byte for byte as before. |
| Cold chunk | A chunk made by `freeze`: the events of one (kind, month), in bigger blocks (`hcColdBlockEvents`, 4,096 events; a hot chunk has 256), **no `mphf.idx`**, and `"tier": "cold"` in the manifest. It is found by the footer and one block, as on the deep path. |
| Erase record | `events/erase/<chunk>-<generation>.bin`, format `HOSIERS1`: the ids of that chunk that were erased, as sorted ranges (6 to 8 bytes for each scattered id; a whole day is one range of 28 bytes). The manifest lists it next to the chunk; it is not inside the chunk's directory and not in the chunk's hash, because it belongs to the world, not to the chunk. It goes when the chunk is written again (`clean`, `freeze`). |
| Use of months | `events/access.json`: when each (kind, month) was last read or written. A hint for `freeze`. It exists only when `hcColdAfterMs` is set. |
| Core material | `khana/` or `khana-dtlv/` (`Hosigraph.Khana`). Create becomes `Add`, Delete becomes `Retract`. The folded position (`folded-seq`) is in the same transaction. |
| Update | None. For an attribute with cardinality one, a newer Create wins. |
| Binary Fuse filter | `filter.bin`: a 3-wise Binary Fuse filter with 16-bit prints over the entity ids of the chunk, written for this project (`Hosigraph.BinaryFuse`, after Graf and Lemire 2022). "Certainly not" is always right; "maybe" is wrong about 1 time in 65,536 for an id that is not there. |
| Zone map | The manifest keeps the month, the ID range, and the seq range of each chunk. `historyTrace` shows how many chunks it skipped. |

The names of the file formats (`HOSICHK1`, `hosi-manifest/1`, `hosi.meta/folded-seq`) keep `hosi`.

### When the system stops

`openHosi` repairs these at the next open:

| Where it stopped | What happens at reopen |
|---|---|
| While it wrote the tail log | hosigraph cuts the half record. If the khana saw it, hosigraph builds the khana again. |
| After the tail-log write, before the fold into the khana | hosigraph folds it at reopen. If the khana refuses it, hosigraph drops that record. |
| While it wrote a chunk | hosigraph cleans the chunk that is not in the manifest. The tail log stays as it is. |
| After the manifest swap, before the tail log is emptied | hosigraph ignores records with a sealed seq. Nothing is doubled. |
| While it wrote `manifest.tmp` | hosigraph removes the tmp file. The old world stays. |
| While `erase` wrote the records, before the manifest swap | hosigraph removes the records that the manifest does not list. Nothing was erased (the seal that came first stands on its own). |
| After the manifest swap of an `erase`, before the khana forgot the entities | The khana's `erase-gen` is behind the manifest's, so hosigraph builds the khana again at open, from the events, which leave the erased ids out. |
| While `clean` or `freeze` wrote a new chunk | The scratch directory (`.rewrite-…`, `.freeze-…`) is not in the manifest, so it goes at open. The old chunk stays. |
| After the manifest swap of a rewrite or a freeze, before the old chunk is removed | The old chunk and its record are not in the manifest any more, so they go at open. |
| While `clean` made the khana new | The old khana stays until the new one is checked and renamed. A leftover `….new` directory goes at open. If it stopped between the remove and the rename, there is no khana at open, so hosigraph builds it again. |
| While `access.json` was written | A leftover `access.json.tmp` goes at open. A missing or unreadable `access.json` means every month was just used, so nothing is frozen early. |

## Phase 2 so far

The design document says Phase 2 is "S3 and the deep path. Zone maps, Binary Fuse filters, Range GET."
and "A question about the past skips the chunks that it does not need to read."
The zone map was already here. This is what was added, and what is still missing.

**The order of the gates.** `history` (and `historyTrace`) asks each chunk the cheapest question first:

1. the zone map in the manifest (month, ID range): no file is touched;
2. the chunk's erase record, if it has one (Phase 3): it is in memory, so an erased ID is turned away without a read;
3. the chunk's filter, `filter.bin`: one small file, read once and kept in memory;
4. only for a "maybe": open the chunk reader;
5. the MPHF slot, its fingerprint, and the key check.

`LookupTrace` has the fields `ltSkippedByErase` and `ltSkippedByFilter`. A chunk that the record or the filter turns away is **never opened**
(a test checks that `xsReadersOpen` stays 0). `verify` also checks that the filter never says no to an ID that is in the chunk.
(The design document puts the erase check last. Asking it first gives the same answer for less work; see item 9 of [`design-notes.md`](design-notes.md).)

**The deep path.** A reader now takes a `ChunkSource` (`Hosigraph.Chunk`): "give me these bytes of this file",
"give me this small file whole", "how big is `chunk.hosi`". `localSource` answers from a directory.
An S3 bucket would answer the first one with Range GET. If the source has no `mphf.idx`, the reader does not need it:
it binary-searches the footer's block table (first and last ID of each block) and reads **one block**.
Measured on one made-up chunk of 4,000 IDs (256 events a block), counting the bytes the reader asks for:

| | bytes |
|---|---|
| `chunk.hosi` | 40,428 |
| `mphf.idx` | 52,036 |
| `filter.bin` | 11,296 (about 22 bits an ID; with 50,000 IDs a test sees 19 to 20 bits, and the paper's formula goes toward 18 for very many) |
| open a chunk: 4 ranges (start, header, trailer, footer) | 645 |
| one lookup, no `mphf.idx`: 1 range | 1,604 to 2,564 |

The tests count the ranges too: opening takes 4 and less than a quarter of the file; one present ID takes one range and less than a tenth of the file;
an ID past the last block takes none. The MPHF way and the block way give the same answers (a property test, for both chunk versions).
`mphf.idx` is bigger than the data here (about 13 bytes an ID). On the deep path a chunk could do without it.

**Still missing.**

* **S3.** There is no S3 `ChunkSource`, no upload, and no manifest tier for a chunk that lives far away. `Events` only builds local sources.
  The object store to build against is **RustFS** (decided). How hosigraph talks to it is not chosen yet. The package has no C and no HTTP or TLS stack,
  so the candidates are a `curl` subprocess (`--aws-sigv4`, `-r` for ranges) or our own signing over an HTTP library. The plan is in [`TODO.md`](../TODO.md).
* The filters are kept in memory with no limit, outside `hcReaderCacheBytes` (about 2.3 to 2.8 bytes an ID). A bucket with millions of chunks needs a limit.
* Only a cold chunk (Phase 3, `freeze`) is written without `mphf.idx`; the deep path is reached by it, and by a source that does not have the file.

## Phase 3 so far

The design document says Phase 3 is "Erase, cleaning, freezing". What is here is the **local** part: it needs no S3.
The S3 side (a remote chunk that must be written again, old object versions, the sweep of a bucket) comes with S3 itself; see "Still missing" at the end of this section.

### Erase: make entities gone

```haskell
r <- erase h [ EraseIds [post]                                       -- a post the user deletes later
             , EraseBetween from to                                  -- everything created in [from, to), UTC
             , EraseRange lo hi                                      -- every id from lo to hi
             , EraseOwnedBy ["post/author", "media/owner"] [account] Nothing ]   -- what an account owns
```

One call is one new manifest, however many erasures it holds. The report (`EraseReport`) says the new erase generation, the chunks that were dropped whole,
the chunks that got a record, how many facts left "now", and the ids that `EraseOwnedBy` found.

* **By date.** An ID has its creation time in it, so a span of time is one range of IDs. "Date" is the time the *entity* was created, not the time of an event:
  an edit made today to an old post is in the old month, so erasing that month takes the whole post. Boundaries are UTC; the caller converts.
  A retention rule ("nothing older than N days") is the same call with a moving `to`.
* **By owner.** What an account owns is *not* one range: it is spread over time, and some of it was deleted already (gone from "now", but its Create and Delete are still in the chunks).
  So `EraseOwnedBy` looks in the events: every entity that has, in a Create event, one of the attributes you name with one of the owners as its value.
  hosigraph does not know what `post/author` means; you pass the names on each call. The account itself is not an owned thing: add `EraseIds [account]`.
  A window (`Just (from, to)`) limits it to what was created then. Any Create event counts, not only the newest, which is what finds a thing whose pointer was deleted;
  the other side is that a thing moved to another owner afterwards is also found for the first one (hosigraph has no idea of a transfer).
  hosigraph does not look at the type of the value either: an integer attribute that happens to equal an owner's id matches, if you name that attribute.
  It reads every Create chunk the window reaches, while writers wait.
* **What it does.** Under the events lock: seal the tail (a record cannot reach the tail log); for each chunk, if the erase covers all the ids the chunk can hold, **drop the chunk**
  (no record, no rewrite: erasing a whole month is only a manifest swap); otherwise write a **record** (`events/erase/…`, the ids as ranges); swap the manifest, which is the commit point;
  then take the entities' own facts out of the khana, and write the erase generation in the same transaction.
* **After it.** `history` of an erased ID is empty (the record is asked right after the zone map; no filter and no block is read). A rebuild of the khana leaves the ID out.
  A `write` to an erased entity is refused (`BadArgument`) for as long as its chunk still has the record; a chunk that was dropped whole leaves no record, so there the ID is free again.
* **What stays.** A fact of *another* entity that holds an erased ID as its value (a reply to an erased post, a mention) keeps the number. It points at nothing. This is on purpose:
  blanking such pointers would rewrite other people's entities, which is a different and bigger operation, and it is not here.
* **If it stops** after the manifest swap and before the khana step, the erase is committed all the same (the events are the truth), and the next open builds the khana again.
  If the khana step fails without a stop, `erase` throws the khana's exception; reopening, or `rebuildKhana`, makes "now" agree.

### Clean: let it go from the disk

`erase` hides at once, but the bytes are still in the chunk, and the retracted facts are still in the khana's file. (This was measured: after a retract, a marker text was still in Datalevin's
`data.mdb` twice, in the native `dtlv` and in the JVM alike. LMDB reuses freed pages *sometime*, which is luck, not a promise. A plain copy keeps the bytes; a compacted copy has none.)
`clean` finishes the job, in this order:

1. **Rewrite the chunks that have a record.** For each one: read its events (leaving out the erased), write a **new** chunk (new zone map, filter and MPHF), and, under the lock, if the old entry
   is still in the manifest exactly as it was, swap the manifest (one swap for each chunk) and remove the old chunk and its record. A chunk with nothing left gets no new chunk.
   No lock is held while the new chunk is written, so writes go on. `coMaxChunks` limits the work of one call: a host can call `clean` a little at a time, in each quiet moment.
2. **When no chunk has a record any more, and the khana file may still hold what an erase took out, make the khana a new file.** The khana keeps `clean-gen` (the erase generation at which
   the file was last known to hold no erased value) next to `erase-gen`; `clean` acts when it is behind. This also covers an erase that only dropped whole chunks, which needs no rewrite.
   The quick way is a **compacted copy** of the khana (no replay: what is alive is written, what was taken out is not). It is not trusted: the copy is opened and compared with the live khana
   (same folded seq, same number of facts), and if it cannot be made or does not agree, the khana is built again from the events. `crKhanaRebuilt` says so; if it is often `True`, look at your `dtlv` or your disk.
   This step holds the events lock.

Calling `clean` when there is nothing to do costs a look at the manifest and at one small fact in the khana. There is no scheduler in hosigraph: you call `clean`, `freeze` and `erase`
when it suits the host. Do not call `clean` or `freeze` while you hold any lock of this `Hosi`.

**The sign that it is done** is a test: a marker text is put into some entities; after `erase` it is still in a chunk and in the khana (the test checks that, so it cannot pass by luck);
after `clean` it is in no file under the database directory (`events/` and the khana), the other entities are still there, `verify` is `[]`, and it is still so after a reopen.

### What "gone" means

It means: gone from hosigraph's own files (chunks, records, tail log, the khana). It does **not** mean gone from freed disk blocks, SSD wear-levelling, snapshots, Time Machine or other backups,
an S3 bucket's old versions (S3 is not here yet), or any copy you made yourself. While a record exists it says that *something was created at that moment and erased*
(an ID carries its creation time); there is no content in it, but it is a trace, and it goes when its chunk is written again. After that the manifest stays `/2` and `events/erase/` stays as an empty directory.

### Freeze: a quiet month goes cold

```haskell
cfg = defaultConfig { hcColdAfterMs = Just (30 * 86400 * 1000) }     -- Nothing (the default): nothing is ever cold
...
r <- freeze h defaultFreezeOptions { foMaxUnits = 10 }
```

A *unit* is a (kind, month) pair. **The rule** is the first version of the design document's: a unit is cold when `now - max lastRead lastWrite >= hcColdAfterMs` and it has a hot chunk.
*Read* is a `history` for an ID of that month (for both kinds: "somebody was interested in that month"); *write* is a seal that made a chunk there. Nothing else counts: not a rebuild of the khana
(it reads everything, in order, and would make everything warm), not an erase, a rewrite or a freeze. The units are frozen in order of kind, then month, oldest first, and `foMaxUnits` limits one call.

A freeze puts the hot chunks of a unit into **one cold chunk**: bigger blocks (`hcColdBlockEvents`, 4,096 events), no `mphf.idx`, `tier` `"cold"` in the manifest. It reads through the erase records,
so a freeze **also finishes the Erase** on that unit (the records of the chunks it replaced go with them). `history` answers the same before and after; only where the events are found changes
(the footer and one block, not an MPHF slot). The khana is not touched.

* A late write into a frozen month makes a new hot chunk. The next freeze puts it into a **second** cold chunk. Cold chunks are not merged with each other.
* **There is no thaw.** Locally the cold way (footer, one block) costs a little more for each `history` than the MPHF way (about 2 times in the probe under "Measured", and both are microseconds),
  so a thaw would buy little here.
* **There is no compression.** The format has none at all; leaving out `mphf.idx` is what makes a cold chunk smaller.
* `events/access.json` holds the use of each unit. It is written at every seal, at the end of `freeze`, and at close (a tmp file and a rename; no fsync, it is a hint; a failed write is ignored).
  Without `hcColdAfterMs` nothing is kept in memory and the file is never written. A stop between two writes of it loses the reads since the last one, so a month may look colder than it is and be frozen a little early.
  That costs a slower read, nothing else. A missing or unreadable file means "every month was just used".
* No format number changed: `tier` was always in the manifest, and the reader already coped without `mphf.idx`.

### From the C door

`erase`, `clean` and `freeze` are operations of the door (`hosi.h` did not change; see "From other languages"):

```
{:op :erase :args {:erasures [[:ids [1001 1002]]                           ; these entities
                              [:range 5000 9000]                           ; every id from the first to the last
                              [:between 1767225600000 1769904000000]       ; created in [from, to): ms since 1970-01-01T00:00Z
                              [:owned-by [:post/author :media/owner] [77]] ; ...and with a window:
                              [:owned-by-between [:post/author] [77] 1767225600000 1769904000000]]}}
  => {:ok {:generation 3 :dropped [...] :recorded [...] :retracted 12 :owned [1001 1002]}}
{:op :clean}                          ; or {:op :clean :args {:max-chunks 5}}
  => {:ok {:rewritten [{:chunk "…" :into "…"}] :skipped [] :records-left 0 :khana-cleaned true :khana-rebuilt false}}
{:op :freeze}                         ; or {:op :freeze :args {:max-units 2}}
  => {:ok {:frozen [{:kind :create :month "2026-09" :into "…"}] :skipped []}}
```

`open` takes two more keys in `:config`: `:cold-after-ms` and `:cold-block-events`. Without `:cold-after-ms`, `freeze` does nothing.
`:into nil` means that nothing of the chunk was left. The doors do not show the erase record of a chunk (it is a file of the world, as `ceFiles` is).

### Measured

One machine (macOS arm64, GHC 9.10.3 -O1, local APFS), one run each, so these are the *size* of each thing, not promises. Not measured: Linux, S3, a world with more than one month.

**The erase record** (ranges of ids; ids are real `makeId` values; a probe that is not part of the package):

| shape | file | bytes a range | build | encode | decode | `member` |
|---|---|---|---|---|---|---|
| 1,000 ids over a year (an ordinary account) | 7,958 B | 7.96 | 0.7 ms | 0.4 ms | 1.0 ms | 160 ns |
| 100,000 ids over a year (a very busy account) | 695 KB | 6.95 | 90 ms | 19 ms | 23 ms | 99 ns |
| 1,000,000 ids inside one month | 5.95 MB | 5.95 | 1.6 s | 217 ms | 246 ms | 121 ns |
| 1,000,000 ids, one for each millisecond (a burst) | 4.00 MB | 4.00 | 0.13 s | 177 ms | 231 ms | 69 ns |

So a record is small, and asking it costs nothing next to reading a block. The slow part is building a set from random ids (it sorts a Haskell list); it happens once, when `erase` writes the record.

**A made-up world through `Hosigraph.Events`** (no Datalevin; 50,000 entities, 4 events each, 100 owners, one month, a seal every 2,500 entities so 20 hot chunks; `sync` off, so the write time says nothing):

| step | result |
|---|---|
| the world as written | 20 hot chunks, 6.49 MB (`chunk.hosi` 5.70 MB, `mphf.idx` 0.65 MB, `filter.bin` 0.13 MB) |
| `history`, hot, warm | 37 µs an id |
| `ownedBy`, one owner of 100 (500 entities) | 73 ms, over 200,000 events in 20 chunks |
| `erase` of those 500 | 5 ms; every one of the 20 chunks gets a record; `history` stays 34 µs |
| `clean`'s rewrite, 20 chunks | 418 ms; 6.43 MB afterwards |
| `freeze` of the month | 404 ms; one cold chunk, 5.97 MB (`chunk.hosi` 5.85 MB, no `mphf.idx`, `filter.bin` 0.12 MB) |
| `history`, cold, warm | 87 µs an id (about 2.3 times the hot one); the same answers as before the freeze, and `verify` is clean |

What to take from it: `freeze` saved **about 7 %** here, not "half". Its saving is the size of `mphf.idx`, and that is large only when an entity has few events (4,000 entities with one event each, in the Phase 2 numbers:
the chunk without `mphf.idx` was about half the size). With 4 events an entity the events are most of the bytes and `mphf.idx` is 10 % of them. How much a real world saves depends on how many events an entity has.
The cold read is slower for each id because it reads a block of 4,096 events and not 256; it is still microseconds. A first `history` after a reopen, with nothing cached, took 2 ms on the cold chunk.
`ownedBy` is a scan: 73 ms for 200,000 events says about 0.4 µs an event, while writers wait; if it grows in a straight line, a world a hundred times bigger would hold writers for several seconds. The numbers for `clean`'s second half
(the khana made new: a compacted copy, or a rebuild) were not taken at this size; in the tests the khana is small.

### Still missing

* **S3.** A remote chunk that has an erase record must be downloaded, written again and uploaded, and the old one deleted, with its old versions, and a sweep of the bucket needs a grace period.
  `ObjectStore.delete` waits for this. The count of deletions still pending belongs in `CleanReport` and `Stats`. Where the upload happens (at seal, as [`TODO.md`](../TODO.md) item 5 says, or at freeze,
  which would save uploading a chunk that is soon replaced and deleted) is open. The local side is written so that the remote side plugs into `retire` and the rewrite.
* **Left out on purpose:** thaw, compression of cold chunks, merging cold chunks, a timer inside hosigraph, the other rules of freezing (the share of sequential reads, capacity pressure, free space),
  metrics and the capacity outlook (Phase 4), pruning horizons.
* **Not tested:** "the world changed between the read and the lock" in a rewrite or a freeze (it needs a second writer at an exact moment; the check reads right to me, but I have not seen it bite);
  stamping of use under real clocks and many readers at once; `clean` called from two threads at once; the Elixir binding (`hosigraph-ex`) has no wrapper for the three new calls yet.
* **Not decided:** which attributes mean "owned by" in sukhi (they are an argument of each call, so hosigraph needs no setting); a separate S3 object for each owner and a dictionary for the cold data
  (proposed in the plan, not built); how many chunks a world may have (below 10,000 chunks nothing was slower than 0.35 s in a probe; at 100,000 a seal took 1.2 to 1.7 s and the first `history` 14 s).

## From other languages (`setu/`, `hosigraph-abi/`)

hosigraph is a Haskell library. To use it from Go, Python, Elixir or C, the repository has two more packages next to `takka/`.
Neither touches `hosigraph` or `takka`, and `hosigraph`, `takka` and `setu` still have no C.

* **`setu/`** (working name; Pali for "bridge") writes the doors other languages use, from one description of an API (`Setu.Spec.Api`):
  the operations, the shape of each argument and answer (`Ty`), and the error kinds. It is a set of pure functions from the description to text,
  so the files it writes are committed and `--check` tells you when they are out of date. It also holds what every door shares:
  the table of waiting requests (`Setu.Ticket`) and the table of operations (`Setu.Dispatch`).
* **`hosigraph-abi/`** is the first customer. `Hosigraph.Abi.Api` describes hosigraph (`write`, `write-many`, `get-entities`, `query-many`,
  `query-cells-many`, `history`, `history-trace`, `seal`, `verify`, `stats`, `rebuild-khana`, `erase`, `clean`, `freeze`); `gen/` holds what setu writes from it:
  `include/hosi.h`, the Haskell `foreign export`s, and a small C file that starts the Haskell runtime and guards the calls.
  The shared library is `libhosi`.

`setu/` is made to be moved into a repository of its own (it has its own README, changelog, CI file and a `cabal.project.standalone`);
[`docs/split-setu.md`](split-setu.md) says how, what was tried, and what is still to be decided.

The C interface is thin and does not change when an operation is added: `hosi_runtime_init`, `hosi_open`, `hosi_submit`, `hosi_cancel`, `hosi_close`,
`hosi_runtime_shutdown`, `hosi_thread_done`, `hosi_abi_version`. A request is handed in with a number (an id the caller chooses) and a body, and is answered **once**,
through a callback, with a status (`HOSI_OK`, `HOSI_ERR_APP`, ...) and a body. Both bodies are UTF-8 EDN, `{:op :write :args {...}}` going in and
`{:ok ...}` or `{:error :write-rejected :message "..."}` coming back. The reserved operations `:ping` and `:abi` work on every connection, without a database.
`open` takes EDN too (`{:dir ".." :schema {..} :config {..}}`) and returns a handle at once; the database starts on a thread of its own, and a failure to start
is the answer of every request. A callback never runs on the thread that called `submit`, `cancel` or `close`, and `close` returns after all callbacks have returned.

```bash
cabal run hosigraph-abi-gen -- --out   hosigraph-abi/gen    # write the generated files
cabal run hosigraph-abi-gen -- --check hosigraph-abi/gen    # fail if they are out of date
cabal build hosigraph-abi:flib:hosi                          # libhosi
python3 -I hosigraph-abi/test/abi_conformance.py <path to libhosi>   # the rules, through ctypes, with a fake database
cabal test setu:spec hosigraph-abi:spec
```

`erase`, `clean` and `freeze` are in the description, so the door has them (see "Phase 3 so far"); the generated files did not change when they were added.
The ctypes conformance does not call them (it uses a fake database); the Haskell tests do, through the dispatcher, over a real hosigraph.

What has been seen working: the Haskell tests above, and the ctypes conformance on macOS (arm64, GHC 9.6.7 when it was first tried; the Haskell tests also pass on 9.10.3). What has not:

* The operations against a real database. `hosigraph-abi:spec` runs them when `HOSIGRAPH_DTLV` (or `HOSIGRAPH_DATALEVIN_JAR` and `HOSIGRAPH_JAVA`) is set,
  as in CI; without one they are skipped, with a line saying so.
* Linux. CI has a separate job for it (`abi`), allowed to fail until it has passed once.
* A host that is not Python. The runtime cannot be started again after `hosi_runtime_shutdown` (GHC's RTS refuses), and signal handlers are never installed
  (`--install-signal-handlers=no`), so Ctrl-C stays with the host.
* Shipping. The dylib names GHC's libraries by absolute path; a copy for someone else's machine needs them gathered with it.

## Not in Phase 1

* Phase 2 (the rest) and later: S3 and Range GET over a real network, the S3 side of Erase and freezing,
  metrics and capacity forecasts, anonymous metrics.
* Not done yet, though they belong to Phase 1: compression of chunk values,
  JSON-LD import (Phase 0 decides this part), and a dictionary for IRIs from outside the ID space.
* **Each chunk has a value dictionary** (chunk format version 2). A value that the chunk says many times is written once in the header,
  and the runs say "number 17" instead (2-3 bytes). A value goes in only when that makes the chunk smaller:
  a value of `len` bytes that appears `n` times is worth it when `n * (len - 4) > len`.
  Values are told apart by their bytes, not by `==`, so `0.0` and `-0.0` stay two values.
  The dictionary is read when a chunk is opened and stays in memory with the reader (it is part of what `hcReaderCacheBytes` counts).
  Chunks of version 1 (no dictionary) are still read; nothing rewrites them. The tail log's format did not change.
* **Version 2 also writes numbers short.** Every number in a run is a variable-length one (LEB128, 1-10 bytes): the attribute number, the seq
  (counted from the chunk's first seq), the entity id (counted from the chunk's first id), the lengths, and integer values (zigzag, so `-1` is short too).
  A text value's length is one byte, not four. Point lookups work as before: the slot still points at the start of a run, and the run's head is
  read with one read of at most 30 bytes. It is a new chunk format only; nothing else needed to change.
  On 20,000 made-up events (4,000 entities with a name, a type, an age, a language and a friend), `chunk.hosi` was 595,208 bytes in version 1
  and 205,466 in version 2, with no compressor (the dictionary alone took version 1 to 474,462; the short numbers did the rest).
  Invented data is only a hint: names made of a few repeated words, random friend ids.
  A general compressor on top does not make version 2 smaller than version 1 (xz -9: 72.6 KB vs 70.0 KB; zstd -19: 94.4 KB vs 91.1 KB),
  because it had been squeezing the same fixed-width zeros. `mphf.idx` was not touched (about 13 bytes for each id).
  The dictionary is per chunk; one for the whole khana, or one shared by IRIs from outside the ID space, is the item above.
* The open chunk readers (each holds the chunk's slot table, 12 bytes per id) have a memory limit: `hcReaderCacheBytes`, 64 MiB by default.
  The least recently used reader is closed first; the one just used is always kept, so a single huge chunk still opens.
  The count is about the slot tables only (`Events.xsReadersOpen`, `xsReaderBytes` show it).

## Notes on the current state

* **Speed is measured only roughly.** Each write fsyncs the tail log and the khana commits once, so one-by-one writes are slow. The design expects grouped writes.
* `Schema` is an argument of open. hosigraph does not store it. See item 6 of [`design-notes.md`](design-notes.md).
* Writes go one at a time (`MVar`). Reads run in parallel.
  `history` takes the lock for a moment to see the manifest and the tail log in a stable state, so a seal in progress is not visible.
* The MPHF is a minimal PTHash style (load factor about 89 %, about 4 keys for each bucket, 16-bit pilots).
  The choice between PtrHash, PTHash, and RecSplit is open.
* `takka/` exports the types (`Value`, `Schema`, …), EDN, and the Datalog and pull syntax (`Takka.Types`, `Takka.Edn`, `Takka.Syntax`).
  hosigraph needs these to write to Datalevin and to read its answers back.
* **There is no in-process khana any more.** An earlier version could keep the khana in a small Haskell engine on dlmdb (`KhanaDatalevinNative`).
  It was removed, so the C sources and that engine's tests are gone. They stay in the git history (commit `2aee1d7`).
  If Datalevin gets a native core of its own, it would be a new backend next to the two above.
* The license is the Blue Oak Model License 1.0.0 (see [`LICENSE`](../LICENSE) and [`dependency-licenses.md`](dependency-licenses.md)).

## Open questions for the design document

[`design-notes.md`](design-notes.md) gives the details. Each item says what the prototype does and what it asks of the design document.

1. **A chunk also needs a sequence number (seq).** The fold needs the order of Create and Delete, and which Create is newer.
2. **The "month" is the month in which the entity was created, not the month of the event.** A Delete or update of an old entity goes into an old month's chunk. How should this meet the freezing unit?
3. **A khana plus a tail log cannot be read together** (a Datalevin `Db` is one LMDB read transaction). Instead, hosigraph folds into the khana at each write (write-through).
4. **Who decides whether a write is allowed?** A type error or a unique-key conflict shows only when hosigraph folds into the khana.
5. **There are two fast paths.** "Now" comes from the khana. "The events of one ID" comes from the MPHF.
6. **Nobody stores the schema.** The cardinality sets the meaning of Create, so it changes the fold.
7. **The 63 bits of the ID have no room for a node ID.** Where does the room for distribution go?
8. **The parts have names that the design document does not have** (Events and Khana). Should the design document take these words?
