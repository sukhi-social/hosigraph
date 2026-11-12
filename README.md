# hosigraph ⭐

A small embedded database that **keeps the past as events**, and keeps **"now" as a copy you can throw away**.

This is the **Phase 1 prototype** of the [design document v0.2.0-draft](docs/hosiDB-design-v0.2.0-draft.md).
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
 write ──► events/tail.log ──seal──► events/chunks/c000001-create-2026-11/{chunk.hosi, mphf.idx}
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

After the MPHF (see "How the files are laid out") says "it is here", Probe asks the slot twice. It answers one of four things:

* `Star`: the key matches.
* `LookAlike`: the fingerprint matches by chance, but the key is different.
* `NotStar`: the fingerprint is different.
* `EmptySky`: the slot is empty.

## Get started

You need `ghc` (tested with 9.6.7), `gcc`, and the packages `aeson` and (for the tests) `QuickCheck`.

Everything is in this repository. [`takka/`](takka/README.md) is the Datalog engine that hosigraph uses
(types, EDN, the Datalog syntax, and the in-process engine). cabal builds it together with hosigraph.
Takka was called Hevin before; "takka" is Pali for reasoning and logic.

```sh
cabal build all
cabal run hosigraph-demo                 # a walk-through that moves a clock from November to December
cabal test hosigraph:spec takka:spec     # 41 + 76 tests; more with Datalevin (see below)
```

(`cabal test` alone runs only hosigraph's tests.)

Tested: GHC 9.6.7 with `cabal build all` and both test suites, on macOS (arm64). Linux is not tested here;
the GitHub Actions workflow ([`test.yml`](.github/workflows/test.yml)) runs it on ubuntu-24.04.

> **`scripts/build-ghc.sh`** builds with plain `ghc` and `gcc`, for machines where cabal cannot reach Hackage:
> `scripts/build-ghc.sh app` (or `test`). It writes to `build/`. Plain `ghc` must be able to see `aeson` (and `QuickCheck` for the tests).
> **It has not been run to the end yet.** An older version stopped at once because `$target` was never set; that is fixed,
> but on the Mac where this was tried, plain `ghc` could not see `aeson`. Use cabal until then.

### Run the tests against Datalevin

The tests also run against real Datalevin if you tell them where it is. If you do not, they print that they skipped those parts.

```sh
HOSIGRAPH_DTLV=/path/to/dtlv cabal test        # the native dtlv
HOSIGRAPH_DATALEVIN_JAR=/path/to/datalevin-1.2.10-standalone.jar HOSIGRAPH_JAVA=/path/to/java cabal test   # a resident JVM (JDK 21 or newer)
```

| You give | Tests that run |
|---|---|
| nothing | 41 (`KhanaDatalevinNative` only) |
| `dtlv` | 76 |
| `dtlv` and the JVM | 113 (including: write with the JVM and read with `dtlv`, and the reverse) |

The same scenarios pass on all three. The tests choose the khana themselves, so the 41 tests need no jar.

The demo uses the JVM when `HOSIGRAPH_DATALEVIN_JAR` (and `HOSIGRAPH_JAVA`) is set. Otherwise it uses `KhanaDatalevinNative`.

## Use it

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Exception (bracket)
import Hosigraph
import qualified Data.Map as M

-- KhanaDatalevinNative needs no jar. See "Where 'now' lives" for the others.
cfg :: HosiConfig
cfg = defaultConfig { hcKhanaBackend = KhanaDatalevinNative }

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

`withHosi dir schema` is a shorter way to open, but it uses `defaultConfig`, which wants a Datalevin jar and `java` (see below).

| Call | What it does |
|---|---|
| `write` | Create and Delete operations in one call. **All of them go in, or none.** |
| `writeMany` | Many batches with one append, one fsync, and one khana transaction. |
| `getEntity`, `query`, `queryCells` | Read **now**. |
| `getEntities`, `queryMany`, `queryCellsMany` | Read **now**, many questions at once. |
| `history`, `historyTrace` | Read **the past**: every event of one ID, from the chunks and the tail log. `historyTrace` also shows which chunks it skipped and what each probe answered. |
| `seal` | Turn the tail log into chunks. `HosiConfig` can also seal by itself, by size or by age. |
| `verify` | Check the manifest signature, the chunk hashes, the footers, and the MPHF lookup of every ID. It also checks that **the khana equals a khana built again from the events.** An empty list means all good. |
| `rebuildKhana` | Throw the khana away and fold the events again. |
| `withKhana` | Run code on one snapshot of the in-process engine (only with `KhanaDatalevinNative`). |

You can also use `Hosigraph.Events` alone, without a khana. The tests cover this.

## Where "now" lives

The events are the same everywhere. Only the place of the khana changes. There are three:

| | `KhanaDatalevinJvm java jar` (default) | `KhanaDtlv path` | `KhanaDatalevinNative` |
|---|---|---|---|
| What it is | Datalevin in **one JVM that stays up** (one JVM for each `Hosi`) | Datalevin through the `dtlv` command, **one process for each operation** | A small Haskell engine inside your process. **Not Datalevin.** |
| One operation | about 0.6 to 0.9 ms (the first call: 1.3 to 1.6 s to start) | about 25 ms (the first call: about 0.4 s) | about 0.1 ms or less |
| What you need | JDK 21 or newer, and the jar (about 93 MB) | one `dtlv` file | nothing |
| `(max ?x)` and `(min ?x)` | work | **do not work** (see "Limits") | work |
| Files in | `khana-dtlv/` | `khana-dtlv/` | `khana/` |

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

| | `KhanaDatalevinNative` | `KhanaDatalevinJvm` | `KhanaDtlv` |
|---|---|---|---|
| `write`, one item at a time | 0.12 ms | 0.9 ms | about 25 ms |
| the same items in one batch (for each item) | 0.01 ms | 0.05 ms | 1.2 ms |
| `getEntity`, one at a time | almost 0 | 0.6 ms | about 23 ms |
| `getEntities`, 200 together | 0.09 ms | about 3 ms | about 23 ms |
| 6 queries (one by one → `queryMany`) | 1.2 → 1.3 ms | 2.6 → 1.9 ms | 156 → 28 ms |
| `stats` | almost 0 | 1 ms | about 24 ms |

* **Grouping helps `KhanaDtlv` most**, because starting a process is almost all of its time.
  A JVM call is cheap already (200 entities: about 40 times faster; 6 queries: about 1.4 times).
  `KhanaDatalevinNative` runs in your process, so it does not change.
* "The same items in one batch" is the **upper limit** for grouped writes. `writeMany` (separate batches) comes close to it.
* The fsync of the tail log makes almost no difference on this Mac (under 0.1 ms). hosigraph calls only `fsync(2)`,
  and on macOS that does not wait for the disk write cache. On systems that really wait, the time is higher. It is not measured.

## Limits

* `withKhana` works only with `KhanaDatalevinNative`. With a Datalevin khana it fails with `BadArgument`. Use `query` or `queryCells`.
* `query` does not accept `InRules`, because a `Rule` is a structure and not EDN text. Write the rules inside the query.
* **`KhanaDtlv` cannot use `(max ?x)` or `(min ?x)`.** In `dtlv exec` 1.2.10, they return the collection of values, not the maximum or the minimum.
  They seem to resolve to `clojure.core/max` and `min`. `count`, `sum`, and `avg` are correct, and the same jar on a JVM is correct too.
  So the problem is in `exec` of the native `dtlv`. hosigraph returns an error and does not read the result.
  A canary test tells you when upstream fixes it.
* The query parser refuses aggregates such as `distinct` or `(max 2 ?x)` before they reach Datalevin, so no backend can send them.
* NaN and ±Infinity have no EDN form, so hosigraph refuses to write them. (`KhanaDatalevinNative` is not checked.)
* The order of rows is not guaranteed (a Datalevin answer is a set). Sort the rows before you compare them.
* Tested only with Datalevin 1.2.10 on macOS arm64 (JVM: Temurin 21). The memory use of the resident JVM is not measured.

## How the files are laid out

| Word in the design document | Form here |
|---|---|
| Tail log | `events/tail.log`. One `write` is one record (length + CRC32). If a record is cut, hosigraph keeps what is before the cut and removes the rest. |
| Hot chunk | `chunk.hosi` and `mphf.idx` in `events/chunks/<name>/`. One chunk for each kind and (creation) month. Nobody changes it. |
| ID | `(milliseconds since 2026-01-01) << 21 \| sequence number` (63 bits). The ID alone tells you the month. |
| MPHF | A *minimal perfect hash*: it gives each known key its own slot, with no collisions. This one is PTHash-style and written for this project (`Hosigraph.Mphf`). Each slot has a 32-bit fingerprint and the position of the body, which `Probe` checks. |
| Manifest | `events/manifest`. Line 1 is JSON. Line 2 is `sha256:`. hosigraph writes a tmp file and replaces the manifest by a rename. A chunk that is not in the manifest is outside the world; hosigraph cleans it at open. |
| Core material | `khana/` or `khana-dtlv/` (`Hosigraph.Khana`). Create becomes `Add`, Delete becomes `Retract`. The folded position (`folded-seq`) is in the same transaction. |
| Update | None. For an attribute with cardinality one, a newer Create wins. |
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

## Not in Phase 1

* Phase 2 and later: S3, deep paths, Binary Fuse filters (planned as the first gate of `Probe`), Range GET, freezing (cold chunks),
  Erase and invalidation records, garbage collection, metrics and capacity forecasts, anonymous metrics.
* Not done yet, though they belong to Phase 1: a dictionary and compression of chunk values (only attribute names have a dictionary),
  JSON-LD import (Phase 0 decides this part), a size limit for the chunk read cache, and a dictionary for IRIs from outside the ID space.

## Notes on the current state

* **Speed is measured only roughly.** Each write fsyncs the tail log and the khana commits once, so one-by-one writes are slow. The design expects grouped writes.
* `Schema` is an argument of open. hosigraph does not store it. See item 6 of [`docs/design-notes.md`](docs/design-notes.md).
* `hcKhanaNoSync` (no fsync in the khana) exists, but nobody tested a power failure. It works with `KhanaDatalevinNative` only.
* Writes go one at a time (`MVar`). Reads run in parallel.
  `history` takes the lock for a moment to see the manifest and the tail log in a stable state, so a seal in progress is not visible.
* The MPHF is a minimal PTHash style (load factor about 89 %, about 4 keys for each bucket, 16-bit pilots).
  The choice between PtrHash, PTHash, and RecSplit is open.
* `takka/` exports the types (`Value`, `Schema`, …), EDN, and the Datalog syntax (`Takka.Types`, `Takka.Edn`, `Takka.Query`, `Takka.Pull`).
  The one place that knows the engine inside is `Takka.Engine`.
  There is no cabal flag to remove the engine: hosigraph needs the types and the syntax even when the khana is Datalevin.
* **About the name `KhanaDatalevinNative`.** It is the slot for the native core of Datalevin, which does not exist yet.
  The engine in the slot today is takka (a small Haskell engine on top of dlmdb). It is not Datalevin.
  The tests use it as the oracle (the reference answer).
* The license is **not set**.

## Open questions for the design document

[`docs/design-notes.md`](docs/design-notes.md) gives the details. Each item says what the prototype does and what it asks of the design document.

1. **A chunk also needs a sequence number (seq).** The fold needs the order of Create and Delete, and which Create is newer.
2. **The "month" is the month in which the entity was created, not the month of the event.** A Delete or update of an old entity goes into an old month's chunk. How should this meet the freezing unit?
3. **A khana plus a tail log cannot be read together** with `KhanaDatalevinNative` (one LMDB read transaction). Instead, hosigraph folds into the khana at each write (write-through).
4. **Who decides whether a write is allowed?** A type error or a unique-key conflict shows only when hosigraph folds into the khana.
5. **There are two fast paths.** "Now" comes from the khana. "The events of one ID" comes from the MPHF.
6. **Nobody stores the schema.** The cardinality sets the meaning of Create, so it changes the fold.
7. **The 63 bits of the ID have no room for a node ID.** Where does the room for distribution go?
8. **The parts have names that the design document does not have** (Events and Khana). Should the design document take these words?
