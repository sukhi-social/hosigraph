# hosigraph ⭐ prototype (Phase 1)

Old name: **hosiDB**. This prototype is **Phase 1** of the [design document v0.2.0-draft](docs/hosiDB-design-v0.2.0-draft.md).
The design document keeps the name hosiDB. The code is in Haskell.

hosigraph keeps the past as events. It keeps "now" in a **khana**. A khana is a Datalog database that the system can throw away and build again from the events.

Datalevin holds the khana. The default is the Datalevin standalone jar in a resident JVM.

> **About `KhanaDatalevinNative`.** This name is the slot for the native core of Datalevin. That core does not exist yet.
> **The engine behind the slot is not Datalevin.** It is a small Haskell engine on top of dlmdb, in a separate package.
> hosigraph sees only the bridge package `native-bridge`.
> The engine needs no jar and no `java`. The tests use it as the oracle (the reference answer).

> The goal of Phase 1: "write, seal, restart, and get the same answers".
> The tests stop the system in 5 places and check the result.
> They also test a khana that is behind the events and a khana that is ahead of them.
> The suite has 41 tests with `KhanaDatalevinNative` only, 76 with the native `dtlv`, and 113 with the resident JVM.

## About the names

* **hosi** (星, "star") is one fact. **graph** is the links between facts (subject, predicate, object).
* **Khana** (刹那, setsuna) is Pali for "a moment" (Sanskrit: kṣaṇa). In hosigraph, it is **"now"**.
  The system folds the events into one moment. You can throw the moment away.
  The events build it again. The design document calls it the **core material** ("芯材").
* **Events** is the part that manages the events: the tail log, the chunks, the manifest, and the lookup of the events of one ID.
  **The events are the truth.** The name is a placeholder. `Sky` was another candidate.
* **Probe** is the check for a point lookup. The name is a pun: `Hosi` and `Khana` together sound like "hoshi kana" ("is it a star?").
  After the MPHF says "it is here", Probe asks the slot twice. The answers are:
  `NotStar` (the fingerprint is different), `LookAlike` (the fingerprint matches by chance, but the key is different), `Star` (the key matches), and `EmptySky` (the slot is empty).

## Try it in 5 minutes

```sh
scripts/build-ghc.sh app  && build/hosigraph-demo     # a walk-through that moves a clock from November to December
scripts/build-ghc.sh test && build/hosigraph-test     # the tests (with QuickCheck; the counts are below)
```

The build needs `ghc`, `gcc`, the packages that come with GHC, and `aeson`. The tests also need `QuickCheck`.

**You must give the location of `native-bridge` and the engine that it wraps.**
hosigraph does not assume where they are:

```sh
cp cabal.project.local.example cabal.project.local   # write the two paths in "packages:"
BRIDGE_DIR=/path/to/native-bridge ENGINE_DIR=/path/to/the-engine scripts/build-ghc.sh app   # without cabal, use these variables
```

Without the paths, cabal says that it cannot find `native-bridge`.
The script tells you what to set and stops.

Tested: GHC 9.6.7 with `cabal build all` and `cabal test` (macOS, arm64).
Not tested with this layout: Linux, and `scripts/build-ghc.sh` to the end (see the notes below).

The code needs `DeriveFunctor` and `FlexibleContexts`. `hosigraph.cabal` enables them.

### Test with dtlv (Datalevin)

To test a Datalevin khana (see "Keep "now" in Datalevin" below), give the `dtlv` executable or the Datalevin standalone jar in environment variables.
The tests run when you give them. If you do not, the tests print that they skipped them.

```sh
HOSIGRAPH_DTLV=/path/to/dtlv cabal test        # the native dtlv (or build/hosigraph-test)
HOSIGRAPH_DATALEVIN_JAR=/path/to/datalevin-1.2.10-standalone.jar HOSIGRAPH_JAVA=/path/to/java cabal test   # the resident JVM (JDK 21 or newer)
```

Without variables, 41 tests run (`KhanaDatalevinNative` only). With `dtlv`, 76 tests run. With the JVM also, 113 tests run.
The same scenarios pass on `KhanaDatalevinNative`, on `dtlv`, and on the JVM.
The tests do not use `defaultConfig`. They choose the khana themselves, so the 41 tests run without a jar.
When you give both `dtlv` and the JVM, more tests run: write with the JVM and read with `dtlv`, and the reverse.

## Use it

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Hosigraph
import qualified Data.Map as M

main :: IO ()
main = withHosi "/tmp/hosi" schema $ \h -> do
  r <- write h [ Create (Tmp 1) "person/name" (Lit (VStr "Frege"))      -- Tmp 1 becomes a real ID with a time in it
               , Create (Tmp 1) "person/age"  (Lit (VInt 70)) ]
  let frege = wrTemps r M.! 1
  getEntity h frege >>= print                -- now (from the khana)
  query h "[:find ?n :where [?e :person/name ?n]]" [] >>= print    -- Datalevin Datalog, as it is
  _ <- seal h                                -- seal the tail log into a frozen chunk
  history h frege >>= mapM_ print            -- the past (from the events, by MPHF point lookup)
  where schema = M.fromList [("person/name", identity (attr TString)), ("person/age", attr TLong)]
```

* `write`: a list of Create and Delete operations in one call. All of them go in, or none.
* `writeMany`: many batches with one append, one fsync, and one khana transaction (see "Write in groups").
* `getEntity`, `query`, `queryCells`, `withKhana`: **now**. They ask the khana.
* `getEntities`, `queryMany`, `queryCellsMany`: ask **now** in groups (see "Ask in groups").
* `history`, `historyTrace`: the **past**. They collect the events of one ID from the chunks and the tail log.
  `historyTrace` also tells you which chunks it skipped by month, which chunks it probed, and what each probe answered.
* `seal`: turn the tail log into a chunk. `HosiConfig` can also seal automatically, by size or by age.
* `verify`: check the manifest signature, the chunk hashes, the footers, and the MPHF lookup of every ID.
  It also checks that **the khana is the same as a khana that it builds again from the events**.
* `rebuildKhana`: throw the khana away and fold the events again.

There are two parts. You can use `Hosigraph.Events` alone, without a khana (the tests cover this).

## Keep "now" in Datalevin

A khana has three places to live:
* a **resident JVM** that runs the Datalevin standalone jar (the default),
* the `dtlv` command,
* the in-process engine `KhanaDatalevinNative` (it is not Datalevin; see the note above).

The events are the same for all of them.
Write, seal, reopen, and `verify` work the same with every khana.

```haskell
cfg :: HosiConfig
cfg = defaultConfig { hcKhanaBackend = KhanaDtlv "/usr/local/bin/dtlv" }
-- The default is KhanaDatalevinJvm "java" "datalevin-standalone.jar".
-- Put the jar there or set the path yourself.
-- If the jar is missing, openHosi touches nothing and stops with BadArgument.
-- To try without a jar: hcKhanaBackend = KhanaDatalevinNative

main = bracket (openHosiWith cfg "/tmp/hosi" schema) closeHosi $ \h -> do
  ...   -- write / getEntity / query / seal / history / verify work as before
```

```haskell
cfg = defaultConfig { hcKhanaBackend = KhanaDatalevinJvm "java" "/path/to/datalevin-1.2.10-standalone.jar" }
```

If `dtlv` (or `java`, or the jar) is not there, hosigraph stops with `BadArgument` before it touches anything.

| | `KhanaDtlv path` | `KhanaDatalevinJvm java jar` |
|---|---|---|
| How it runs | one `dtlv exec` process for each operation | one JVM that stays up (one for each `Hosi`, shared by all khanas) |
| Time for one operation (measured, below) | about 25 ms (the first call: about 0.4 s) | about 0.6 to 0.9 ms (the first call only: 1.3 to 1.6 s to start) |
| What you need | one `dtlv` file | JDK 21 or newer, and the jar (about 93 MB) |
| `(max ?x)` and `(min ?x)` | not usable (see below) | usable |

* `KhanaDatalevinNative` keeps its files in `khana/`. The two Datalevin khanas keep their files in `khana-dtlv/`.
  The directories are different. To switch, you only fold the events into the new khana (the tests do this in both directions).
  The two Datalevin khanas read the same files (the tests write with the JVM and read with `dtlv`, and the reverse).
  **Do not open them at the same time.** The LMDB lock is not checked.
* `KhanaDtlv` starts `dtlv exec` **once for each operation** and sends the code on standard input (`Hosigraph.Khana.Dtlv`).
  A write is one `transact!`, so "all or none" holds. No process stays up.
* `KhanaDatalevinJvm` starts one JVM and keeps it. It talks to the JVM one line at a time (`Hosigraph.Khana.Sidecar`).
  `closeHosi` ends it. If your process ends without `closeHosi`, the JVM also ends.
  If the JVM dies, that one call fails (for a write: `WriteRejected`), and the next call starts a new JVM.
  The khana can be built again from the events, so nothing is lost.
* The schema maps to the Datalevin schema (`:db/valueType`, `:db/cardinality`, `:db/unique`). `TAny` has no type.
* hosigraph sends your EDN Datalog to Datalevin as it is. The answer has the same shape as `KhanaDatalevinNative` (a list of rows).
  It also accepts `pull` in `queryCells`.

### Ask in groups (fewer round trips)

`KhanaDtlv` takes about 25 ms for each operation (one process). `KhanaDatalevinJvm` also makes one round trip for each call.
To ask many questions, ask them in one call. `KhanaDatalevinNative` has the same calls and answers from one snapshot.

```haskell
es <- getEntities h [a, b, c]                                  -- in the order you ask: [Map attribute [values]]
rs <- queryMany h [(q1, []), (q2, [InScalar (VInt 70)])]       -- in the order you ask: [rows]
cs <- queryCellsMany h [(pullQ, []), (q1, [])]                 -- pull is also accepted
```

* All answers are from **the same moment**. No other write comes between them. If one question fails, the whole call fails and returns nothing.
  hosigraph refuses a query that the query parser (`Native.Query`) refuses, and it refuses `InRules`, **before it sends anything**.
* `dtlv` starts one process, however many questions you ask (the tests count this: 6 entities or 6 queries, one process).
  A list of 3000 IDs travels as data, not as code in the text, so the JVM does not refuse to compile it.
* Inside, hosigraph also groups work where it can:
  `openHosi` catch-up puts **all** unfolded tail-log batches in **one transaction**
  (if the khana refuses, it goes back to one by one and drops only the refused batch).
  It reads `foldedSeq` once, not twice. `stats` makes one query, not two.
  A catch-up of 5 batches starts 3 `dtlv` processes, not 8.
  The JVM link sends `Text`, not `String`, and it does not re-read big answers.

### Write in groups

```haskell
rs <- writeMany h [ batch1, batch2, batch3 ]     -- the answers are in order: [Either HosiError WriteReport]
```

`writeMany` gives the same result as one `write` for each batch.
It uses one append, one fsync, and one khana transaction (for `dtlv`: one process; for the JVM: one round trip).
The batches stay separate. Each batch gets its own IDs for its `Tmp` names. This is different from one big batch made with `concat`.

* An invalid batch (empty, or the reserved ID 1) becomes `Left (BadArgument …)` in its place. The other batches go in.
* If the khana refuses, hosigraph first cuts all batches from the log. Then it writes them one by one, as `write` does.
  The good batches go in. Only the refused batches become `Left (WriteRejected …)`.
  If only one batch is valid, hosigraph does not retry.
  The seq numbers of a refused batch are not used. The next batch takes them.
* If a seal is due, hosigraph seals once, after the last batch. Only that `WriteReport` has `wrSealed`.
* If the system stops in the middle, the log keeps **the first batches** (or none). It never keeps half of a batch.
  Nobody got a reply, so reopen works as for `write`.
* Implicit grouping (joining `write` calls from many threads) does not exist. It needs a use case with many writers.

**Measurements** (macOS, arm64). One item is a `write` of 2 facts: a name and an age. The benchmark is not part of the project.

| | `KhanaDatalevinNative` | `KhanaDatalevinJvm` | `KhanaDtlv` |
|---|---|---|---|
| `write`, one item at a time | 0.12 ms | 0.9 ms | about 25 ms |
| the same items in one batch (for each item) | 0.01 ms | 0.05 ms | 1.2 ms |
| `getEntity`, one at a time | almost 0 | 0.6 ms | about 23 ms |
| `getEntities`, 200 together | 0.09 ms | about 3 ms | about 23 ms |
| 6 queries (one by one → `queryMany`) | 1.2 → 1.3 ms | 2.6 → 1.9 ms | 156 → 28 ms |
| `stats` | almost 0 | 1 ms | about 24 ms |

* The JVM takes 1.3 to 1.6 s to start (the first call only). The first `dtlv` call takes about 0.4 s. Later calls take about 25 ms.
* **Grouping helps `KhanaDtlv` most**, because starting the process is almost all of the time. A JVM call is cheap already:
  200 entities are about 40 times faster, and 6 queries are about 1.4 times faster.
  `KhanaDatalevinNative` runs in the same process, so it does not change.
* The fsync of the tail log has almost no effect on this Mac (the difference between fsync on and off is under 0.1 ms).
  hosigraph calls only `fsync(2)`. On macOS, this call does not wait for the disk write cache.
  On systems that really wait, the time is higher. It is not measured.
* "The same items in one batch" is an **upper limit** for grouped writes (one fsync, one transaction, one round trip).
  With `writeMany` (separate batches), each item takes 0.01 ms on `KhanaDatalevinNative`, 0.05 ms on the JVM, and 1.2 ms on `dtlv`. This is almost the same as the limit.

**Limits**

* `withKhana` gives the `Db` of the in-process engine, so it works only with `KhanaDatalevinNative`.
  With a Datalevin khana, it fails with `BadArgument`. Use `query` or `queryCells`.
* `query` does not accept `InRules` (`Rule`), because a `Rule` is a structure and not EDN text. Write rules inside the query.
* **`KhanaDtlv` cannot use `(max ?x)` or `(min ?x)`.** (`KhanaDatalevinJvm` can.)
  In `dtlv exec` 1.2.10, the aggregates `max` and `min` return the collection of values, not the maximum or the minimum.
  They look like they resolve to `clojure.core/max`. `count`, `sum`, and `avg` are correct.
  The same 1.2.10 standalone jar on a JVM gives correct `max`, `min`, and `distinct`.
  So the problem is only in `exec` of the native `dtlv`. It occurs for aggregates whose names are the same as names in `clojure.core`.
  hosigraph returns an error and does not read the result. A canary test tells you when upstream fixes it.
  The query parser (`Native.Query`) refuses aggregates such as `distinct` or `(max 2 ?x)` before they reach Datalevin, so no backend can send them.
* NaN and ±Infinity have no EDN form, so hosigraph refuses to write them. (`KhanaDatalevinNative` is not checked.)
* The order of rows is not guaranteed (a Datalevin answer is a set). Sort the rows before you compare them.
* Only Datalevin 1.2.10 on macOS arm64 (JVM: Temurin 21) is tested. The memory of the resident JVM is not measured.

## Structure

```
 write ──► events/tail.log ──seal──► events/chunks/c000001-create-2026-11/{chunk.hosi, mphf.idx}
                │                                  │      (events/manifest lists the chunks)
                └───────────── fold ───────────────┴──────────► khana/  (now, Datalog)
```

| Word in the design document | Form here |
|---|---|
| Tail log | `events/tail.log`. One `write` is one record (length + CRC32). If a record is cut, hosigraph keeps what is before the cut and removes the rest. |
| Hot chunk | `chunk.hosi` and `mphf.idx` in `events/chunks/<name>/`. One chunk for each kind and (creation) month. Nobody changes it. |
| ID | `(milliseconds since 2026-01-01) << 21 \| sequence number` (63 bits). The ID alone tells you the month. |
| MPHF and key check | A PTHash-style MPHF written for this project (`Hosigraph.Mphf`), and `Hosigraph.Probe` ("is it a star?"). Each slot has a 32-bit fingerprint and the position of the body. |
| manifest | `events/manifest`. Line 1 is JSON. Line 2 is `sha256:`. hosigraph writes to a tmp file and replaces the manifest with a rename. A chunk that is not in the manifest is outside the world (hosigraph cleans it when it opens). |
| Core material | `khana/` (`Hosigraph.Khana`). A Datalevin database (or `KhanaDatalevinNative`). Create becomes `Add`. Delete becomes `Retract`. The folded position (`folded-seq`) is in the same transaction. |
| Update | None. For an attribute with cardinality one, a new Create wins (the first idea of the design document). |
| Zone map | The manifest keeps the month, the ID range, and the seq range of each chunk. `historyTrace` shows how many chunks it skipped. |

The names of the file formats (`HOSICHK1`, `hosi-manifest/1`, `hosi.meta/folded-seq`) keep `hosi`.

### When the system stops (what `openHosi` repairs)

| Where it stopped | What happens at reopen |
|---|---|
| While it wrote the tail log | hosigraph cuts the half record. If the khana saw it, hosigraph builds the khana again. |
| After the tail-log write, before the fold into the khana | hosigraph folds at reopen. If the khana refuses, hosigraph drops that record. |
| While it wrote a chunk | hosigraph cleans the chunk that is not in the manifest. The tail log stays as it is. |
| After the manifest swap, before the tail log is emptied | hosigraph ignores records with a sealed seq. Nothing is doubled. |
| While it wrote `manifest.tmp` | hosigraph removes the tmp file. The old world stays. |

## Not in Phase 1

Phase 2 and later: S3, deep paths, Binary Fuse filters (planned as the first gate of `Probe`), Range GET, freezing (cold chunks), Erase and invalidation records, garbage collection, metrics and capacity forecasts, anonymous metrics.
Also in Phase 1: a dictionary and compression of chunk values (only the attribute names have a dictionary), JSON-LD import (a part that Phase 0 decides), a size limit for the chunk read cache, and a dictionary for IRIs that come from outside the ID space.

## Notes on the current state

* **Speed is measured only roughly.** Each write fsyncs the tail log, and the khana commits once, so one-by-one writes are slow. The design expects grouped writes.
* `Schema` is an argument of open. hosigraph does not store it. See item 6 of [`docs/design-notes.md`](docs/design-notes.md).
* `hcKhanaNoSync` (no fsync in the khana) exists, but nobody tested a power failure. It is a setting of `KhanaDatalevinNative` only.
* Writes go one at a time (`MVar`). Reads run in parallel. `history` takes the lock for a moment to see the manifest and the tail log in a stable state, so a seal in progress is not visible.
* The MPHF is a minimal PTHash style (load factor about 89 %, about 4 keys for each bucket, 16-bit pilots). The choice between PtrHash, PTHash, and RecSplit is open.
* The small package `native-bridge` (outside hosigraph) exports the types (`Value`, `Schema`, …), EDN, and the Datalog syntax (`Native.Types`, `Native.Edn`, `Native.Query`), and the engine interface (`Native.Engine`).
  hosigraph knows only this bridge.
  There is no cabal flag to remove the engine. The types and the syntax come from the same package, so a flag would not remove the dependency.
* The default is a resident JVM, so `openHosi` with `defaultConfig` needs a jar and `java`.
  The demo (`hosigraph-demo`) uses the JVM when `HOSIGRAPH_DATALEVIN_JAR` (and `HOSIGRAPH_JAVA`) is set. Otherwise it uses `KhanaDatalevinNative`.
* `scripts/build-ghc.sh` takes the bridge and the engine from `BRIDGE_DIR` and `ENGINE_DIR`.
  It stops when the plain `ghc` has no `aeson`. **It did not run to the end on the test Mac.** The cabal build works.
* The license is **not set**.

## Open questions for the design document

[`docs/design-notes.md`](docs/design-notes.md) gives the details. Each item says what the prototype does and asks what the design document wants.

1. **A chunk also needs a sequence number (seq).** The fold needs the order of Create and Delete, and the order of old and new Creates.
2. **The "month" is the month in which the entity was created, not the month of the event.** A Delete or update of an old entity goes into an old month's chunk. How should this meet the freezing unit?
3. **A khana plus a tail log cannot be read together** with `KhanaDatalevinNative` (one LMDB read transaction). Instead, hosigraph folds into the khana at each write (write-through).
4. **Who decides whether a write is allowed?** A type error or a unique-key conflict shows only when hosigraph folds into the khana.
5. **There are two fast paths.** "Now" comes from the khana. "The events of one ID" comes from the MPHF.
6. **Nobody stores the schema.** The cardinality sets the meaning of Create, so it changes the fold.
7. **The 63 bits of the ID have no room for a node ID.** Where does the room for distribution go?
8. **The parts have names that the design document does not have.** The part that manages events (Events) and "now" (Khana). Should the design document take these words?
