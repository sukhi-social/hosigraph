# TODO

## 0.1.0: the first version you can keep data in (draft)

**Status:** a draft of 2026-11-16 (Shiro, after a question from nyanrus). Nothing here is decided except where it says so.
The package is `0.0.1.0` and calls itself "Phase 1 prototype". In the PVP the next major is `0.1.0.0`; the number is cheap, what it promises is not.

**What 0.1.0 would say:** hosigraph is an embedded database for **one process on one machine, on a local disk**. What you write is kept.
What 0.1.0 wrote, any 0.1.x reads. The names it lists as public stay; the others may change.
It brings Datalevin along (the `dtlv` program, or JDK 21 and the standalone jar), and says so on the first screen of the README.

### Needed

1. **Refuse a second open of the same world.** *Done on 2026-11-16. With both khanas (`dtlv` and the JVM, Datalevin 1.2.10, Temurin 21): 180 passed, 0 failed; the C door's conformance script passes.*
   * *Tried first* (events only, two processes, one directory, both writing), and it does break. 300 writes each, no seal: both were told that all went well, and **300 of the 600 records were left**
     (each process appends at the offset it remembers, so they overwrite each other); `verify` passed. With a seal every 20 writes: one process died on `rename manifest.tmp` (both use the same temp name),
     the other went on, and 200 events were left. (The probe is not in the repository.)
   * *Done:* `Hosigraph.WorldLock`: `flock(2)` (`LOCK_EX | LOCK_NB`) on `events/lock`, close-on-exec (so the `java` or `dtlv` that hosigraph starts does not keep it alive). `openEvents` takes it first
     and `closeEvents` lets it go last; a second opener, in this process or another, gets `WorldBusy` and nothing is touched (a test checks that a chunk directory the manifest does not list is not swept).
     `openHosiWith` now opens the events *before* it starts the khana runtime, so a second opener never starts a JVM, and `closeHosi` closes the khana and the runtime *before* the events, so nobody opens on top of an open khana.
   * *Tests* (each was seen to fail with the lock turned off): the same process; another process holds it, and is killed with SIGTERM, so it is free again; an open that fails after the lock does not keep it.
   * *Decided (nyanrus, 2026-11-16):* **0.1 has one opener, and no shared lock for readers.** A second process that only wants to read is refused too (`WorldBusy`), and the guide says so. The shared lock is in "Would help" below.
   * *Also run:* the Hosi path (the new order of open and close) with each khana: a test opens a world twice through `openHosiWith`, and the second is `WorldBusy` with nothing under the world touched, khana files included.
     "A second opener never starts a JVM" is by the order of the code (the lock comes before `newKhanaRuntime`); no test proves it.
   * *Not done:* the Elixir binding's own tests (`hosigraph-ex`, `mix test`) were not run. Network file systems and Windows are out of scope; the guide says so.
2. **The shape on disk is a promise, and a test holds it.** *Done on 2026-11-16, except that the tail has no mark (and why).*
   * Formats that exist: `hosi-manifest/1` and `/2`, `HOSICHK1` (chunk version 1 and 2), the filter and the MPHF (version 1), `HOSIERS1`, the tail log. The khana is derived and promised nothing (a different one is simply made again).
   * *Looked at first:* what each reader does with a version it does not know. The manifest, the chunk, the MPHF, the filter and the erase record all refuse it (`Corrupt`). **The tail log did not.** `readTail` reads a first record it cannot understand as a torn end,
     and `openHeld` cuts the file to the good part (`resetTail`): so a world written by a *newer* build would have lost its unsealed writes at the first open by an older one. (Read from the code. The cut itself was not run; with the check turned off, the test below fails on the refusal, before it looks at the file.)
   * *The mark: not added.* A header would change the file, and a 0.1.0 build could no longer be put under a world that a later 0.1.x had written. What is needed is that a later format can *tell*, and that 0.1.x refuses it. The first four bytes of a tail are the length of the first record, big-endian;
     `HOSI` as a length is 1.1 GiB, and a record is one batch. So **`HOSI` is reserved** (`Hosigraph.Tail`): `readTail` refuses a file that begins with it (`Corrupt`, nothing touched), and a later format may begin with any mark that starts with it. Nothing on disk changes.
     If you want a mark anyway (say, to tell a tail from a file that is not one), it can still be added in a later 0.1.x in this way; say so.
   * *The fixture:* `test/fixtures/world-0.1.0/`, made by `spec --make-fixture DIR` at the events layer (no khana program needed): October's Create chunk written again as **version 1** (what 0.0.1.0 wrote) beside version 2 chunks, a Delete chunk, a **cold** chunk (November), an **erase record** (so the manifest is `/2`), and a tail that is not sealed. 15 events, 6 people, one erased.
     `expected.txt` has the events as read back, and, written by hand in the maker, what "now" must say for each person.
   * *Tests* (3 without a khana, 1 for each khana): the world reads back as `expected.txt` says and `verify`s; the fixture holds what it claims (both chunk versions, a cold chunk without `mphf.idx`, an erase record and its file, a tail); a tail that begins with `HOSI` and a manifest of format 9 are refused (`Corrupt`) and every file is as it was;
     with each khana, `Hosi` opens the fixture, makes the khana, answers for each person as written, takes a new write and a seal on top, and says the same after a reopen.
     *Seen to fail:* with `HOSI` not reserved, the refusal test fails; with the erase magic changed to `HOSIERS2`, the read-back test fails with `not an erase record (wrong magic)`.
   * *Not tried:* a world written by a real 0.0.1.0. Its chunks are version 1 (the fixture has them) and its tail has the same shape; it was not opened with one.
   * *Found, and changed on 2026-11-16 (nyanrus: "fix it"):* a record that is damaged in the **middle** of the tail (a flipped bit, not a torn end) was handled as a torn end: everything after it was cut, and the batches after it had been told to their writers as kept.
     Now `readTail` looks, after the first record it cannot read, for **any whole record later in the file** (length, CRC and payload all fit, at any offset). If there is one, it was not a crash: `Corrupt`, with the byte where the bad record begins and the byte where the whole one is; nothing is cut, and the lock is let go.
     The way out is the operator's: keep a copy of `tail.log` and cut it at the byte the message names (what follows is lost). Nothing is added to the API for that.
     *What is not told apart:* a damaged **last** record (nothing follows it) still looks like a torn end, and is cut; and if a crash ever left a bad record with a later one whole (a write that reached the disk out of order), that is refused too, where it used to be cut. A test holds each (damage in the payload, the CRC and the length of the second of three, and in the first; the last, damaged and torn; and through `openEvents`, twice, so the lock is let go).
     *Seen to fail:* with the look-ahead turned off, that test fails. The cost is a scan of the bytes after the bad record (only when there is one), at most as long as the log.
3. **Errors that say which kind they are.** *Mostly done on 2026-11-16: three kinds added; the fourth is left, and why.*
   * Listed where each `BadArgument` was thrown (`grep`, 2026-11-16) and grouped by what a caller does: the program is missing (3 places in `checkBackend`) → `BackendMissing`; the schema is refused (2 places in `recoverKhana`) → `SchemaRefused`;
     the world is open already (new) → `WorldBusy`. What is left in `BadArgument` is a wrong call: an empty batch, a reserved id, an erased id, a range that ends before it starts, a pull sent to `query`, a directory that exists for `backfillSince`, an unknown chunk version.
     Two places (`Khana.hs`, "needs a KhanaRuntime made for it") are the caller's mistake in assembling the config; they stay `BadArgument`.
   * The C door tells the new kinds (`world-busy`, `backend-missing`, `schema-refused`); `errorKind` is exhaustive over `HosiError`, so a fourth would not build until it is told. **This is visible to hosts:** a backend that is missing was `bad-argument`
     and is now `backend-missing` (the ABI test that said so was changed). The Elixir binding takes the kind as an atom, so it needs no change, but its tests were not run here.
   * **Left:** "the khana backend died". It is a `DtlvError` (a bare string, not a `HosiError`), thrown in about a dozen places, and some of them are the caller's fault: an unreadable query, a value that has no EDN form. The C door tells it `HOSI_ERR_INTERNAL`.
     To make it a kind (say `KhanaFailed`) the caller's own mistakes there must be told apart first. It is a bigger change, and the guide says that it waits.
4. **Draw the line around the public names.** *Done on 2026-11-16, as a proposal for nyanrus to change.*
   * `Hosigraph`'s export list is in two parts: the groups before the heading **Experimental** are stable (not removed, same meaning, in 0.1.x); under the heading are `freeze` (and its options and report), `historyTrace`, `LookupTrace`, `Probe`, `probeSays`, `Stats`, `stats`, `Coverage`, `cvQueryable`, `BackfillStage`, `backfillSince`.
     Nothing was added or removed: the names were only moved. The module header says what 0.1 promises; the guide has the same as a section ("What 0.1 promises") with the table of files, and the README's first screen points to it.
   * *Why `freeze` is experimental:* when a month is cold is not settled (the rule, `hcColdAfterMs`, and thaw is not in 0.1). The **cold chunk's file** is promised (the fixture has one); the **call that makes it** is not. It is also in the C door, which is not covered.
   * *`Hosigraph.Events` alone:* kept, and narrowed to what the tests use without a khana: `EventsConfig`, `defaultEventsConfig`, `openEvents`, `closeEvents`, `withEventsLock`, `record`, `recordMany`, `commit`, `rollback`, `seal`, `history`, `eventsAfter`, `lastSeq`, `verifyEvents`. The rest of that module, and every other module, is not promised.
   * `synopsis` and `description` no longer say "Phase 1 prototype"; the README's "A prototype" note is the 0.1 note. `version` is `0.1.0.0`. No tag, no push of a tag, no upload.
   * *Open:* the C door (`hosigraph-abi`, `libhosi`) has its own version (`0.0.1.0`) and was seen working on macOS only; this section does not cover it. Does 0.1.0 of the library come with a promise about the door, or not yet?
5. **Numbers on one page** *Done on 2026-11-16: the guide's "Measurements", taken again, with the machine and the date.*
   * One program takes them all: `scripts/bench.hs` (`scripts/build-ghc.sh bench`; not in the cabal package). Apple M4, 16 GB, internal SSD, Datalevin 1.2.10, Temurin 21.
   * *Calls* (replaces the old table, whose machine was not written down): `write` 1.05 ms (JVM) / 24 ms (dtlv); `writeMany` 0.06 / 0.14 ms for each item; `getEntity` 0.66 / 24 ms; `getEntities` of 200: 2.7 / 26 ms; 6 queries 2.0 → 1.0 / 160 → 30 ms.
     The old table's "the same items in one batch" row is gone: the new row is `writeMany` of separate batches (what a caller does).
   * *A world* (made up: 100,000 events): written in 3.1 / 2.6 s; open 1.5 / 0.1 s (1.4 s of the JVM's is the JVM starting); made again 2.8 / 2.3 s; the JVM's memory 260 MB after an open, 400 MB at most; dtlv 89 MB at most; hosigraph about 50 MB; events 1.4 MB, khana 8 to 22 MB.
   * *The worst open* is the real data's: 527,971 events (a copy of a real server). The khana made again: dtlv **34 s** on the internal disk (71 to 111 s on an external SSD, on 2026-11-16; 0.13 ms for each event, 0.02 ms for the made-up one), the JVM **7 s** (an open that does it: 10 s). Plan it as events × 0.15 ms (dtlv) and events × 0.02 ms (JVM).
   * *`verify`* on the same data: dtlv **65 s**, 165 MB for the largest process; the JVM **22 s**, and it passes with a 256 MB heap (2026-11-17). Before it was held whole: 4.3 GB (dtlv).
   * *Found while measuring:* the first run of the six queries was 41 ms in the JVM (the second 2 ms): a first use pays for parsing and warming up. The table says each query was asked once before. And the 25 ms sampling of memory sees nothing of a `dtlv` that lives for 25 ms (0 MB, then 41 MB, where `time -l` saw 89 MB); the guide says which was used.
   * *Not measured:* anything above 530,000 events; two threads; a disk that waits for `fsync` (on macOS it does not); Linux.

### Would help, and 0.1.x is soon enough

* **A shared lock for readers.** 0.1 has one opener: a second process that only wants to read (a backup, an inspector, a second reader of the same world) gets `WorldBusy`. A `LOCK_SH` on the same `events/lock` is possible with the same `flock`: readers share it, the writer takes it exclusively.
  Open: a reader would see a world whose owner may seal or erase underneath it (a chunk directory swapped, a file gone), so it needs a read that survives that, or a promise that its answers are for one manifest. Not decided; waits for a user who needs it.
* **A backup call.** The khana is derived, so `events/` alone is a backup. Open: may it be copied while writes go on (seal first, under the lock?), or only when the `Hosi` is closed?
* **Many threads.** The guide lists as not tested: `clean` from two threads at once, many readers while `seal` or `erase` runs, and "the world changed between the read and the lock" in a rewrite.
* **A `verify` that walks both khanas in one `dtlv` process.** Today it asks for a page at a time, and each page is a `dtlv` process (about 110 of them on real data; the 65 s is 34 s of rebuild and about 30 s of this walk, not measured call by call). The JVM keeps one process and takes 22 s. Not needed for 0.1: `verify` is rare work.
* **Linux, for real.** CI runs on ubuntu with the JVM khana only; the native `dtlv` is not run there, and the `abi` job has been seen to pass on macOS only. Windows has no `System.Posix`; say it.

* **`highest` with a filter, and the other way.** `highest` walks one attribute and cannot be given a filter, so a timeline of the people one follows has to ask for more and filter, or use a query over a window.
  A walk with a predicate, or one walk per author merged, is not built; nor is the walk upwards (`lowest`: `seek-datoms` on `:ave`, a few lines when it is wanted). A tuple, or a long string, as the walked attribute was not tried.
* **Fulltext without the JVM.** The analyzer is an `inter-fn`, which the native `dtlv` cannot run (`Can only freeze an inter-fn`), so the native khana refuses the schema. With the default analyzer it would work for English and not for Japanese; whether that half is worth having is open. Also not tried: a phrase search, other analyzers, an attribute name that is not `ns/name`.
* **Tuples.** Not checked at open: a tuple of an attribute that is not in the schema, or of itself, or `many`. A pull of a tuple attribute was not tried.

### Not in 0.1.0

S3 (the section below), live serving of a backfill (the section after it), distribution and sync, metrics, JSON-LD import, thaw and compression of cold chunks.
The reason is one sentence: "local, one process" can be promised now; S3 brings failures of the network, which is a different promise (0.2).

### Open for nyanrus: how strongly to say "embedded"

The khana needs a program outside the process, and the in-process one was taken out on purpose (see "Notes on the current state" in the guide).

* **(a)** 0.1.0 says it plainly: embedded, and it brings Datalevin along. This is the leaning. It keeps 0.1.0 near.
* **(b)** Wait for a khana inside the process. 0.1.0 then moves far away, and nothing in the repository starts that work.

If the first users are sukhi and tacchaka-matrix, (a) probably promises enough; that is a guess, not something asked.

**Done when:** items 1 to 5 each have a test or a line in the guide, and the first paragraph of the README says what it needs.

**State (2026-11-16):** items 1 to 5 are done as written above (item 3 with `DtlvError` left, item 2 without a tail mark). `version` is `0.1.0.0`; `CHANGELOG.md` says "not released". Left for nyanrus: the tag and the release (nothing was tagged, uploaded or pushed as a tag), the open questions below and in items 2 and 4, and `mix test` of `hosigraph-ex`, which was not run.

## Phase 2, the S3 part — against RustFS

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
(`delete` waits for Phase 3.) Errors keep three things apart: **the object is not there**, **the store cannot be reached**, **we are not allowed**.

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
**Question for nyanrus:** after a verified upload, does the local copy stay (a cache) or go (remote only)? Phase 3's freezing probably decides; for now keep it.

### 6. Manifest: where a chunk lives

A chunk entry gains a place (local, or remote with bucket and key prefix). Old manifests must still read as "local".
Check how the manifest hash and signature cover it. Cleaning of objects the manifest does not list: `list prefix`, then delete (after Phase 3 has `delete`).

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

### Not now (Phase 3 and later)

* Erase and bucket versioning: the design document says an Erase is complete only after the old versions are deleted too (line 123 of the design document). Check what RustFS does with versioning then.
* Freezing / cold chunks, garbage collection of remote chunks, lifecycle rules.
* Expanding RustFS: a single-node single-drive deployment cannot be grown in place (its docs say so). Not our problem for tests; it matters for a real deployment.

## Backfill: live serving (after `backfillSince`)

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
* **Speed.** On the real world nearly all of the time is `dtlv`, one process for each transaction of 2,000 facts (the program's own CPU time was 2.8 s of 107 s). A larger transaction is the lever; `applyBatched`
  (the rebuild) uses 2,000 too. Not tried.

