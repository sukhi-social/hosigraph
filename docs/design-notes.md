# Design notes: open points of the Phase 1 prototype

> About names: the design document calls the "now" part the "core material" (芯材). In hosigraph, it is the **khana** (刹那, "a moment" = now).
> The part that groups the tail log, the chunks, and the manifest is called **Events**.
> In these notes, "core material" means the khana (see "What the names mean" in docs/guide.md).

These notes list the open points of the Phase 1 prototype of the [design document v0.2.0-draft](hosiDB-design-v0.2.0-draft.md).
Each note says what the prototype does and asks what the design document wants. Item 9 was added for Phase 3 (the local part: Erase, cleaning, freezing).

## 1. A chunk also needs a seq

In the design document, only the tail log has a sequence number.
But to build the core material again (to fold the chunks), the fold must know
**whether a Create or a Delete came first, and which Create is newer**, from the chunks alone.
A chunk is sorted by kind, then month, then ID, so the sort order does not give the time order.
So each event in a chunk has a `seq` (8 bytes).
The fold sorts the events of all chunks and of the tail log by seq.

Suggestion: add "an event has a seq, and the chunk keeps it" to the "Data model" of the design document.

## 2. The "month" is the month in which the entity was created

The design document sorts by kind, then creation month, then ID. The ID tells you the creation month. It does not sort by update date.
In the prototype, a Delete and a newer Create go into the chunk of **the month in which the target entity was created**.

* Good: the ID tells you the month, so a point lookup skips whole chunks of other months (a test checks this).
* Concern: each time somebody changes an old entity, a small chunk appears in an old month.
  A "cold month" also gets new hot chunks. Inside one freezing unit (kind × month), frozen and hot data live together.
  This must be decided together with the freezing rule (how cold is cold) and with thawing (copy to a new hot chunk).

## 3. Read the core material and the tail log together

Design document: "When you read, look at the core material and the unsealed tail log together."
A Datalevin `Db` is one LMDB read transaction (the same was true of the in-process engine this prototype once had). It has no place to put other data on top of it.
To make the whole Datalog engine read two sources together is a big job.

The prototype uses **write-through**: `write` appends to the tail log (fsync), and then folds into the core material.
The core material is the fold of all events, not only of the sealed chunks.
But it can still be built again (from the chunks and the tail log), and "now" never stays old.

Other ways: (a) before a read, fold the tail log into the core material (lazy). (b) Read the two sources together only for ID lookups, and show Datalog results only after the seal.
Open question: which way is closer to the intent of the design document?

## 4. Who decides whether a write is allowed

A type error (a string for `age`) or a unique-key conflict shows **only when the fold runs** (the khana holds the schema).
But the design writes the tail log first. If the tail log keeps an event that the core material refuses, the fold fails at every restart.

The order in the prototype:
1. Append to the tail log and fsync.
2. Fold into the core material (one transaction).
3. If the core material refuses, cut that one write from the tail log and return `WriteRejected`.

If the system stops in the middle, the next open folds again. It drops a record that the core material refuses (nobody got a reply).
The promise for events in chunks is: "every accepted event can be folded".

## 5. There are two fast paths

The fast path in the design document is: the MPHF returns a number, the system compares the key, checks the invalidation, and answers.
In the prototype, **the core material answers "now", and the MPHF answers "the list of events of this ID"** (each chunk has its own MPHF).

* `getEntity` (now) → the core material
* `history` (the past) → narrow by month → narrow by ID range → MPHF → fingerprint → key in the body

In Phase 3, the invalidation record (Erase) joins the `history` path (and asks earlier than the design says: see item 9).
How Erase works on the "now" path (does the data stay in the core material until the cleanup?) is answered in item 9 too: yes, it stays until `clean`.

## 6. Nobody stores the schema

Whether an attribute has cardinality one or many changes the meaning of Create (replace or add).
So **the result of the fold depends on the schema**.
You give the schema when you open. Nobody stores it.
If you reopen with a different schema and build the core material again, "now" can be different.
The simplest fix is to store the schema (or its hash) in the manifest and compare it at open.

## 7. The 63 bits of the ID

The ID has a 42-bit millisecond count and a 21-bit sequence number. This gives about 139 years and up to 2 million IDs in one millisecond.
The entity ID of the khana is a signed 64-bit number, so 63 bits is the limit.
To make IDs that never collide across nodes (room for distribution), the ID needs node bits or random bits. There are no spare bits.
One option: a 12-bit sequence and 9 bits for the node. Phase 0 ("ID scheme") must decide this.

## (Extra) What the tests check

* MPHF: any set of different keys maps one-to-one to [0,n) (QuickCheck, 300 runs); a serialization round trip; 50,000 keys (the shape of real IDs).
* Chunks: any list of events that you write is the same when you read it; every ID finds only its own events; an ID that is not in the chunk is not found; a damaged file gives `Corrupt`.
* Tail log: a round trip; detection of a cut tail.
* The 5 places where the system can stop; a core material that is ahead of the events or behind them (it catches up from the chunks and the tail log); a deleted core material; a changed manifest or chunk.
* A record that is in the log but that the core material refuses (right after a stop) is dropped at open, and its seq number is left free.
* A refused write (type error, unique-key conflict) leaves no trace in the log and none in the sequence numbers.
* The zone map: the system does not read chunks of "other months" or "outside the range"; the fingerprint refuses without a read of the body.
* IDs still grow when the clock goes back; 3000 IDs in one millisecond; 4 parallel writers × 25 writes.
* Seal 20,000 events in 4 steps → look up every ID → reopen → `verify`.

## 8. The parts have names that the design document does not have

The code has two parts that the design document does not name.

* **Khana** (刹那 = now = core material): the part that the system builds from the events and that you can throw away. It sits on Datalevin.
* **Events**: the tail log, the chunks, the manifest, and "find the events of one ID". This part holds the truth. It knows no khana and no Datalog.

`Events` has an interface of **three beats: write (record) → see if the khana accepts → confirm (commit) or cancel (rollback)**.
Suggestion: name these three beats (especially the cancel) in the "write flow" of the design document.
They make it easy to put another khana on top.

## 9. Phase 3 (the local part): where the prototype differs from the design document

What `erase`, `clean` and `freeze` do is in [the guide](guide.md#phase-3-so-far). These are the places where the prototype chose differently from the design document,
or where the document says nothing. Each one asks the design document to take the choice, or to say why not.

1. **The erase record is asked right after the zone map, not last.** The design's read order is "filter → MPHF → key → invalidation". Here it is "zone map → record → filter → MPHF → key".
   The record is in memory, so an erased ID is turned away with no file read at all; asked last, the same ID would first cost a filter and a block. The answer is the same, and
   `LookupTrace` has a field of its own for it (`ltSkippedByErase`).
2. **The record is not part of the chunk's hash.** `ceHash` stays the identity of the chunk's content, which the design wants for distribution (the same chunk, the same hash, on every node).
   The record belongs to the *world*: it is a file of its own (`events/erase/<chunk>-<generation>.bin`), listed in the manifest next to the chunk and not inside the chunk's directory.
   That also means it works the same when the chunk is on S3. `verify` checks the record's own hash.
3. **The manifest is `/2` once the world has ever erased.** An old build must *refuse* such a manifest, not ignore the field it does not know and show what was erased. A world that never erased
   is still written as `/1`, byte for byte as before. It is `/2` when the erase generation is above 0, not only when a record exists, because the counter must only go up: if `/2` depended on the
   records alone, `clean` taking away the last record would write a `/1` without the counter, and it would start again at 0. The price is that a world that erased once stays `/2`.
   The other way would be to reset the counter in memory when the last record goes. Which one does the design want?
4. **One manifest swap for each rewritten chunk, not one for the whole run.** The design says one swap per run. Small steps keep each swap cheap, and they let a host call `clean` a little at a time,
   in each quiet moment (`coMaxChunks`). The price is more swaps; a crash between two leaves a world in which some chunks are clean and some are not, which is a good world.
5. **Ranges, not Roaring bitmaps.** The design suggests a Roaring bitmap for single IDs. An ID is 63 bits and a month holds about 2^52 of them, so a million erased IDs are a density of 2^-32:
   a Roaring container would hold one ID each. A list of sorted ranges (a single ID is a range of one) took 4 to 8 bytes for each ID, and a lookup took 70 to 160 ns, for 1,000 to
   1,000,000 IDs (a probe, in the plan; the guide gives the table). A whole day or a whole month is one range. A flag for "single ID" would save about one byte an ID (17 %); not worth
   a second file format until a number asks for it.
6. **"Now": the entity's own facts go, and the file is made new.** The khana retracts the *entity's own* facts only (not `:db/retractEntity`, which would also take other entities' references to it), so
   that "now" stays equal to a fold of the events minus the erased ones, and `verify` agrees. A retract leaves the bytes in Datalevin's LMDB file (measured, on the native `dtlv` and on the JVM),
   so `clean` ends by making the khana a new file: a compacted copy, and a full rebuild when the copy cannot be made. The crash recovery is a rebuild too: if the khana's `erase-gen` is behind the
   manifest's, the khana is built again at open. This is where the design's question "does the data stay in the core material until the cleanup?" is answered: it does.
7. **The user unit is found in the events, not in "now".** An account's things are not one range of IDs (an ID is a time), and a post the account already deleted is gone from "now" but its Create and
   Delete are still in the chunks. So `EraseOwnedBy` reads the Create chunks for the attributes the caller names. This costs a scan of the chunks it reaches; the design's "a range suits everything
   of this user" fits the date unit, not the user unit. (If the design wants this to be cheap, it needs an index from owner to IDs; the plan has a proposal for per-owner objects, not built.)
8. **Freezing and note 2.** A late write into a frozen month makes a new hot chunk (the month is that of the entity, note 2). The prototype lets the next freeze turn it into a second cold chunk,
   and never merges cold chunks, and has no thaw. Whether a month that is written again should be thawed, or its cold chunks merged, is not decided; it needs the numbers of a real world.
   The first rule of freezing is the design's version 1 (last read or write older than a threshold); the other rules are not here.
9. **After a record goes, the guard goes.** While a chunk has a record, a write to an erased ID is refused. When the chunk is written again, or was dropped whole, there is no record, and the ID is
   free: a write would make a new, lonely entity. Keeping the erased IDs for ever would stop that, but the list would grow without bound; the design's "pruning horizon" is the place to decide it,
   together with distribution (a node that was away must still learn what was erased).
10. **What "physically gone" does and does not mean.** It means gone from hosigraph's own files (chunks, records, tail log, the khana); a test puts a marker text in the data and looks in every file.
    It does not mean gone from freed disk blocks, SSD wear-levelling, snapshots and backups, an S3 bucket's old versions, or a copy the caller made. An ID carries its creation time, so while a record
    exists it says that something was created at that moment and erased; there is no content in it, but it is a trace, and it goes with its chunk's rewrite. The design document should say this
    in plain words wherever it promises that Erase makes data gone.
11. **A pointer to an erased entity stays as a number.** Someone else's reply to an erased post, or a mention of an erased account, keeps the ID it points at, which now points at nothing.
    Blanking such pointers rewrites other people's entities, a different and bigger operation, and it is not planned.
