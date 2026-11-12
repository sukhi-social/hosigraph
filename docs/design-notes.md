# Design notes: open points of the Phase 1 prototype

> About names: the design document calls the "now" part the "core material" (芯材). In hosigraph, it is the **khana** (刹那, "a moment" = now).
> The part that groups the tail log, the chunks, and the manifest is called **Events**.
> In these notes, "core material" means the khana (see "What the names mean" in docs/guide.md).

These notes list the open points of the Phase 1 prototype of the [design document v0.2.0-draft](hosiDB-design-v0.2.0-draft.md).
Each note says what the prototype does and asks what the design document wants.

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

In Phase 3, the invalidation record (Erase) has the read order "filter → MPHF → key → invalidation", so it joins the `history` path.
How Erase works on the "now" path (does the data stay in the core material until the cleanup?) is a question for that phase.

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
