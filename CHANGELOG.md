# Changelog

## 0.1.0.0 (not released)

The first version you can keep data in: **one process on one machine, on a local disk.** What 0.1.0 wrote, every 0.1.x reads.
What is promised and what is not: [the guide](docs/guide.md#what-01-promises).

### New

* **One opener at a time.** Opening a world that is open already, in this process or in another, stops with `WorldBusy` before anything is touched (`flock(2)` on `events/lock`; let go when the process dies).
  Before this, two writers overwrote each other's records in the tail log, and each was told that its write went in.
* **Errors say which kind they are:** `WorldBusy`, `BackendMissing` (the khana's program is not where the config says), `SchemaRefused` (the khana cannot be made under this schema). The C door tells them as `world-busy`, `backend-missing`, `schema-refused`.
* **The files are a promise, and a test holds it:** `test/fixtures/world-0.1.0/` (chunks of both versions, a cold chunk, an erase record, an unsealed tail) is opened by every build.
* **A newer tail log is refused, not cut:** the first four bytes `HOSI` of `tail.log` are reserved for a later format; this version refuses such a file (`Corrupt`) and leaves it as it is.
* `Hosigraph` lists what is **stable** and what is **experimental** (`freeze`, `historyTrace`, `stats`, `backfillSince`, and their types).

* **`:order-by`, `:limit` and `:offset`** in a query (a relation `:find`; the forms Datalevin would answer without sorting or cutting are refused), **composite tuples** (`tuple`: a pair that is kept by the khana and held once) and **fulltext** (`fulltext`: words, with a CJK-aware analyzer; JVM khana only; `clean` makes the khana again for it).
  What they are, what they cost and what they do not do: [the guide](docs/guide.md#order-and-cut-tuples-and-words).
* **`highest`**: the *n* facts of one attribute with the largest values, from the end of its index, a page after a page (`:after` is the place of the last one). It reads about *n* facts and not all of them, where `:order-by … :limit` sorts every row first
  (200,000 notes: 0.05 ms against 92 ms). One attribute with a type, and no filter. Also an operation of the C door (`highest`) and `Hosi.highest` in Elixir.
* **A tuple crosses the C door:** a vector, with `nil` for a missing part. It is read, and asked for in an input or a condition; it is never written.
* The query parser reads a function's binding in every form (`[(f ?x) [?a ?b]]`, `[?x ...]`, `[[?e ?a ?v]]`), `$` and an option map as arguments, and a vector of values as a tuple.

### Changed (visible to a host)

* **`Value` has a constructor more, `VTuple`, and `AttrSchema` has two fields more (`asTuple`, `asFulltext`).** Keep a catch-all case when you match a `Value`, and build a schema with `attr`, `many`, `identity`, … and not with the constructor. A schema that uses neither of the new things has the schema mark it had, so a world made before is not made again.

* **A tail log that is damaged in the middle is refused, not cut.** Before, a record that could not be read ended the log, and everything after it was cut at the open, even when whole records followed it
  (batches that had been told to their writers as kept). Now, if a whole record follows the bad one, the open stops with `Corrupt` (the byte where the bad record begins is in the message) and the file is left as it is.
  A torn end, and a damaged last record (which looks the same), are cut as before.
* **`verify` no longer holds both khanas, nor the chunks.** The two khanas are compared a page at a time (10,000 facts, whole entities), and each chunk's problems are looked at when the chunk is done.
  On a real world of 527,971 facts: `KhanaDtlv` takes 165 MB for the largest process (it was 4.3 GB) and 65 s (it was 55 s); with the JVM it takes 22 s (24 to 26 s before) and passes with a 256 MB heap.
  A khana that cannot be read for the comparison is a problem in the list (`khana: cannot compare: …`), and the second khana is closed and removed; before, it was an exception that left both behind.
* A khana program that is missing is `BackendMissing`; it was `BadArgument` (`bad-argument` at the C door).
* A schema that the khana refuses is `SchemaRefused`; it was `BadArgument`.
* `HosiError` has three more constructors. Keep a catch-all case when you match on it.

### Not in 0.1

S3, live serving of a backfill, distribution and sync, metrics, JSON-LD import, thaw and compression of cold chunks. Windows, and a directory on a network file system.
A world has **one opener**: a second process cannot even read it while it is open (a shared lock for readers is in `TODO.md`).
