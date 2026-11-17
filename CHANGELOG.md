# Changelog

## 0.2.0.0 (not released)

**No change to the files.** What 0.1.0 wrote, every 0.1.x and 0.2.x reads (the fixture of 0.1.0 is opened by every build), and a schema that uses none of the new things is the schema it was, so its khana is not made again.
0.2 asks more of "now": an order and a cut, pairs, words, and the newest *n* of what some people wrote. It is still one process on one machine, on a local disk ([what is promised](docs/guide.md#what-01-promises)).

### New

* **`:order-by`, `:limit` and `:offset`** in a query (a relation `:find`; the forms Datalevin would answer without sorting or cutting are refused), **composite tuples** (`tuple`: a pair that is kept by the khana and held once) and **fulltext** (`fulltext`: words, with a CJK-aware analyzer; JVM khana only; `clean` makes the khana again for it).
  What they are, what they cost and what they do not do: [the guide](docs/guide.md#order-and-cut-tuples-and-words).
* **`highest`**: the *n* facts of one attribute with the largest values, from the end of its index, a page after a page (`:after` is the place of the last one). It reads about *n* facts and not all of them, where `:order-by … :limit` sorts every row first.
  One attribute with a type, and no filter. Also an operation of the C door (`highest`) and `Hosi.highest` in Elixir.
* **`highestIn`**: the same for the facts of *some keys*, "the newest *n* notes of the people I follow". The attribute is a pair `(key, order)` made with `tuple`; the answer is merged from one short walk for each key, so what it costs is the number of keys and *n*,
  not the number of notes those keys have. Also an operation of the C door (`highest-in`) and `Hosi.highest_in` in Elixir. What it costs, measured, and what it does not do: [the guide](docs/guide.md#highestin-the-newest-n-of-some-keys).
* **`highest` and `highestIn` are experimental** in 0.2 (under that heading in `Hosigraph`): what they answer now is not meant to change, but their form may, in a 0.2.x, when an order that is not a long or the other direction is added.
* **A tuple crosses the C door:** a vector, with `nil` for a missing part. It is read, and asked for in an input or a condition; it is never written.
* **`hosigraph stdio`**: the operations of the C door over stdin and stdout, one line each, EDN or (`--json`) JSON; `--ids-as-strings` writes integers past 2^53 as strings. `{"type":"double","value":1}` says the type of a value. An executable of `hosigraph-abi`: [the guide](docs/guide.md#hosigraph-stdio).
* The query parser reads a function's binding in every form (`[(f ?x) [?a ?b]]`, `[?x ...]`, `[[?e ?a ?v]]`), `$` and an option map as arguments, and a vector of values as a tuple.

### Changed (visible to a host)

* **`Value` has a constructor more, `VTuple`, and `AttrSchema` has two fields more (`asTuple`, `asFulltext`).** Keep a catch-all case when you match a `Value`, and build a schema with `attr`, `many`, `identity`, … and not with the constructor. A schema that uses neither of the new things has the schema mark it had, so a world made before is not made again.
  (`takka`, which holds `Value`, is still `0.1.0.1`.)

### Not in 0.2

S3, live serving of a backfill, distribution and sync, metrics, JSON-LD import, thaw and compression of cold chunks. Windows, and a directory on a network file system. What is left is in `TODO.md`, under 0.3.0.
A world still has **one opener**.

## 0.1.0.0 (not released)

The first version you can keep data in: **one process on one machine, on a local disk.** What 0.1.0 wrote, every 0.1.x reads.
What is promised and what is not: [the guide](docs/guide.md#what-01-promises).

### New

* **One opener at a time.** Opening a world that is open already, in this process or in another, stops with `WorldBusy` before anything is touched (`flock(2)` on `events/lock`; let go when the process dies).
* **Errors say which kind they are:** `WorldBusy`, `BackendMissing` (the khana's program is not where the config says), `SchemaRefused` (the khana cannot be made under this schema). The C door tells them as `world-busy`, `backend-missing`, `schema-refused`.
* **The files are a promise, and a test holds it:** `test/fixtures/world-0.1.0/` (chunks of both versions, a cold chunk, an erase record, an unsealed tail) is opened by every build.
* **A tail log of a format this code does not know is refused, not cut:** the first four bytes `HOSI` of `tail.log` are reserved for a later format (`Corrupt`; the file is left as it is).
  A record that cannot be read, with a whole record after it, stops the open with `Corrupt` (the byte where the bad record begins is in the message); a torn end, and a damaged last record (which looks the same), are cut.
* **`verify` holds a page of each khana at a time (10,000 facts), and each chunk while it checks it.** On a real world of 527,971 facts: 165 MB and 65 s with `KhanaDtlv`, 22 s with the JVM (it passes with a 256 MB heap).
  A khana that cannot be read for the comparison is a problem in the list (`khana: cannot compare: …`), and the second khana is closed and removed.
* `Hosigraph` lists what is **stable** and what is **experimental** (`freeze`, `historyTrace`, `stats`, `backfillSince`, and their types).

### Changed (visible to a host)

For a host that was written against 0.0.1.0:

* A khana program that is missing is `BackendMissing` (`backend-missing` at the C door), not `BadArgument`.
* A schema that the khana refuses is `SchemaRefused`, not `BadArgument`.
* `HosiError` has three more constructors. Keep a catch-all case when you match on it.
