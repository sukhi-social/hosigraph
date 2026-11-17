# hosigraph ⭐

[![test](https://github.com/sukhi-social/hosigraph/actions/workflows/test.yml/badge.svg)](https://github.com/sukhi-social/hosigraph/actions/workflows/test.yml)

A small embedded database that **keeps the past as events**, and keeps **"now" as a copy you can put down**.

> [!NOTE]
> **0.2 is for one process on one machine, on a local disk.** It is written in Haskell, and it brings [Datalevin](https://github.com/datalevin/datalevin) along:
> the `dtlv` program, or JDK 21 and the standalone jar.
> What 0.1.0 wrote, every 0.1.x and 0.2.x reads ([what is promised, and what is not](docs/guide.md#what-01-promises)). **S3 is not here yet** (it waits in 0.3).
> 0.2 changes no file. It asks more of "now": an order and a cut, pairs, words, and the newest *n* of what some people wrote ([below](#a-timeline)).
> Come and sit with it for a while; if anything puzzles you, an issue is welcome.

## The idea

1. Every change is an **event**: a `Create` or a `Delete` of one fact. Events are only added, never edited. **The events are the truth.**
2. **"Now"** is a Datalog database called the **khana**, built by folding the events. Lose it, or stop trusting it? Throw it away; the events build it again.
3. So there are two ways to read:

| You ask about | You get it from | Call |
|---|---|---|
| **now** | the khana | `getEntity`, `query` |
| **the past** | the events of one ID | `history` |

```mermaid
flowchart LR
    W([write]) --> T[tail log]
    T == "fold, at every write" ==> K{{"khana: now"}}
    T -- "seal: freeze" --> C[(chunks)]
    C -. "fold, only to rebuild" .-> K
```

* **Every day** (thick arrow): a write goes into the tail log and is folded into the khana at once.
* **`seal`** (thin arrow): the tail log is frozen into chunks. The khana is not touched.
* **Rebuild** (dotted arrow): only when the khana is lost or doubted (`rebuildKhana`). It folds the chunks, then the tail log, into a fresh khana.

It promises one thing first: **write, seal, restart, and get the same answers.**
The tests stop the system in five places and check what comes back.

## A taste

```haskell
main = bracket (openHosiWith cfg "/tmp/hosi" schema) closeHosi $ \h -> do
  r <- write h [ Create (Tmp 1) "person/name" (Lit (VStr "Frege"))      -- Tmp 1 becomes a real ID
               , Create (Tmp 1) "person/age"  (Lit (VInt 70)) ]
  let frege = wrTemps r M.! 1
  getEntity h frege >>= print                                     -- now
  query h "[:find ?n :where [?e :person/name ?n]]" [] >>= print   -- Datalog, as Datalevin reads it
  _ <- seal h                                                     -- freeze the tail log into chunks
  history h frege >>= mapM_ print                                 -- the past
```

A write goes in whole, or not at all. `verify` rebuilds a khana from the events and checks that it equals yours.

## A timeline

"The newest 20 notes of the people I follow", a page after a page: what a feed asks for.

```haskell
-- the schema: ("note/by", attr TString), ("note/at", attr TLong), and the pair of the two, which the khana keeps for you:
--             ("note/by+at", tuple ["note/by", "note/at"])
page1 <- highestIn h "note/by+at" (map VStr followed) Nothing 20
```

It takes a short walk for each person and merges them, so what it costs follows the number of people, and not how much they have written.
On 200,000 made-up notes, 400 people with 400 notes each: **3.9 ms** (the same question as a query, `:order-by … :limit 20`, took 240 ms). For a few people who wrote little, the query does as well.
[Where it stops, and the numbers](docs/guide.md#highestin-the-newest-n-of-some-keys).

<details>
<summary><b>How does it ask the past without waking everything?</b></summary>

<br>

A question about one ID goes through gates, the cheapest first:

1. the **zone map** in the manifest (month, ID range): no file is touched;
2. the chunk's **Binary Fuse filter**: a few bytes per ID. "Certainly not" is always right; "maybe" is wrong about once in 65,536;
3. only for a "maybe", the chunk is opened and the slot is found.

A chunk that the filter turns away is **never opened**. A test checks this.

</details>

## Running it

```sh
cabal build all
cabal test
cabal run hosigraph-demo
```

Tested with GHC 9.10.3 on macOS arm64.

> [!TIP]
> "Now" lives in [Datalevin](https://github.com/datalevin/datalevin). Bring either the `dtlv` executable, or JDK 21 and the standalone jar.
> Without them the tests still run, and print which parts they skipped. How to point hosigraph at Datalevin: [the long guide](docs/guide.md).

## Where it is

- [x] Phase 1: events, chunks, the khana, restart-safe
- [x] Phase 2, first half: the filter, and reading a chunk by ranges
- [x] A C interface, `libhosi`, written by [setu](docs/guide.md#from-other-languages-setu-hosigraph-abi) (seen working on macOS only)
- [x] `hosigraph stdio`: the same operations over stdin and stdout, one line each, EDN or (`--json`) JSON ([guide](docs/guide.md#hosigraph-stdio))
- [x] Phase 3, the local part: `erase` (ids, a span of ids or time, or what an account owns), `clean`, `freeze` (cold chunks), also through the C door
- [x] 0.2: an order and a cut (`:order-by`, `:limit`), pairs (`tuple`), words (`fulltext`), and the newest *n* (`highest`, `highestIn`), also through the C door and in Elixir
- [ ] S3 ([plan](TODO.md), for 0.3)

Speed is only roughly measured, and the guide lists the limits.

## More

- [The long guide](docs/guide.md): Datalevin setup, file layout, limits, and what the names mean
- [The design this grew from](docs/hosiDB-design-v0.2.0-draft.md) (it was called hosiDB then) and [design notes](docs/design-notes.md)
- [takka](takka/README.md): the small library of words hosigraph and Datalevin share

*P.S.* `Hosi` + `Khana` sounds like *hoshi kana*: "is it a star?"

## License

[Blue Oak Model License 1.0.0](LICENSE): short, plain English. Do what you like, and pass the licence (or its link) on with it.
What hosigraph stands on is in [`docs/dependency-licenses.md`](docs/dependency-licenses.md). Datalevin is a separate program with its own licence; hosigraph only talks to it.
