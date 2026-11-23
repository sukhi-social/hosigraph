# hosigraph, the long guide ⭐

This is the whole guide. The short, friendly introduction is the [README](../README.md).

A small embedded database that **keeps the past as events**, and keeps **"now" as a copy you can throw away**.

It follows the [design document v0.2.0-draft](hosiDB-design-v0.2.0-draft.md), which calls the project **hosiDB**: the events and the khana,
the Binary Fuse filter and the "deep path" that reads a chunk by ranges (see "The filter and the deep path"),
and the local part of Erase, cleaning and freezing (see "Erase, clean and freeze"). **S3 itself is not here.** The code is in Haskell.

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

What it promises first: **write, seal, restart, and get the same answers.**
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

`historyTrace` asks the chunk's filter first (see "The filter and the deep path"); a chunk that the filter turns away is never opened.
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
"Takka" is Pali for reasoning and logic.

```sh
cabal build all
cabal run hosigraph-demo                 # a walk-through that moves a clock from November to December
cabal test                               # the tests that need no Datalevin; more with Datalevin (see below)
```

Tested: GHC 9.10.3 with `cabal build all`, on macOS (arm64). Linux is not tested here;
the GitHub Actions workflow ([`test.yml`](../.github/workflows/test.yml)) runs the tests on ubuntu-24.04, with the JVM.

> **`scripts/build-ghc.sh`** builds with plain `ghc`, for machines where cabal cannot reach Hackage:
> `scripts/build-ghc.sh app` (or `test`). It writes to `build/`. Plain `ghc` must be able to see `aeson` (and `QuickCheck` for the tests).
> **It has not been run to the end.** On the Mac where it was tried, plain `ghc` could not see `aeson`. Use cabal.

### Run the tests against Datalevin

Most tests need a real Datalevin, so tell them where it is. If you do not, they print that they skipped those parts.

```sh
HOSIGRAPH_DTLV=/path/to/dtlv cabal test        # the native dtlv
HOSIGRAPH_DATALEVIN_JAR=/path/to/datalevin-1.2.10-standalone.jar HOSIGRAPH_JAVA=/path/to/java cabal test   # a resident JVM (JDK 21 or newer)
```

| You give | Tests that run |
|---|---|
| nothing | those of the events, chunks, filters, the deep path, the tail log and the EDN writer |
| `dtlv` | those, and the ones that use the khana through `dtlv`, both ways: one process for each operation (`KhanaDtlv`, the tests start with `[dtlv]`) and one resident `dtlv repl` (`KhanaDtlvRepl`, `[dtlv-repl]`) |
| the JVM | those, and the ones that use the khana through the JVM (`[jvm]`) |
| `dtlv` and the JVM | all of them (including: write with the JVM and read with `dtlv`, and the reverse) |

The same scenarios run on `dtlv`, on the resident `dtlv repl` and on the JVM; the write queue and the regions run on the JVM and on the repl.
The tests choose the khana themselves. `HOSIGRAPH_TEST_ONLY='[dtlv-repl]'` runs one group (any text in a test's name will do).

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
| `highest` | The *n* facts of one attribute with the largest values, from the end of its index; a page after a page. See "Order and cut". |
| `highestIn` | `highest` for the facts of some keys only (the newest notes of the people one follows), merged from a tuple `(key, order)`. See "`highestIn`". |
| `history`, `historyTrace` | Read **the past**: every event of one ID, from the chunks and the tail log. `historyTrace` also shows which chunks it skipped (by the zone map, and by the filter) and what each probe answered. |
| `seal` | Turn the tail log into chunks. `HosiConfig` can also seal by itself, by size or by age. |
| `verify` | Check the manifest signature, the chunk hashes, the footers, and the MPHF lookup of every ID. It also checks that **the khana equals a khana built again from the events** (a second khana is made in a temporary directory beside the first, and the two are compared 10,000 facts at a time). An empty list means all good. It takes about as long as a rebuild, and then some; see Measurements. |
| `rebuildKhana` | Throw the khana away and fold the events again. |
| `erase` | Make entities gone, from the events and from "now": some ids, a range of ids, a span of creation time, or what an account owns. See "Erase, clean and freeze". |
| `clean` | Let what `erase` took out go from the disk: write the chunks again without it, and make the khana a new file. |
| `freeze` | Put a month that nobody has read or written for a while into one cold chunk. Does nothing unless `hcColdAfterMs` is set. |

You can also use `Hosigraph.Events` alone, without a khana. The tests cover this. Which of its names are promised: see "What 0.1 promises".

## Order and cut, tuples, and words

What a timeline, a "does this pair exist" check and a search box need. All three are the khana's: they are about "now", and none of them is written into the events.

### `:order-by`, `:limit`, `:offset`

```haskell
query h "[:find ?n ?a :where [?e :person/name ?n] [?e :person/age ?a] :order-by [?a :desc ?n] :limit 20 :offset 40]" []
```

They are Datalevin's, and they work for a **relation** find (`:find ?n ?a`) only. With `:find ?x .` or `:find [?x ...]` Datalevin 1.2.10 answers the whole set, in no order, and says nothing;
so hosigraph refuses those forms before they are asked (`parse-error` at the C door). Ask for the rows and take the column you want.

* In the vector form, `:order-by [?a :desc ?n]` is a variable and then `:asc` (the default) or `:desc`. In the map form it is a vector of pairs, `{:order-by [[?a :desc] [?n]]}`; a flat `[?a :desc]` there is refused, because Datalevin would read it as ascending.
* `:limit` is 1 or more (Datalevin: 0 is an error, and a negative number means no limit at all, so both are refused); `:offset` is 0 or more. Either may be a scalar of `:in`: `:in $ ?k ... :limit ?k`.
* Rows come back in the order asked. (Without `:order-by` the order is still not promised.)

**`:limit` does not make a query cheaper.** Datalevin finds every row that matches, sorts them, and then cuts. Measured on 200,000 notes (JVM, one machine, medians of 20 runs with the input changed each time, since Datalevin keeps answers):
all rows 64 ms; `:order-by [?ts :desc] :limit 20` 94 ms (more: it sorts); the same inside a window of the newest 50,000, 21 ms; for one author's 400 notes, 1.5 ms.
So narrow first (a window of ids or time, an author), and order and cut after. For "the newest *n*" of a whole attribute there is a call that does not sort: `highest`, next.

### `highest`: the newest *n*, without sorting all of them

(`highest` and `highestIn` are **experimental** in 0.2: [the names](#the-names).)

```haskell
page1 <- highest h "note/at" Nothing 20                                          -- [Datom id "note/at" value], the largest value first
page2 <- highest h "note/at" (Just (dV (last page1), dE (last page1))) 20        -- the 20 after the last one
notes <- getEntities h (map dE page1)                                            -- what they hold, from the same khana
```

It walks one attribute's index from its high end and stops after *n* facts (Datalevin's `rseek-datoms` with a count), so what it costs does not grow with the number of facts there are.
On 200,000 notes (Datalevin in the JVM, medians of 20 runs with the count or the start changed each time): the first 20 took 0.05 ms, 20 from the middle 0.04 ms (at 20,000 notes, 0.15 ms and 0.11 ms);
the query `:order-by [?ts :desc ?e :desc] :limit 20` on the same notes took 92 ms (9 ms at 20,000). Add the trip to the khana: a fraction of a millisecond with the JVM, a process start with the native `dtlv`.

* **The order:** the largest value first and, among equal values, the largest id first (an id is made from its creation time, so it grows). It is the order a query with `:order-by [?v :desc ?e :desc]` gives (a test holds the two together).
* **The cursor is a place, `(value, id)`:** a fact that need not exist. The answer is what comes after it in that order, and not the place itself, so the value and id of the last fact of a page is the cursor of the next.
  (Datalevin applies the id of a start to every value of the range, and not to the first only, so the start is given as the value alone and what the cursor has passed at that value is dropped. A value that many facts share costs as many reads as it has facts;
  when they are more than the page it asks again for twice as many, in the same call and the same snapshot.)
* **No filter.** It cannot be given "only these authors" or "this month". For "only these keys" (the people one follows) there is `highestIn`, next. For any other filter, ask for more than you need and drop what fails, or use a query over a window.
* **The attribute has to be in the schema with a type** (`TLong`, `TString`, ...). Datalevin keeps an attribute with no type in an order of its bytes that is of no use (tried: 200, 5, 40, -3 came out in that order), so `TAny` is refused, and so is an attribute that is not in the schema.
  A tuple is refused (its order was not tried). The cursor's value has the attribute's type. A refused call is a `BadArgument` (`bad-argument` at the C door), and the count is 1 or more.
* The order is Datalevin's for the type: numbers by size (doubles with their sign, tried), strings by their bytes (a long string was not tried). A many-valued attribute has one place per value.
* **Through the C door** it is `highest`: `{:op :highest :args {:attr :note/at :limit 20 :after {:val 1700000000000 :id 5678}}}`, and the answer is `[{:val .. :id ..} ...]`: give the last one as `:after` for the next page. In Elixir, `Hosi.highest(db, :"note/at", 20, after: place)`.

### `highestIn`: the newest *n* of some keys

"The newest 20 notes of the people I follow." The facts are *some keys'*, not the whole attribute, so `highest` does not do it. `highestIn` does: it walks a short run for each key and merges them.

```haskell
-- the schema: the notes' author and time, and the pair of them, which the khana keeps
schema = M.fromList [ ("note/by", attr TString), ("note/at", attr TLong)
                    , ("note/by+at", tuple ["note/by", "note/at"]) ]

let keys = map VStr followed
    place d = case dV d of VTuple [_, Just t] -> (t, dE d); _ -> error "a pair"     -- (the order value, the id)
page1 <- highestIn h "note/by+at" keys Nothing 20                  -- [Datom id "note/by+at" (VTuple [Just author, Just time])], newest first
page2 <- highestIn h "note/by+at" keys (Just (place (last page1))) 20   -- the 20 after the last one
```

* **The attribute is a pair `(key, order)`** of two attributes of one entity, in the schema with a type: the key is of any type but `TAny` (a string, a number, a reference: an author is an entity, so `ref` works), and **the order part has to be a `TLong`** (a time, a number). A pair that is not so is a `BadArgument`.
  The answer is the facts of the pair attribute: `VTuple [Just key, Just order]`, so the author and the time are in it, and `getEntities` gives the rest.
* **The order and the cursor are `highest`'s:** the largest order value first and, among equal values, the largest id first, *whichever key they are under*. The cursor is `Just (order value, id)` of the last fact of the page before (the place, which is not in the next page).
  Two notes of one author at one time are told apart by the id, and are neither lost nor doubled between pages (tested with ties, in pages of 1, 2, 7 and 100).
* **The keys:** a list, in any order. A key that has no facts, and a key given twice, are harmless. A key that is not of the key part's type is refused. An empty list gives an empty answer.
* **What stays out:** an entity that has the key and not the order (or the order and not the key) has a half-made pair, and it is not in the answer.
* **What it costs** is one seek for each key and one for each fact it gives, and so it does not depend on how many notes those keys have. Walking `highest` and dropping what is not the keys' costs a read for every fact newer than the last one that passes:
  nothing when the keys are busy, the whole attribute when they are quiet (a walk of that kind, measured in Datalevin alone on 200,000 notes and 400 keys: 0.7 ms and 900 ms; that run did not go through hosigraph).
  **Through hosigraph**, with the JVM khana (Apple M4; 200,000 made-up notes; the newest 20 of the first *k* keys; medians of 15 calls with the count changed each time, since Datalevin keeps answers; `Hosi.query` with `:order-by [?ts :desc ?e :desc] :limit 20` is the same question asked as a query, and gave the same facts every time):

  | notes for each key | keys | `highestIn` | the query |
  |---|---|---|---|
  | about 40 (5,000 keys) | 10 | 5.9 ms | 5.9 ms |
  | | 100 | 5.7 ms | 12.6 ms |
  | | 400 | 6.4 ms | 28.5 ms |
  | | 1,000 | 15 ms | 65 ms |
  | 400 (500 keys) | 10 | 2.8 ms | 11 ms |
  | | 100 | 2.4 ms | 50 ms |
  | | 400 | 3.9 ms | 240 ms |

  The query's time follows the notes the keys have (about 1.5 µs a row, and it sorts them all); `highestIn`'s follows the keys (about 10 µs a key) over **a floor of 2 to 6 ms** for every call.
  So with few keys that wrote little (10 keys, 40 notes each) the query does as well, and the more the people have written, the more `highestIn` is for.
  Where the floor goes was not looked into. The same walk written as compiled Clojure took 0.27 ms in a spike, so it is probably the script, which the khana reads and runs afresh for each call.
  With `KhanaDtlv`, every call starts a process, and both are tens of milliseconds (400 keys, 40 notes each: 29 ms against 55 ms).
  The program that took these numbers is not in the repository (a small one over `openHosiWith`, `write` in batches of 5,000 notes, `highestIn` and `query`).
* **A filter that is not "the key is one of these"** (hidden, muted, followers only): ask for more than you need, drop what fails, and go on from the last fact you looked at. It was not measured how that does when the filter drops most of what it sees.
* **What the tuple costs at write** (one more datom and one more index entry for each note) was not measured.
* **Through the C door** it is `highest-in`: `{:op :highest-in :args {:attr :note/by+at :keys ["u1" "u7"] :limit 20 :after {:val 1700000000000 :id 5678}}}`, and the answer is `[{:key "u7" :val 1700000000000 :id 5678} ...]`; give the last one's `:val` and `:id` as `:after`. In Elixir, `Hosi.highest_in(db, :"note/by+at", ["u1", "u7"], 20, after: place)`.
  Only the shape of the call was tested there, on a small world; the times above are the Haskell call's.

### Tuples: `tuple`

```haskell
schema = M.fromList [ ("follow/from", ref), ("follow/to", ref)
                    , ("follow/pair", identity (tuple ["follow/from", "follow/to"])) ]
```

`follow/pair` is the tuple of the other two (a *composite tuple*, Datalevin's). The khana keeps it up to date as they are written; you do not write it.

* **A tuple is never written.** `Create … "follow/pair" (Lit (VTuple …))` is a `BadArgument` before anything is logged, and any other value written to it is a `WriteRejected`, which leaves nothing in the log either.
* **Reading:** `getEntity` shows it as `VTuple [Just (VInt 12), Just (VInt 34)]` (`Nothing` for a part that is not there). It can be asked for in the query text, `[?e :follow/pair [12 34]]`, or as an input, `[InScalar (VTuple [Just (VInt 12), Just (VInt 34)])]` with `[?e :follow/pair ?p]`.
* **With `identity`, a pair is held once:** a second entity with the same pair is refused (`WriteRejected`), so "follow" can be written without looking first. **Write the parts of one entity in one batch.** While a part is missing the tuple is `[a nil]`, and two entities that are alike so far have the same tuple, so the second write is refused.
* A tuple holds plain values, not a tuple, and has one value (not `many`). Datalevin's other tuples (a tuple of one type) are not exposed.
* **Through the C door** a tuple is a vector with `nil` for a part that is missing (`[12 34]`). It is read (`get-entities`, the rows of a query), and asked for in an input (`[:scalar [12 34]]`) or in a condition of `write`; it is not written there either.
* The parts are in the schema mark: another list of parts is another schema, and the khana is made again at the next open.
* `erase` and `clean` take it with its parts. A pull of a tuple attribute was not tried.
* **A tuple `(key, order)` is what `highestIn` walks:** the newest facts of some keys, merged. See "`highestIn`".

### Words: `fulltext`

```haskell
schema = M.fromList [ ("note/body", fulltext (attr TString)) ]
-- [:find ?e ?v :in $ ?q :where [(fulltext $ :note/body ?q) [[?e ?a ?v]]]]      with InScalar (VStr "fox")
```

* **Without a tokenizer, only with `KhanaDatalevinJvm`.** The words are made by an analyzer of our own, a function that Datalevin keeps in the database, so it has to be an `inter-fn`. The JVM makes one; `dtlv` 1.2.11 cannot (tried in its repl:
  a plain `fn` is refused with "Can only freeze an inter-fn", and `inter-fn` itself stops at "Unable to resolve symbol: datalevin.interpret/compile-inter-fn-source").
  Its own default analyzer does work (Latin words found in any case, a Hangul word found whole), but it does not cut Han, kana and hangul into pairs: 東京 does not find 東京都, nor 문장 find 문장입니다. Those would be other words, in files that another backend opens, so it is refused rather than made different. With the native `dtlv`, whether `KhanaDtlv` or `KhanaDtlvRepl`, the schema is refused at open (`SchemaRefused`, before anything is made) unless you give a tokenizer: "Words of your own", below.
* **What a word is:** a run of letters and digits, lower-cased. A run of Han, kana or hangul is cut into overlapping pairs of characters, so that it is found by any two of its characters in a row. (A search for one such character on its own does not find it.)
  This is one analyzer, fixed; nothing about stems, stop words or a language was tried. Another analyzer will be another version (`words/1` is in the schema mark), and the khana is made again under it.
* The query is Datalevin's `fulltext`: with the attribute, `(fulltext $ :note/body ?q)`; without it, `(fulltext $ ?q)` searches every fulltext attribute; `{:top 5}` is an option (a last argument). Several words are matched as Datalevin matches them (best first); a phrase search was not tried.
* It follows the data: a value that is taken out or changed is out of the index, and an erased note is found no more.
* **`clean` always makes the khana again from the events** when the schema has a `fulltext` attribute (`crKhanaRebuilt` is `True`). The quick way, a compacted copy, **keeps the words of an erased value** (tried: an erased word was still in two files of the copy). The cost is the cost of `rebuildKhana`.

#### Words of your own: a `Tokenizer` (experimental)

```haskell
cfg = defaultConfig
  { hcKhanaBackend = KhanaDtlvRepl "/usr/local/bin/dtlv"
  , hcTokenizer    = Just myTokenizer }       -- a Tokenizer { tkMark, tkWords, tkClose }
```

With a tokenizer the words are made by you, outside Datalevin, so **any khana can have a `fulltext` attribute**, the native `dtlv` too, and the words can be the ones your language needs (the analyzer of the JVM cuts Han, kana and hangul into pairs of characters, which is not Japanese). `hcTokenizer` is `Nothing` by default, and then nothing here happens: the JVM has its analyzer, and the native khana refuses the schema.
[`example/hosi-mikke`](../example/hosi-mikke/README.md) makes one for Japanese from [Sudachi](https://github.com/WorksApplications/sudachi.rs), and is the way to see what it does on real posts.

A `Tokenizer` is three things:

* `tkWords :: [Text] -> IO [Text]`: the words of each text, joined by a single space, one answer for each text in the order given (`""` for a text with no words). It is asked for what is written and for what is searched, so a search is cut the way a text was. It may throw: a write is then rejected (`WriteRejected`, nothing is written) and a query fails with it.
* `tkMark :: Text`: says which words this makes (the rules, the dictionary, the way of cutting). It is in the schema mark, so **another mark makes the khana again from the events** at the next open; keep it the same while the words are the same, and change it when they are not.
* `tkClose :: IO ()`: let go of what it holds (a process). The runtime calls it when it closes: `closeHosi` when the `Hosi` made the runtime (`hcTokenizer`), `closeKhanaRuntime` when you made one with `newKhanaRuntimeWith`. A tokenizer that starts again when it is used again can be given to the next world.

What hosigraph does with it:

* **The events and the text are as written.** A `fulltext` attribute `note/body` is kept by the khana as an ordinary string attribute, and beside it `note/body.words` holds the words of that text, joined by a space; that one is the fulltext attribute, with Datalevin's default analyzer (which cuts at the space). A schema that has an attribute with that name is refused (`BadArgument`). The words follow every write (`Create`, `Delete`, `erase`), and `rebuildKhana` makes them again from the events.
* **A search text is cut the same way**, in the query: a string written in it, or a scalar `:in` input (`[(fulltext $ :note/body "猫") …]`, `:in $ ?q`). Any other form of the text is refused. Both forms of the call (with an attribute, and `(fulltext $ ?q)`) work, with `{:top 5}` too.
* **`clean` makes the khana again**, as it does for the JVM (see above).
* **Several words are AND, within the best `:top`.** A text that the tokenizer cuts into several words finds the entities that have **all** of them, in any order, as whole words (`dog` is not found in `dogma`; a word said twice is one word). Datalevin's own search finds the ones that have *any* of them, so the call is sent as it is, with its `:top` (Datalevin's own: the best 10 when none is written), and the same query checks what it found: the words of the entity come back as `?v`, and one clause for each word asks whether it is one of them. So the answer is **the ones of the best `:top` (by Datalevin's relevance) that have every word**:
  a small `:top` can give fewer than it, or none, while more entities that have every word are further down; a `:top` as big as the number of entities gives all of them (and then every entity that has any one of the words is scored: 1.2 s to 2.7 s on 50,000 notes with the native `dtlv`). One word is as before.
  A word that Datalevin's analyzer drops from a search (an English stop word, `at`) is still asked for, since it is a word of the tokenizer. The call must be a clause of the query itself (inside `or`, `not` or any other form it is refused, not searched as if it were one word), and its rows `[[?e ?a ?v]]` (any of the three may be `_`, and a shorter row is fine; the checks add the variables `?fulltext-v1`, `?fulltext-p1`, ... to the query).
* **What the call gives back:** `?a` is `note/body.words` and `?v` is the words (`"猫 動画"`), not the text; ask for the text with `[?e :note/body ?t]`. An `:in` input used by a `fulltext` call is the words in the whole query, not only there. `note/body.words` is also seen in `entity`, `datoms` and the counts.
* **The default analyzer still works on the words:** it seems to drop English stop words (a search for `on` found nothing in a text that had it, `and onward`). The list was not looked at. (With several words, a stop word is still asked for: see above.)
* **Not done:** the C door has no option for it (`:khana` and `:schema` only, no tokenizer); with `openHosiShared` the runtime that was lent keeps its own tokenizer and `hcTokenizer` is not looked at.
* **Tried** with `KhanaDtlv` and `KhanaDtlvRepl` (`dtlv` 1.2.11), by tests with a tokenizer made for them and by the Sudachi one of the example on 4,602 real notes. With `KhanaDatalevinJvm` it is written and the tests for it are written, and **were not run** (no JDK here).

## Where "now" lives

The khana is a Datalevin database. There are three ways to reach it:

| | `KhanaDatalevinJvm java jar` (default) | `KhanaDtlvRepl path` | `KhanaDtlv path` |
|---|---|---|---|
| What it is | Datalevin in **one JVM that stays up** (one JVM for each `Hosi`) | Datalevin in **one native `dtlv repl` that stays up** (one process for each `Hosi`) | Datalevin through the `dtlv` command, **one process for each operation** |
| One operation | about 0.6 to 0.9 ms (the first call: 1.3 to 1.6 s to start) | about 0.1 ms to read, 0.25 ms to write (the first call: about 30 ms to start) | about 25 ms (the first call: about 0.4 s) |
| What you need | JDK 21 or newer, and the jar (about 93 MB) | one `dtlv` file (tried with 1.2.11) | one `dtlv` file |
| Write-ahead log | yes (relaxed) | yes (the same) | no |
| `(max ?x)` and `(min ?x)` | work | work | work with `dtlv` 1.2.11; **do not** with 1.2.10 (see "Limits") |
| `fulltext` | yes | with a tokenizer | with a tokenizer |
| Files in | `khana-dtlv/` | `khana-dtlv/` | `khana-dtlv/` |

The numbers of the JVM and of `dtlv` are from 2026-11-16 and the repl's from 2026-11-22 (another program, `dtlv` 1.2.11): they are not side by side. See "Measurements".

```haskell
cfg = defaultConfig { hcKhanaBackend = KhanaDtlvRepl "/usr/local/bin/dtlv" }
cfg = defaultConfig { hcKhanaBackend = KhanaDtlv "/usr/local/bin/dtlv" }
cfg = defaultConfig { hcKhanaBackend = KhanaDatalevinJvm "java" "/path/to/datalevin-1.2.10-standalone.jar" }
```

* If `dtlv`, `java`, or the jar is missing, `openHosi` stops with `BackendMissing` **before it touches anything**.
  The default expects `datalevin-standalone.jar` in the place where you run, so give the real path.
* To switch from one khana to another, just fold the events into the new one. The tests do this in both directions.
* The Datalevin khanas read the same files, so they must not be open at the same time. Through hosigraph they cannot be: a world is open once at a time (see "One opener at a time" under Limits).
  A program that opens `khana-dtlv/` on its own is not stopped by that, and hosigraph does not check the LMDB lock.
* `KhanaDtlv` sends the code on standard input to `dtlv exec`. A write is one `transact!`, so "all or none" holds.
* `KhanaDtlvRepl` starts `dtlv repl` at the first call and keeps it. It sends one form at a time (one call at a time, under a lock) and reads one line back;
  the form prints a string, so what comes back is one line however many newlines the value has. The connection stays open in the repl, so a call does not open and close the database.
  A value with a newline, the word `user> `, a lone parenthesis, a thousand characters or unicode in it comes back whole, and 20,000 facts go through in one write (tested).
  `closeHosi` closes the connection and ends the repl. **If your process ends without `closeHosi`, the repl stays:** a `dtlv repl` with a database open was still there ten seconds after its input closed (it leaves at once when the connection was closed first), so it has to be killed by hand.
  If the repl dies, that one call fails (a write fails with `WriteRejected`), and the next call starts a new one. Nothing is lost, because the khana can be built again from the events. This was tried with `kill -9`.
  What the interpreter itself refuses (a form it cannot read) is said on standard error; hosigraph returns the first lines of it as the error.
  **A call has no time limit**, as with the JVM: a repl that is alive and says nothing is waited for.
* `KhanaDtlvRepl` opens the khana with the same write-ahead log as the JVM (the same options) and writes with `transact-ack!`; `txlog/` is made, and the compacted copy that `clean` makes is taken through the repl too.
  An erased value stays in `txlog/` until `clean` and is in no file after it (the same test as the JVM's). Measured, a write is 0.235 ms with the log and 0.37 ms without (about 1.6 times; the JVM's is about 3 times).
  **The repl killed with `kill -9` after 200 acknowledged writes kept all 200** (three times). **Not tried: the power going off, or the operating system crashing**, with the relaxed profile.
* `KhanaDatalevinJvm` talks to the JVM one line at a time. `closeHosi` ends it, and so does the end of your process.
  If the JVM dies, that one call fails (a write fails with `WriteRejected`), and the next call starts a new JVM.
  Nothing is lost, because the khana can be built again from the events.
* `KhanaDatalevinJvm` (and, as above, `KhanaDtlvRepl`) opens the khana with Datalevin's **write-ahead log in the relaxed profile** (small segments, small retention), and writes with `transact-ack!`.
  A write waits for the log once, not for the log and LMDB's own sync as well; measured, one write per transaction is about 3 times faster (batches, `writeMany`, are the same speed).
  If the process dies, the khana may lose its last few writes; the folded seq is in the same transaction as the facts, so the next open sees that the khana is behind the tail log and folds the rest in.
  `KhanaDtlv` is left as it was: one process per operation, with `transact!`.
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
* With `KhanaDtlv`, a read made at the very moment another call writes can fail to open the
  database. The lock only orders the writers; it does not stop an outside read. (The JVM backend was not tried for this.)
  `KhanaDtlvRepl` asks its one process one thing at a time, so two calls of hosigraph do not meet in two processes; the tests of `writeIf` run on it and pass.

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

Taken on **2026-11-16**: an Apple M4 with 16 GB, macOS (arm64), the world on the internal SSD, GHC 9.10.3 (`-O1`), Datalevin 1.2.10 (JVM: Temurin 21), `fsync` on.
One program does it all, **`scripts/bench.hs`** (build it with `scripts/build-ghc.sh bench`; it is not part of the cabal package). Run it alone on the machine: the numbers are times.
The numbers are one run on one machine; they say how big a thing is, not how fast it will be on yours.

**What a call costs** (an item is a `write` of 2 facts, a name and an age; 200 of each):

| | `KhanaDatalevinJvm` | `KhanaDtlv` |
|---|---|---|
| `write`, one item at a time | 1.05 ms | 24 ms |
| the same items through `writeMany` (for each item) | 0.06 ms | 0.14 ms |
| `getEntity`, one at a time | 0.66 ms | 24 ms |
| `getEntities`, 200 together | 2.7 ms | 26 ms |
| 6 queries (one by one → `queryMany`; each asked once before, so it is not the first) | 2.0 → 1.0 ms | 160 → 30 ms |
| `stats` | 1.0 ms | 24 ms |

* **Grouping helps `KhanaDtlv` most**, because starting a process is almost all of its time (200 entities: about 180 times faster; 6 queries: about 5 times).
  A JVM call is cheap already (about 50 times; about 2 times).
* The fsync of the tail log makes almost no difference on this Mac (under 0.1 ms; not part of the run above). hosigraph calls only `fsync(2)`,
  and on macOS that does not wait for the disk write cache. On systems that really wait, the time is higher. It is not measured.

**`KhanaDtlvRepl`** was measured on **2026-11-22**, on the same Mac, with `dtlv` 1.2.11 and **not by `scripts/bench.hs`**, so it is not in the table above.
A small program over `openHosiWith` (not in the repository): a world of 1,000 people, then one call at a time through `Hosi`, median of 500 for the repl and of 60 for `dtlv`, `fsync` on.

| | `KhanaDtlvRepl` | `KhanaDtlv` |
|---|---|---|
| `write`, one person at a time | 0.235 ms (0.37 ms without the write-ahead log) | 23.1 ms |
| `getEntity` | 0.106 ms | 22.4 ms |
| `[:find (max ?a) .]` | 0.098 ms | 25.1 ms |

* The slowest of the 500 calls took 2 to 8 ms; the 95th percentile is under 0.35 ms.
* It starts in about 25 to 30 ms (263 ms the first time, when `dtlv` was not in the file cache); that is from a small probe, not through hosigraph.
  In that probe the process held about 53 MB after it opened a database, and about 108 MB after 1,000 writes and 140 point queries. The JVM's memory (below) is from another world; none of these were taken side by side.
* The JVM was not run in this measurement (there was no JDK 21 on the machine), so the table does not say how the repl and the JVM compare.

**What a world costs.** A made-up world: 25,000 entities of 4 facts each (name, age, score, tag), so **100,000 events**, written in groups of 500 by `writeMany`:

| | `KhanaDatalevinJvm` | `KhanaDtlv` |
|---|---|---|
| the writes | 3.1 s | 2.6 s |
| open, the khana is in step | 1.5 s | 0.1 s |
| open, the khana is 100 batches behind | 1.5 s | 0.1 s |
| **open, the khana is made again from all the events** | 2.8 s | 2.3 s |
| memory of the khana's program | 260 MB after an open, 400 MB at most | 90 MB at most (one process for each call) |
| memory of hosigraph itself (GHC) | about 50 MB | about 50 MB |
| on disk | events 1.4 MB, khana 11 to 22 MB | events 1.4 MB, khana 8 MB |

* Opening with the JVM takes about 1.4 s even when nothing is to be done: that is the JVM starting.
* **Real data is much slower to make again than this.** A copy of a real server's data (527,971 events, 54,402 entities, 288 attributes, 53 chunks, 96 MB, 2026-11-07), one run each:
  with `KhanaDtlv` the khana was made again in **34 s** with the world on the Mac's internal disk (with the world on an external SSD: 71 to 111 s, about 0.13 ms for each event, against 0.02 ms above),
  and with the JVM in **7 s** (an open that has to do it: 10 s). The same again when the schema changes.
  Plan the worst open for `KhanaDtlv` as **events × 0.15 ms** (the slower disk) and for the JVM as **events × 0.02 ms**; use these, not the made-up number.
* **`verify` makes a second khana and compares the two a page at a time** (10,000 facts, whole entities), so it costs a rebuild and a walk through both, and what it holds is a page, not the khanas.
  On the same real data: `KhanaDtlv` **65 s**, and 165 MB for the largest process (`time -l`; hosigraph is 157 MB of it); the JVM **22 s**, and it passes with `-Xmx256m` (23 s).
  A JVM given no limit takes up to a quarter of the RAM: 870 MB of it was resident here, and that is the JVM's choice, not what `verify` needs.
  Each page is asked from Datalevin with a count (`seek-datoms` with `n`). Without the count, Datalevin makes every fact after the start before anything is cut (read in its source), and the JVM ran out of 2 GB.
  A page of 50,000 facts held 605 MB in hosigraph, which is why the page is 10,000.
* The memory of `KhanaDtlv` is the largest of its processes, from `time -l` over the whole run; sampling every 25 ms misses a process that lives for 25 ms (the sampling saw 40 MB, and `time -l` saw 89 MB).
  On a made-up world of 100,000 *people* (2026-11-13) one `dtlv` took about 260 MB, and the JVM about 300 MB resident that a smaller heap did not remove. The memory depends on the data.
  The JVM is started with no heap setting.
* A world of 25,000 entities is **small**. Nothing here was measured above 530,000 events.

## Limits

* `query` does not accept `InRules`, because a `Rule` is a structure and not EDN text. Write the rules inside the query.
* **`dtlv` 1.2.10 cannot do `(max ?x)` or `(min ?x)`** (`KhanaDtlv`; `KhanaDtlvRepl` runs the same interpreter). In `dtlv exec` 1.2.10, they return the collection of values, not the maximum or the minimum.
  They seem to resolve to `clojure.core/max` and `min`. `count`, `sum`, and `avg` are correct, and the same jar on a JVM is correct too.
  So the problem is in the interpreter of the native `dtlv`. hosigraph returns an error and does not read the result.
  **`dtlv` 1.2.11 does them right** (tried through `KhanaDtlv` and `KhanaDtlvRepl`; the test of the repl asks for both). The canary test, which expects the 1.2.10 answer, fails against 1.2.11: that is how you are told.
* The query parser refuses aggregates such as `distinct` or `(max 2 ?x)` before they reach Datalevin, so no backend can send them.
* NaN and ±Infinity have no EDN form, so hosigraph refuses to write them.
* The order of rows is not guaranteed (a Datalevin answer is a set), **unless the query has `:order-by`** (see "Order and cut"). Sort the rows before you compare them otherwise.
* Tested only with Datalevin 1.2.10 on macOS arm64 (JVM: Temurin 21). Its numbers (calls, opens, memory) are in "Measurements", for one machine.
* **One opener at a time.** A world (a directory) is open in one place at a time: the first `openHosi` takes a lock (`events/lock`, `flock(2)`) and keeps it until `closeHosi`;
  a second one, in this process or in another, stops with `WorldBusy` before it touches anything.
  The lock is let go when the process dies, however it dies; a `lock` file left behind means nothing.
  It keeps two writers from overwriting each other's records in the tail log.
  Not promised: a directory on a network file system (`flock` may say yes to two machines), and Windows (there is no `System.Posix` there).
  This is for writers and readers alike: **in 0.2 a world has one opener, and a second process cannot even read a world that the first one has open.**
  A lock that readers share (for a backup, or an inspector) is not in 0.2; it is in `TODO.md`. To read a world that is open elsewhere, copy `events/` while its owner is closed, or ask its owner.

## Errors

A `HosiError` says which kind it is, so that you can answer it without reading the text. The C door tells the same kind as `{:error :kind ...}` (and as `HOSI_ERR_APP`).

| `HosiError` | C door kind | What it says | What you do |
|---|---|---|---|
| `WriteRejected` | `write-rejected` | the batch was refused (for example a unique value that is taken); nothing was kept | change the batch |
| `BadArgument` | `bad-argument` | the call itself is wrong: an empty batch, an id that is not an entity, a range that ends before it starts | change the call |
| `Corrupt` | `corrupt` | a file does not match what its hash or format promises | stop; look at the world with `verify` |
| `WorldBusy` | `world-busy` | the world is open already, in this process or another | close the other one, or try later. Nothing was touched |
| `BackendMissing` | `backend-missing` | `dtlv`, or `java` and the Datalevin jar, is not where the config says | install it, or fix the path |
| `SchemaRefused` | `schema-refused` | the khana cannot be made under this schema (a type that clashes with the data, a unique value that is not unique); nothing was changed | open again with the schema the world was made with |

A khana that fails while it runs (the `dtlv` process, the repl or the JVM) is not a `HosiError` yet. It is a `DtlvError`, a bare string, and the C door tells it as `HOSI_ERR_INTERNAL`.
So a backend that has failed cannot be told from a query that the caller wrote wrongly and that reached the khana (a Haskell caller's `query`; the C door reads the query first and refuses it as `parse-error`). Making it a kind of its own is in `TODO.md`.

## What 0.1 promises

hosigraph 0.1 was, and 0.2 still is, **an embedded database for one process on one machine, on a local disk.** It brings Datalevin along
(the `dtlv` program, or JDK 21 and the standalone jar), and the README says so on its first screen.
**What 0.1.0 wrote, every 0.1.x and 0.2.x reads.** The names listed as stable stay. The rest may change.
0.2 asks more of the khana (an order, tuples, words) and **nothing more of the files**: the fixture of 0.1.0 is opened by every build, and a schema that uses none of the new things is the schema it was, so its khana is not made again.
The reason for this size: "local, one process" can be promised now. S3 brings failures of the network, which is a different promise (0.3).

### The files

| File | What is promised |
|---|---|
| `events/manifest` | `hosi-manifest/1` and `/2` are read by every 0.1.x and 0.2.x. A format this code does not know is refused (`Corrupt`), and nothing is touched. |
| `events/chunks/*/chunk.hosi` | `HOSICHK1`, **versions 1 and 2**, are both read (a world that began before 0.1 has version 1). New chunks are version 2. A later layout will have a new version number, not a changed meaning. |
| `filter.bin`, `mphf.idx` | Version 1 of each. A chunk may have no `filter.bin` (it is read without it) or no `mphf.idx` (a cold chunk). |
| `events/erase/*.bin` | `HOSIERS1`. |
| `events/tail.log` | Records of `length:4  crc32:4  payload`, with **no header**. The first four bytes `HOSI` are reserved: no record of this format starts that way (read as a length, it would be 1.1 GiB), so a later format may begin with a mark that starts with them. 0.1.x and 0.2.x **refuse** such a file (`Corrupt`) and do not cut it. A **torn end** (a crash in the middle of an append: the last record is short, or does not match its CRC) is cut at the open. A bad record with a **whole record after it** is not a torn end but damage: the open is refused (`Corrupt`, with the byte where the bad record begins), and the file is not cut. To open anyway, keep a copy and cut `tail.log` at that byte (what follows is lost). A damaged *last* record cannot be told from a torn one, and is cut. |
| `khana-dtlv/` | **Nothing.** It is derived. A khana that is missing, behind, or made under another schema is made again from the events. |
| `events/access.json`, `events/lock` | Nothing. A hint, and a lock; both may be missing. |

A test holds this. `test/fixtures/world-0.1.0/` is a small world that 0.1.0 wrote (made by `spec --make-fixture DIR`), with chunks of both versions, a cold chunk, an erase record, and a tail that is not sealed.
Every build opens it, and must show the events listed in `expected.txt`, and (with each khana) answer for each person as `expected.txt` says.
**If a change makes that test fail, it is a change of the format.** It needs a new version number and a reader for the old one. Do not make the fixture again to make the test pass.

Not tried: a world written by a build *older* than 0.1.0 (0.0.1.0). Its chunks are version 1, which the fixture has; its tail has the same shape. It was not opened with a real one.

### The names

`Hosigraph` lists them in the export list, in groups. Those **before the heading "Experimental"** are stable: within 0.2.x they are not removed, and what they mean does not change.
New names, new fields in `HosiConfig`, in the reports and in `AttrSchema`, and new constructors of `HosiError` and of `Value` may come. So: build a config with `defaultConfig { … }`, and keep a catch-all case when you match an error.

| | Names |
|---|---|
| **Stable** | Opening (`Hosi`, `HosiConfig`, `defaultConfig`, `openHosi`, `openHosiWith`, `closeHosi`, `withHosi`, `KhanaBackend`); writing (`Op`, `Ref`, `Val`, `write`, `writeMany`, `writeIf`, `Cond`, `seal`, and their reports); `erase`, `clean`; reading now (`getEntity`, `query`, `queryCells`, and the `…Many` forms); `history`; `rebuildKhana`, `verify`; ids; `HosiError`, `Event`, `Kind`; the words of the khana that `Hosigraph` re-exports. |
| **Experimental** (may change, or go, in a 0.2.x; the changelog says so) | `freeze` (when a month is cold is not settled; the cold chunk's file is promised, the call that makes it is not); `historyTrace`, `LookupTrace`, `Probe`; `Stats`, `stats`; `Coverage`, `backfillSince`; `highest`, `highestIn` (an order that is not a long, and the other direction, may come; what they answer now is not meant to change). |
| **Not promised** | Every module other than `Hosigraph`. They are exposed so that the tests and tools can reach them. |

One exception: `Hosigraph.Events` **used alone** (no khana) is promised for `EventsConfig`, `defaultEventsConfig`, `openEvents`, `closeEvents`, `withEventsLock`, `record`, `recordMany`, `commit`, `rollback`, `seal`, `history`, `eventsAfter`, `lastSeq` and `verifyEvents`.
The rest of that module (erase, freeze, rewrite, access stamps, the pieces the khana folds from) is the inside of `Hosigraph`.
Whoever uses the events alone owns "now": there is no khana to keep in step with them.

The C door (`hosigraph-abi`, `libhosi`) is not covered here. It was seen working on macOS only, and it has its own documents.

### Not in 0.2

S3, live serving of a backfill, distribution and sync, metrics, JSON-LD import, thaw and compression of cold chunks. Windows, and a directory on a network file system.

## How the files are laid out

| Word in the design document | Form here |
|---|---|
| Tail log | `events/tail.log`. One `write` is one record (length + CRC32). If a record is cut, hosigraph keeps what is before the cut and removes the rest. |
| Hot chunk | `chunk.hosi`, `mphf.idx` and `filter.bin` in `events/chunks/<name>/`. One chunk for each kind and (creation) month. Nobody changes it. A chunk with no `filter.bin` is still read. |
| ID | `(milliseconds since 2026-01-01) << 21 \| sequence number` (63 bits). The ID alone tells you the month. |
| MPHF | A *minimal perfect hash*: it gives each known key its own slot, with no collisions. This one is PTHash-style and written for this project (`Hosigraph.Mphf`). Each slot has a 32-bit fingerprint and the position of the body, which `Probe` checks. |
| Lock | `events/lock`: empty. Held with `flock(2)` for as long as the world is open ("One opener at a time", under Limits). A file left behind means nothing. |
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

## The filter and the deep path

The design document says "S3 and the deep path. Zone maps, Binary Fuse filters, Range GET."
and "A question about the past skips the chunks that it does not need to read." Here are the zone map, the filter and the deep path; S3 is not here.

**The order of the gates.** `history` (and `historyTrace`) asks each chunk the cheapest question first:

1. the zone map in the manifest (month, ID range): no file is touched;
2. the chunk's erase record, if it has one (see "Erase, clean and freeze"): it is in memory, so an erased ID is turned away without a read;
3. the chunk's filter, `filter.bin`: one small file, read once and kept in memory;
4. only for a "maybe": open the chunk reader;
5. the MPHF slot, its fingerprint, and the key check.

`LookupTrace` has the fields `ltSkippedByErase` and `ltSkippedByFilter`. A chunk that the record or the filter turns away is **never opened**
(a test checks that `xsReadersOpen` stays 0). `verify` also checks that the filter never says no to an ID that is in the chunk.
(The design document puts the erase check last. Asking it first gives the same answer for less work; see item 9 of [`design-notes.md`](design-notes.md).)

**The deep path.** A reader takes a `ChunkSource` (`Hosigraph.Chunk`): "give me these bytes of this file",
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

**Not here.**

* **S3.** There is no S3 `ChunkSource`, no upload, and no manifest tier for a chunk that lives far away. `Events` only builds local sources. The plan is in [`TODO.md`](../TODO.md).
* The filters are kept in memory with no limit, outside `hcReaderCacheBytes` (about 2.3 to 2.8 bytes an ID). A bucket with millions of chunks needs a limit.
* Only a cold chunk (`freeze`) is written without `mphf.idx`; the deep path is reached by it, and by a source that does not have the file.

## Erase, clean and freeze

The design document says "Erase, cleaning, freezing". What is here is the **local** part: it needs no S3.
The S3 side (a remote chunk that must be written again, old object versions, the sweep of a bucket) is not here; see "Not here" at the end of this section.

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
`data.mdb` twice, in the native `dtlv` and in the JVM alike. LMDB reuses freed pages *sometime*, which is luck, not a promise. A plain copy keeps the bytes; a compacted copy has none.
With the write-ahead log of the JVM khana (and of the repl's) the bytes are also in `txlog/segment-*.wal` and in `snapshots/*/data.mdb`, and *stay there until `clean`* (retracting, writing more, closing: none of it removes them).
So the promise is the same, "after `clean` it is nowhere", but the time in between is different: without the log it was sometimes gone by luck, with it it is there for sure. The compacted copy has neither
directory, and the old khana directory is replaced as a whole, so the log goes with it.)
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

What to take from it: `freeze` saved **about 7 %** here, not "half". Its saving is the size of `mphf.idx`, and that is large only when an entity has few events (in the chunk of 4,000 entities with one event each under "The filter and the deep path",
the chunk without `mphf.idx` would be about half the size). With 4 events an entity the events are most of the bytes and `mphf.idx` is 10 % of them. How much a real world saves depends on how many events an entity has.
The cold read is slower for each id because it reads a block of 4,096 events and not 256; it is still microseconds. A first `history` after a reopen, with nothing cached, took 2 ms on the cold chunk.
`ownedBy` is a scan: 73 ms for 200,000 events says about 0.4 µs an event, while writers wait; if it grows in a straight line, a world a hundred times bigger would hold writers for several seconds. The numbers for `clean`'s second half
(the khana made new: a compacted copy, or a rebuild) were not taken at this size; in the tests the khana is small.

### Not here

* **S3.** A remote chunk that has an erase record must be downloaded, written again and uploaded, and the old one deleted, with its old versions, and a sweep of the bucket needs a grace period.
  `ObjectStore.delete` waits for this. The count of deletions still pending belongs in `CleanReport` and `Stats`. Where the upload happens (at seal, as [`TODO.md`](../TODO.md) item 5 says, or at freeze,
  which would save uploading a chunk that is soon replaced and deleted) is open. The local side is written so that the remote side plugs into `retire` and the rewrite.
* **Left out on purpose:** thaw, compression of cold chunks, merging cold chunks, a timer inside hosigraph, the other rules of freezing (the share of sequential reads, capacity pressure, free space),
  metrics and the capacity outlook (Phase 4), pruning horizons.
* **Not tested:** "the world changed between the read and the lock" in a rewrite or a freeze (it needs a second writer at an exact moment; the check reads right to me, but I have not seen it bite);
  stamping of use under real clocks and many readers at once; `clean` called from two threads at once.
* **How many chunks a world may have:** below 10,000 nothing was slower than 0.35 s in a probe; at 100,000 a seal took 1.2 to 1.7 s and the first `history` 14 s.

## Reading the past into a khana, newest first

`backfillSince` reads the events from the newest to the oldest into **a new khana beside the world**, only for the entities created in a given month or after,
and says after every step what can be asked. Use it to know "can I query yet?" before the whole khana is built, or to build only the recent part.

```haskell
import Hosigraph
import Hosigraph.Khana (khanaEntity, closeKhana)

(k, cov) <- backfillSince h "/path/to/a/new/directory" (2026, 10) $ \step khana -> do
  let c = bfCoverage step
  putStrLn (show (bfIndex step) ++ "/" ++ show (bfCount step) ++ " complete: " ++ show (cvComplete c) ++ " pending: " ++ show (cvPending c))
  khanaEntity khana someId >>= print          -- whatever is known about it so far
closeKhana k                                  -- here cvQueryable cov == True
```

Step 0 is the unsealed log. The next steps are the sealed groups (chunks whose seq ranges overlap), the newest group first.
After each step the callback sees the khana as it is, and a `Coverage`.

**What a step puts in.** The rule is one line: *an older event never overrides a newer one*. So:

| What was read | What the khana holds |
|---|---|
| an update of an entity whose Create is not read yet | the update, as it is: the entity shows with the facts known so far |
| a Delete of a value whose Create is not read yet | nothing now; when the Create comes, it is kept out |
| a Create of a one-valued attribute that a newer event has settled | nothing: the newer one stays |
| a many-valued attribute | each value is settled by its own newest event, a Create or a Delete |

A fact that is put in is never taken out later, so at any step the khana holds a part of what the finished khana holds, and never something else.
The tests check that reading newest first, in any step sizes, gives what folding oldest first gives (a property test), and cut each rule in turn to see a test fail.

**What `Coverage` says.** A chunk is made for the *creation* month of its entities, and an update or a delete goes to the chunks of that month (note 2 of [`design-notes.md`](design-notes.md)).
So when every chunk of a month has been read, every entity created in that month is whole, and a question about those entities is answered as the finished khana would.
`cvComplete` lists those months, `cvPending` the months with chunks still unread, and `cvQueryable` is "nothing is pending".
It is cautious: an entity of a pending month may already be whole, and the report does not say so. A question that follows a reference to an entity of another month may still meet a pointer to nothing.
Entities created before the month you gave are not read at all: their chunks are never opened, and they are not in this khana.

**What it is not.**

* It is not the khana that `getEntity` and `query` read. It carries neither the folded seq nor the schema mark, so an open never takes it for a whole khana. `dir` must not exist yet (`BadArgument` if it does).
  If a step fails (or the callback throws), the khana is closed and `dir` is removed.
* It holds the events lock for the whole reading, so writes wait, and the callback must not write to this `Hosi`. Serving it as the world's own khana while it is still being built ("live serving") is not built (note 10 of the design notes says what it needs, and [`TODO.md`](../TODO.md) lists the open points).
* The erased ids are left out, as a rebuild leaves them out.

**Measured** on the sukhi cold copy (527,971 events in 53 chunks, one month: they were all written when the world was loaded; `dtlv` khana): 54 steps, **106.6 to 109 s** (in the same run the world's own khana took 111 s to build from the events), the Haskell process peaked at **about 131 MB** (the `dtlv` processes are not counted), and the khana it made held the same 527,971 facts as the world's own (the sets are equal).
Almost all of the time is `dtlv` (one process for each transaction of 2,000 facts): the program's own CPU time was 2.8 s, and 3.5 s of garbage collection. This world has no updates or deletes, so the run says what the reading costs, not how a half-built entity looks; the tests do that.

## From other languages (`setu/`, `hosigraph-abi/`)

hosigraph is a Haskell library. To use it from Go, Python, Elixir or C, the repository has two more packages next to `takka/`.
Neither touches `hosigraph` or `takka`, and `hosigraph`, `takka` and `setu` still have no C.

* **`setu/`** (working name; Pali for "bridge") writes the doors other languages use, from one description of an API (`Setu.Spec.Api`):
  the operations, the shape of each argument and answer (`Ty`), and the error kinds. It is a set of pure functions from the description to text,
  so the files it writes are committed and `--check` tells you when they are out of date. It also holds what every door shares:
  the table of waiting requests (`Setu.Ticket`) and the table of operations (`Setu.Dispatch`).
* **`hosigraph-abi/`** is the first customer. `Hosigraph.Abi.Api` describes hosigraph (`write`, `write-many`, `get-entities`, `highest`, `highest-in`, `query-many`,
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

### `hosigraph stdio`

The same operations for a process that talks over a pipe. `hosigraph stdio [--json] [OPEN]` reads requests on stdin and answers on stdout, **one EDN line each**,
the same bodies as the C door (`{:op :write :args {...}}` in, `{:ok ...}` or `{:error :kind :message "..."}` out). `OPEN` is what `open` takes
(`'{:dir ".." :schema {..} :config {..}}'`), or `@FILE` to read it from a file; without it nothing is opened and only `:ping` and `:abi` answer.

* Answers come one at a time, in the order of the requests (use `write-many`, `query-many` to ask for several at once). A blank line is skipped, `\r\n` is a line end.
* A request that is not the contract is answered with an `{:error ...}` line and the process goes on. End of input closes the database and exits 0.
* If it cannot open, the one line of stdout is that error, stderr says it in words, and the exit code is 1. stdout holds answers and nothing else.
* The database can take a while to open (a big one needs its khana read first); the first answer comes after that.

```bash
cabal build hosigraph-abi:exe:hosigraph
printf '{:op :abi}\n' | $(cabal list-bin hosigraph-abi:exe:hosigraph) stdio
```

**JSON.** `hosigraph stdio --json [--ids-as-strings] [OPEN]` speaks the same contract with JSON lines, for a client that has no EDN reader. It is
a way of writing the same requests and answers at the door; nothing else changes. The shape of each operation (`Hosigraph.Abi.Api`) decides how a line is read:

```
{"op":"write","args":{"ops":[["create",["tmp",1],"person/name","Frege"],["create",["tmp",1],"person/score",3]]}}
{"ok":{"temps":[[1,58047583928451072]],"first-seq":1,"last-seq":2,"sealed":false}}
{"error":"conflict","message":"..."}
```

* An object is a map, an array is a vector, a tagged vector starts with its tag (`["create", ...]`, `["tmp", 1]`, `["ref", ...]`).
  A key and a keyword the shape asks for are bare strings (`"person/name"`, `"create"`).
* Where a *value* may be a string or a keyword, a string that starts with `:` is the keyword (`":active"`), and a string that starts with `:` by itself
  is written with one more (`"::x"` is the string `":x"`). Answers are written the same way.
* **1 and 1.0.** If `open` named the attribute, its schema type decides: `:double` makes `3` a double, `:long` and `:ref` accept a string of digits.
  For an attribute the schema does not name, a number written with a fraction or an exponent is a double and a whole number is an integer.
  An answer always writes a double with a fraction (`3.0`).
* **Saying the type yourself.** `{"type":"double","value":1}` is the double 1.0, whatever the schema says and whatever the language can write. The types are the
  names `open` uses: `double`, `long` (a number or a string of digits), `string` (so `":x"` needs no escape) and `keyword`. It is read wherever a value or a number
  may stand, and not where an object is a map of its own, so a map with a `"type"` and a `"value"` key is still a map there. What you wrote is not changed by the schema:
  a `long` given to a `:double` attribute is a `long`, and hosigraph's own check says so.
* **Big ids.** An integer is a number. JavaScript rounds a number past 2^53, and ids are past it (an id holds its creation time), so `--ids-as-strings` writes every
  integer past ±2^53 in an answer as a string of digits. A request may always give an integer as a string of digits where an integer is asked for
  (`{"ids":["58047583928451072"]}`), and for a `:long` or `:ref` attribute. The one thing it cannot tell: an integer past 2^53 in an attribute of type `:any`
  comes back as a string, like a string of digits that was written there; give such an attribute a type.
* The text of a query (`:q`) is still Datalog, a string.

```bash
cabal run hosigraph-abi-gen -- --out   hosigraph-abi/gen    # write the generated files
cabal run hosigraph-abi-gen -- --check hosigraph-abi/gen    # fail if they are out of date
cabal build hosigraph-abi:flib:hosi                          # libhosi
python3 -I hosigraph-abi/test/abi_conformance.py <path to libhosi>   # the rules, through ctypes, with a fake database
cabal test setu:spec hosigraph-abi:spec
```

`erase`, `clean` and `freeze` are in the description, so the door has them (see "Erase, clean and freeze").
The ctypes conformance does not call them (it uses a fake database); the Haskell tests do, through the dispatcher, over a real hosigraph.

What has been seen working: the Haskell tests above, and the ctypes conformance on macOS (arm64; the Haskell tests also pass on GHC 9.10.3). What has not:

* The operations against a real database. `hosigraph-abi:spec` runs them when `HOSIGRAPH_DTLV` (or `HOSIGRAPH_DATALEVIN_JAR` and `HOSIGRAPH_JAVA`) is set,
  as in CI; without one they are skipped, with a line saying so.
* Linux. CI has a separate job for it (`abi`), allowed to fail until it has passed once.
* A host that is not Python. The runtime cannot be started again after `hosi_runtime_shutdown` (GHC's RTS refuses), and signal handlers are never installed
  (`--install-signal-handlers=no`), so Ctrl-C stays with the host.
* Shipping. The dylib names GHC's libraries by absolute path; a copy for someone else's machine needs them gathered with it.

## What is inside a chunk

* **Each chunk has a value dictionary** (chunk format version 2). A value that the chunk says many times is written once in the header,
  and the runs say "number 17" instead (2-3 bytes). A value goes in only when that makes the chunk smaller:
  a value of `len` bytes that appears `n` times is worth it when `n * (len - 4) > len`.
  Values are told apart by their bytes, not by `==`, so `0.0` and `-0.0` stay two values.
  The dictionary is read when a chunk is opened and stays in memory with the reader (it is part of what `hcReaderCacheBytes` counts).
  Chunks of version 1 (no dictionary) are still read; nothing rewrites them.
* **Version 2 also writes numbers short.** Every number in a run is a variable-length one (LEB128, 1-10 bytes): the attribute number, the seq
  (counted from the chunk's first seq), the entity id (counted from the chunk's first id), the lengths, and integer values (zigzag, so `-1` is short too).
  A text value's length is one byte, not four. Point lookups work as before: the slot still points at the start of a run, and the run's head is
  read with one read of at most 30 bytes.
  On 20,000 made-up events (4,000 entities with a name, a type, an age, a language and a friend), `chunk.hosi` was 595,208 bytes in version 1
  and 205,466 in version 2, with no compressor (the dictionary alone took version 1 to 474,462; the short numbers did the rest).
  Invented data is only a hint: names made of a few repeated words, random friend ids.
  A general compressor on top does not make version 2 smaller than version 1 (xz -9: 72.6 KB vs 70.0 KB; zstd -19: 94.4 KB vs 91.1 KB),
  because it had been squeezing the same fixed-width zeros. `mphf.idx` was not touched (about 13 bytes for each id).
  The dictionary is per chunk; one for the whole khana, or one shared by IRIs from outside the ID space, is not built (`TODO.md`).
* The open chunk readers (each holds the chunk's slot table, 12 bytes per id) have a memory limit: `hcReaderCacheBytes`, 64 MiB by default.
  The least recently used reader is closed first; the one just used is always kept, so a single huge chunk still opens.
  The count is about the slot tables only (`Events.xsReadersOpen`, `xsReaderBytes` show it).

## Notes on the current state

* **Speed is measured only roughly.** Each write fsyncs the tail log and the khana commits once, so one-by-one writes are slow. The design expects grouped writes.
* `Schema` is an argument of open. The events do not store it, but the khana keeps a mark of the schema it was folded under: open with another schema makes the khana again
  (beside the old one; it costs a fold of every event, about 0.13 ms an event with `dtlv`), and a schema that the events or the log refuse stops the open with `SchemaRefused`,
  with the khana and the log untouched. See item 6 of [`design-notes.md`](design-notes.md).
* Writes go one at a time (`MVar`). Reads run in parallel.
  `history` takes the lock for a moment to see the manifest and the tail log in a stable state, so a seal in progress is not visible.
* The MPHF is a minimal PTHash style (load factor about 89 %, about 4 keys for each bucket, 16-bit pilots).
  The choice between PtrHash, PTHash, and RecSplit is open.
* `takka/` exports the types (`Value`, `Schema`, …), EDN, and the Datalog and pull syntax (`Takka.Types`, `Takka.Edn`, `Takka.Syntax`).
  hosigraph needs these to write to Datalevin and to read its answers back.
* **There is no in-process khana.** The khana is Datalevin, through `dtlv` (a process for each operation, or one `dtlv repl`) or a JVM.
  If Datalevin gets a native core of its own, it would be a new backend next to the ones above.
* The license is the Blue Oak Model License 1.0.0 (see [`LICENSE`](../LICENSE) and [`dependency-licenses.md`](dependency-licenses.md)).

## Open questions for the design document

[`design-notes.md`](design-notes.md) gives the details. Each item says what the prototype does and what it asks of the design document.

1. **A chunk also needs a sequence number (seq).** The fold needs the order of Create and Delete, and which Create is newer.
2. **The "month" is the month in which the entity was created, not the month of the event.** A Delete or update of an old entity goes into an old month's chunk. How should this meet the freezing unit?
3. **A khana plus a tail log cannot be read together** (a Datalevin `Db` is one LMDB read transaction). Instead, hosigraph folds into the khana at each write (write-through).
4. **Who decides whether a write is allowed?** A type error or a unique-key conflict shows only when hosigraph folds into the khana.
5. **There are two fast paths.** "Now" comes from the khana. "The events of one ID" comes from the MPHF.
6. **The schema is an argument of open; the khana keeps a mark of it.** The cardinality sets the meaning of Create, so it changes the fold; a different schema at open makes the khana again.
7. **The 63 bits of the ID have no room for a node ID.** Where does the room for distribution go?
8. **The parts have names that the design document does not have** (Events and Khana). Should the design document take these words?
