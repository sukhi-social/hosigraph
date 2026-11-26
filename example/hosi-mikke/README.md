# hosi-mikke

Japanese words for hosigraph's `fulltext`, from [Sudachi](https://github.com/WorksApplications/sudachi.rs). **An example, and experimental.** It is not a part of the `hosigraph` package; it uses hosigraph's public `Tokenizer` and is kept here to show how, and to keep what was tried.

*Mikke* is 見っけ, "found it!".

## What it is for

The `fulltext` of the JVM khana cuts Han, kana and hangul into overlapping pairs of characters. That finds a word of two or more characters, but not one character (猫, 雨), not 受付 when the text says 受け付け, not 描く in 描いた, and it finds 京都 inside 東京都.
The native khana (`KhanaDtlvRepl`) cannot run that analyzer at all, so they refuse a `fulltext` attribute.

A morphological analyzer cuts a text into words and gives each word a normal form (食べました → 食べる, 引越し → 引っ越し, ネコ → 猫). `Hosigraph.Mikke.Sudachi` makes a hosigraph `Tokenizer` from one resident `sudachi` process; with it **any khana has `fulltext`**, the native ones too, and Japanese is found by its words.

## Use

```haskell
import Hosigraph
import Hosigraph.Mikke.Sudachi

main = do
  tk <- newSudachi (SudachiSpec "/usr/local/bin/sudachi" "/path/to/system_core.dic" ModeA)
  let cfg = defaultConfig { hcKhanaBackend = KhanaDtlvRepl "/usr/local/bin/dtlv", hcTokenizer = Just tk }
      schema = M.fromList [ ("note/body", fulltext (attr TString)) ]
  h <- openHosiWith cfg "world" schema
  _ <- write h [Create (Tmp 1) "note/body" (Lit (VStr "うちの猫が毎朝起こしに来る。"))]
  query h "[:find ?e :in $ ?q :where [(fulltext $ :note/body ?q) [[?e ?a ?v]]]]" [InScalar (VStr "猫")]
```

What hosigraph does with a tokenizer (the second attribute `note/body.words`, the query rewritten, the schema mark, `clean`) is in [the guide](../../docs/guide.md#words-of-your-own-a-tokenizer); this README is about the Japanese part.

* **You bring two things.** `sudachi` ([sudachi.rs](https://github.com/WorksApplications/sudachi.rs), Apache-2.0; a `cargo build --release` gives it) and a dictionary ([SudachiDict](https://github.com/WorksApplications/SudachiDict); the `core` one is `system_core.dic`, 200 MB). Neither is in this package.
  If the program or the dictionary is not there, `newSudachi` throws `BackendMissing`, and nothing is started.
* **One resident `sudachi` process**, started by the first text that needs it (about 0.5 s; about 85 MB resident for a few short texts, and about 160 MB after thousands of real ones: the dictionary is mapped, and what is touched grows). One request at a time. If it dies, that request fails (`SudachiError`; a write is then rejected, with nothing written) and the next one starts a new process.
  The world owns the tokenizer it is opened with: `closeHosi` ends the process, and the same tokenizer starts it again if it is used again.
* **How it cuts** is `ModeA` (the short unit: 東京都 is 東京 and 都), `ModeB` (between) or `ModeC` (the long unit: 東京都庁 stays whole). A found the most in the tries: 東京 finds 東京都庁 only with A, and 京都 no longer finds 東京都. With `ModeC`, 東京都 is one word, and 東京 does not find it.
* **What a word is** (`wordsOfAnalysis`): the normal form of a word of the text, lower-cased. A word of ASCII letters and digits stays as written (lower-cased), since the normal form of `Hello` is ハロー. Particles, auxiliary verbs, punctuation and white space are left out, and so is a "word" with no letter or digit in it (the colons of a custom emoji, `:name:`, with a zero-width space in each, which `sudachi` calls nouns).
  A newline is a space, and a text of more than 3000 characters goes in pieces (a word on the cut is two words, at most once in 3000 characters).
* **The mark** (`sudachi/1:A:<size of the dictionary>:<a hash of its first 4 KB>`) is what hosigraph keeps in the schema mark: another dictionary, or another way of cutting, makes the khana again at the next open. The place of the file is not in it, nor the version of the `sudachi` program. A dictionary that is changed after its first 4 KB (a user dictionary on top, `-r` settings) is not seen.

## What it finds

From the tests (`test/Spec.hs`), with the dictionary above:

| searched | finds | why |
|---|---|---|
| 京都 | 来月、京都に旅行する予定です。 and **not** 東京都の人口… | words, not pairs of characters |
| 東京 | 東京都の人口… | 東京都 is 東京 and 都 (mode A) |
| 食べる | …寿司を食べました。 | the normal form |
| 猫 | うちの猫が… and ネコ動画を… | one character is a word; ネコ is 猫 |
| 引っ越し | 来週引っ越しをします。 and 引越し業者の… | spellings |
| WORLD, 3 | Hello, World! Rust 3 jumps | ASCII as written, any case |
| 誕生日 | 今日は誕生日パーティー… and 誕生を祝う日が来た。 but **not** a note that has only 誕生, or only 日 | 誕生日 is 誕生 and 日 (mode A): a search of several words wants all of them |

## On real posts

On 2026-11-22, 4,602 public notes of one instance (the data stays on the machine of whoever made it; none of it is in this repository). The notes that "have the word" are the ones where its words come in a row, allowing another form or spelling: **a yardstick that Sudachi itself sets, so it favours Sudachi**; no one labelled anything by hand.
The queries are the dictionary forms of content words (385 of them, which is not what people type) and long compound nouns that mode A cuts into several words (73). Mode A only.

* **A word searched alone** found every such note: 385 of 385 queries gave exactly that set, through the whole of hosigraph. The pairs of the JVM, on the same yardstick, found 3% of the notes of a one-character word (猫, 絵, 日), 60% of those of a two-character word and 43% over all (another form of a verb: 描く does not find 描いた), at a precision of 0.91 (あり finds ありがとう).
* **A word that A cuts into several** (誕生日 is 誕生 and 日) is a search of several words, and that finds the notes that have **all** of them, within the best `:top` by BM25 ([the guide](../../docs/guide.md#words-of-your-own-a-tokenizer-experimental) says how). When it was OR (Datalevin's own way) only 6% of the notes it gave had the words in a row, and a page of 20 for a word with fewer than 20 such notes was mostly notes that have one part of it (67 of the 73 words).
  Now, for the 73 words, **no note that was returned lacks a word**, and with the best 10, 20 or *R* asked for, **no word came back with fewer notes than it could** (0 of 73 each time). What is returned also has the notes whose words are apart (誕生を祝う日), so against "in a row" the precision is 0.945: that is the whole set of notes that have all the words, 508 of them, with the 477 that have them in a row (all found, 73 of 73, with `:top` as big as the notes). The best *R* alone found 465 of the 477 (63 of 73 words entirely): BM25 puts some notes whose words are apart before some whose words are in a row.
* **What it costs**: the 4,602 notes were written in 0.7 to 1.9 s (the words made in the same round; the faster runs came with the dictionary already in the file cache), 458 searches took 0.13 to 0.2 s (the same for `:top` 10, 20, *R* and as big as the notes), the world is 24 MB, `verify` found nothing, `rebuildKhana` took 0.8 s. The `sudachi` process holds about 160 MB and the `dtlv repl` about 160 MB (206 MB after a rebuild).
* **Junk it taught**: the colons of custom emoji were 10% of the words (43% of the notes had some) until a word with no letter or digit was left out. URLs split into words (`https`, `com`) and `#tag`, `@user` are words. Common parts (御, 日, 方, 目, 語, 的) are words like any other. 楽しみ is 楽しい and 味 (the normal form of the suffix み is 味): a way mode A has.

To do it again on your own posts (Python 3 and the built `mikke-real-check`; the work directory holds real posts, so **make it outside the repository and delete it afterwards**):

```
python3 -I real-check/extract.py  datoms.edn  /somewhere/work            # the public notes -> notes.json   (a dump of the sukhi server)
HOSIGRAPH_SUDACHI=… HOSIGRAPH_SUDACHI_DICT=… python3 -I real-check/yardstick.py  /somewhere/work   # the yardstick and the queries
mikke-real-check  /somewhere/work  /path/to/dtlv  /path/to/sudachi  /path/to/system_core.dic  A  R   # the world, the queries, the timings; the last is the :top: R (the notes that have the word in a row), all, or a number
python3 -I real-check/score.py  /somewhere/work  /somewhere/work/out.tsv  R                         # R-precision, precision and the checks against the yardstick (the same :top)
```

`extract.py` reads the datoms of one particular server; for another source, write `notes.json` yourself (a list of `[id, "text"]`) and go on from the second line.

## Tests

```
HOSIGRAPH_SUDACHI=/path/to/sudachi HOSIGRAPH_SUDACHI_DICT=/path/to/system_core.dic \
HOSIGRAPH_DTLV=/path/to/dtlv  cabal test hosi-mikke
```

`HOSIGRAPH_DATALEVIN_JAR` (and `HOSIGRAPH_JAVA`) adds the JVM khana. `HOSIGRAPH_TEST_ONLY=text` runs the tests whose name has that text in it. What is not set is skipped, and the run says so; the one test that needs nothing (`wordsOfAnalysis`, the rules of what a word is) always runs.
Tried with `KhanaDtlvRepl` (`dtlv` 1.2.11), sudachi.rs 0.7.0 and SudachiDict 20261015 `core`. **With `KhanaDatalevinJvm` the tests are written and were not run** (no JDK here); whether its default analyzer takes the `<attribute>.words` attributes as the native one does is not known.

## What is left

* **A faster AND that gives every answer.** A search of several words is the best `:top` of the OR, checked for every word: it was enough on these 73 words, and it is not shown for every pair (a rare word and a very common one could keep the notes that have both below the best *N*). A `:top` as big as the notes gives them all and scores every note that has any one word (1.2 to 2.7 s on 50,000 notes with the native `dtlv`; here, 4,602 notes, the same 0.15 s). Not tried: asking again with a wider `:top`; one fact for each word, as an index lookup. It is in `TODO.md` of hosigraph.
  Another way: index the long unit too (A and C words in one index, the search cut as C), so that 誕生日 is one word.
* **More of the real-post check.** The yardstick is set by Sudachi and not by hand; the queries are not what people type; only mode A and one instance's public notes. Not tried: B and C, A and C together, a stop list for the commonest parts, URLs and hashtags kept out of the words.
* **The version of the `sudachi` program** is not in the mark (now: the rules of this module, the mode, the dictionary).
* **A cost not measured:** a write that has a long text (the tokenizer is asked once for each batch); a world of 100,000 posts made again.
* **An option of the C door** for a tokenizer: there is none, and this one is Haskell.

## Licences

Neither `sudachi` nor the dictionary is in this package or this repository; `sudachi` is started as a program and talks over standard input and output, and `system_core.dic` is a file it reads. What each is under (read on 2026-11-22):

| what | licence |
|---|---|
| sudachi.rs 0.7.0 | Apache-2.0 (`LICENSE`, and `license = "Apache-2.0"` in its `Cargo.toml`) |
| SudachiDict 20261015 `core` | Apache-2.0 (`LICENSE-2.0.txt`), and its `LEGAL` names parts that have their own terms: UniDic (the BSD-3-Clause text of the UniDic Consortium) and NEologd (with notes on the sources of its data: Hatena keywords, Japan Post's postcodes, a list of station names, a list of personal names). **What those notes ask of someone who ships the dictionary was not worked out**; this repository does not ship it. |

This package is under the [Blue Oak Model License 1.0.0](LICENSE), like hosigraph.
