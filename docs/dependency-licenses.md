# Licenses of what hosigraph depends on

Checked on 2026-11-12, against `main` of sukhi-social/hosigraph (commit `1cc3629`, with setu and hosigraph-abi), with GHC 9.10.3 and cabal's own dependency plan.
This is a record of what was read, not legal advice. (The first version of this table came from a GHC 9.6.7 build; it was made again from the 9.10.3 plan. Six packages that tasty reaches are not built here, so their licences are those read from the 9.6.7 run, for the same versions.) Where something was **not** checked, it says so.

There are three kinds of dependency, and they are different in kind:

1. **Haskell packages** that hosigraph is built with (this is what ends up linked into a program).
2. **Datalevin** (and the JDK), which hosigraph talks to *as a separate program* and does not contain.
3. **What is inside the Datalevin jar / `dtlv`**, which matters only if you hand those files to someone else.

## In short

* Every Haskell package is BSD-3-Clause, BSD-2-Clause, MIT or ISC. No GPL, LGPL or AGPL. The obligation is the usual one:
  keep the copyright notice and the licence text when you pass along binaries or source.
* Datalevin is **EPL-2.0**. hosigraph does not include its code: it starts `dtlv`, or a JVM running the jar, and sends it EDN
  on standard input. So hosigraph's own licence is not decided by Datalevin's. If you *ship* the jar or `dtlv` together with hosigraph, you ship it under its own terms
  (section 3).
* hosigraph itself is under the **Blue Oak Model License 1.0.0** (`LICENSE`; `license: BlueOak-1.0.0` in the `.cabal` file of each of the four packages).

## 1. Haskell packages

Source of the licence column: `ghc-pkg` metadata of the installed package, and for three packages not installed here
(`hsc2hs`, `tasty`, `terminal-size`) the `.cabal` file on Hackage. This is the licence *named by the package*; the copyright holders
and exact notice text live in each package's own `LICENSE` file, which is what you copy if you redistribute.

"GHC 付属" means it comes with the compiler (a boot library); "直接" means named in one of hosigraph's `.cabal` files.
`QuickCheck` is a dependency of the test suites only, and `stm` of `setu`.
In cabal's plan, `tasty` is reached only through `vector`, and `terminal-size`, `ansi-terminal`, `optparse-applicative`
and `prettyprinter*` only through `tasty`. This is probably `vector`'s own test suite, which a normal build of hosigraph does not compile;
I did not confirm that. `hsc2hs` is a build tool that nothing depends on. Even if all of these were linked, they are MIT or BSD.

| package | version | licence | from | |
|---|---|---|---|---|
| aeson | 2.2.5.1 | BSD-3-Clause | Hackage | 直接 |
| ansi-terminal | 1.1.5 | BSD-3-Clause | Hackage |  |
| ansi-terminal-types | 1.1.3 | BSD-3-Clause | Hackage |  |
| array | 0.5.8.0 | BSD-3-Clause | GHC 付属 | 直接 |
| assoc | 1.1.1 | BSD-3-Clause | Hackage |  |
| base | 4.20.2.0 | BSD-3-Clause | GHC 付属 | 直接 |
| base-orphans | 0.9.4 | MIT | Hackage |  |
| bifunctors | 5.6.3 | BSD-3-Clause | Hackage |  |
| binary | 0.8.9.3 | BSD-3-Clause | GHC 付属 |  |
| bytestring | 0.12.2.0 | BSD-3-Clause | GHC 付属 | 直接 |
| character-ps | 0.1 | BSD-3-Clause | Hackage |  |
| colour | 2.3.7 | MIT | Hackage |  |
| comonad | 5.0.10 | BSD-3-Clause | Hackage |  |
| containers | 0.7 | BSD-3-Clause | GHC 付属 | 直接 |
| contravariant | 1.5.6 | BSD-3-Clause | Hackage |  |
| data-fix | 0.3.4 | BSD-3-Clause | Hackage |  |
| deepseq | 1.5.0.0 | BSD-3-Clause | GHC 付属 |  |
| directory | 1.3.8.5 | BSD-3-Clause | GHC 付属 | 直接 |
| distributive | 0.6.3 | BSD-3-Clause | Hackage |  |
| dlist | 1.0 | BSD-3-Clause | Hackage |  |
| exceptions | 0.10.9 | BSD-3-Clause | GHC 付属 |  |
| filepath | 1.5.4.0 | BSD-3-Clause | GHC 付属 | 直接 |
| ghc-bignum | 1.3 | BSD-3-Clause | GHC 付属 |  |
| ghc-boot-th | 9.10.3 | BSD-3-Clause | GHC 付属 |  |
| ghc-internal | 9.1003.0 | BSD-3-Clause | GHC 付属 |  |
| ghc-prim | 0.12.0 | BSD-3-Clause | GHC 付属 |  |
| hashable | 1.5.1.0 | BSD-3-Clause | Hackage |  |
| hsc2hs | 0.68.10 | BSD-3-Clause | Hackage |  |
| indexed-traversable | 0.1.5 | BSD-2-Clause | Hackage |  |
| indexed-traversable-instances | 0.1.2.1 | BSD-2-Clause | Hackage |  |
| integer-conversion | 0.1.1 | BSD-3-Clause | Hackage |  |
| integer-logarithms | 1.0.5 | MIT | Hackage |  |
| mtl | 2.3.1 | BSD-3-Clause | GHC 付属 |  |
| network-uri | 2.6.4.2 | BSD-3-Clause | Hackage |  |
| OneTuple | 0.4.3 | BSD-3-Clause | Hackage |  |
| optparse-applicative | 0.19.0.0 | BSD-3-Clause | Hackage |  |
| os-string | 2.0.7 | BSD-3-Clause | GHC 付属 |  |
| parsec | 3.1.18.0 | BSD-2-Clause | GHC 付属 |  |
| pretty | 1.1.3.6 | BSD-3-Clause | GHC 付属 |  |
| prettyprinter | 1.7.2 | BSD-2-Clause | Hackage |  |
| prettyprinter-ansi-terminal | 1.1.4 | BSD-2-Clause | Hackage |  |
| primitive | 0.9.1.0 | BSD-3-Clause | Hackage |  |
| process | 1.6.26.1 | BSD-3-Clause | GHC 付属 | 直接 |
| QuickCheck | 2.18.0.0 | BSD-3-Clause | Hackage | 直接 |
| random | 1.3.1 | BSD-3-Clause | Hackage |  |
| rts | 1.0.2 | BSD-3-Clause | GHC 付属 |  |
| scientific | 0.3.10.0 | BSD-3-Clause | Hackage | 直接 |
| semialign | 1.4 | BSD-3-Clause | Hackage |  |
| semigroupoids | 6.0.2 | BSD-2-Clause | Hackage |  |
| splitmix | 0.1.3.2 | BSD-3-Clause | Hackage |  |
| StateVar | 1.2.2 | BSD-3-Clause | Hackage |  |
| stm | 2.5.3.1 | BSD-3-Clause | GHC 付属 | 直接 |
| strict | 0.5.1 | BSD-3-Clause | Hackage |  |
| tagged | 0.8.11 | BSD-3-Clause | Hackage |  |
| tasty | 1.5.5 | MIT | Hackage |  |
| template-haskell | 2.22.0.0 | BSD-3-Clause | GHC 付属 |  |
| terminal-size | 0.3.4 | BSD-3-Clause | Hackage |  |
| text | 2.1.3 | BSD-2-Clause | GHC 付属 | 直接 |
| text-iso8601 | 0.1.1.2 | BSD-3-Clause | Hackage |  |
| text-short | 0.1.6.1 | BSD-3-Clause | Hackage |  |
| th-abstraction | 0.7.2.0 | ISC | Hackage |  |
| th-compat | 0.1.7 | BSD-3-Clause | Hackage |  |
| these | 1.2.1 | BSD-3-Clause | Hackage |  |
| time | 1.12.2 | BSD-2-Clause | GHC 付属 | 直接 |
| time-compat | 1.9.9 | BSD-3-Clause | Hackage |  |
| transformers | 0.6.1.1 | BSD-3-Clause | GHC 付属 |  |
| transformers-compat | 0.8 | BSD-3-Clause | Hackage |  |
| unix | 2.8.7.0 | BSD-3-Clause | GHC 付属 | 直接 |
| unordered-containers | 0.2.21 | BSD-3-Clause | Hackage |  |
| uuid-types | 1.0.6.1 | BSD-3-Clause | Hackage |  |
| vector | 0.13.2.0 | BSD-3-Clause | Hackage | 直接 |
| vector-stream | 0.1.0.1 | BSD-3-Clause | Hackage |  |
| witherable | 0.5 | BSD-3-Clause | Hackage |  |

## 2. Programs hosigraph talks to (not contained in it)

| what | licence | how hosigraph uses it |
|---|---|---|
| Datalevin 1.2.10 | EPL-2.0 (upstream `LICENSE`) | `KhanaDtlv`: starts the `dtlv` executable. `KhanaDatalevinJvm`: starts `java -jar` on the standalone jar. Talks over standard input/output. |
| JDK 21 or newer | Depends on the build. CI uses Eclipse Temurin (GPL-2.0 with Classpath Exception). | Only `java` is started. |

Nothing of Datalevin is compiled into or linked with hosigraph, and no Datalevin source is copied into this repository
(a search for copyright and "derived from" notes found none). `takka/` is hosigraph's own reader and writer of the same EDN words.
Because of that, choosing a licence for hosigraph does not depend on EPL-2.0.

## 3. Inside the Datalevin jar and `dtlv` (matters only if you redistribute them)

Read from the Maven POMs of the **direct** dependencies listed in `deps.edn` at tag `1.2.10`, and from upstream repositories.

| component | version | licence |
|---|---|---|
| org.clojure/clojure | 1.12.6 | EPL-1.0 |
| org.clojure/tools.cli | 1.4.256 | EPL-1.0 (README; the POM has no licence entry) |
| babashka/babashka.pods | 0.2.0 | EPL-1.0 |
| org.babashka/sci | 0.13.53 | EPL-1.0 |
| nrepl/bencode | 1.2.0 | EPL-1.0 (POM says "Eclipse Public License", link to v1.0) |
| com.taoensso/nippy | 3.9.0 | EPL-1.0 |
| com.taoensso/timbre | 6.5.0 | EPL-1.0 |
| metosin/jsonista | 1.0.0 | EPL-2.0 |
| com.cognitect/transit-clj | 1.1.357 | Apache-2.0 |
| me.lemire.integercompression/JavaFastPFOR | 0.3.14 | Apache-2.0 |
| org.roaringbitmap/RoaringBitmap | 1.6.14 | Apache-2.0 |
| com.alipay.sofa/jraft-core | 1.4.1 | Apache-2.0 (POM has none; parent POM and README say Apache-2.0) |
| org.eclipse.collections/eclipse-collections | 13.0.0 | EPL-1.0 and EDL-1.0 (POM has none; the project's README and file headers say so) |
| com.github.luben/zstd-jni | 1.5.7-11 | BSD-2-Clause |
| io.github.nextjournal/markdown | 0.7.225 | ISC |
| com.github.clj-easy/graal-build-time | 1.0.6 | MIT |
| org.bouncycastle/bcprov-jdk15on, bcpkix-jdk15on | 1.70 | Bouncy Castle Licence (an MIT-style licence) |
| org.clojars.huahaiy/dtlvnative-* | 1.1.8 | EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0 (POM); repository `LICENSE` is EPL-2.0 |
| dlmdb (inside dtlvnative; a fork of LMDB) | not pinned | repository `LICENSE` is BSD-3-Clause; `libraries/liblmdb/LICENSE` is the OpenLDAP Public License 2.8 |

### Not checked

* **Transitive dependencies of the jar.** For example `jraft-core` brings several libraries of its own. Only the direct ones are listed above.
  The way to settle this is to open the jar itself (`META-INF`, and each library's licence file). That needs the 93 MB jar, which I did not download.
* **What `dtlvnative` 1.1.8 really contains.** Its README now mentions `usearch` and `llama.cpp` as features of newer versions; I did not check
  whether 1.1.8 carries them (if it does: USearch is Apache-2.0 and llama.cpp is MIT, from memory, not read).
* The `deps.edn` above is the one at tag `1.2.10`; the `dtlv` native binaries are built from the same tag, but their build inputs were not inspected.
* Haskell: the licence column is what each package declares. Copyright holders and notice texts were not copied out.

## 4. Why Blue Oak 1.0.0 sits well with all of this

* Blue Oak is permissive. It asks one thing: whoever receives any part of the software from you must also receive the licence text or a link to it.
  It also gives a patent licence and has a 30-day grace period for fixing a missed notice.
* It does not clash with any Haskell dependency (BSD, MIT, ISC are all permissive), so nothing here stops you.
* Datalevin is separate (section 2), so EPL-2.0 does not reach hosigraph. If you later **bundle** the jar or `dtlv` in a release, that bundle also carries
  Datalevin's licence and the notices of section 3. They stay under their own terms; Blue Oak applies only to hosigraph's own files.
* The SPDX identifier is `BlueOak-1.0.0` (it is in the SPDX licence list, and the text in `LICENSE` is the SPDX text). Every hand-written `.hs`, `.sh` and `.py` file
  begins with `SPDX-License-Identifier: BlueOak-1.0.0`; the files that setu writes (`hosigraph-abi/gen/`) and the golden header in setu's tests do not, because a
  generator should not write hosigraph's licence into what it makes for others. In a `.cabal` file the id is accepted from `cabal-version: 3.0` on (Cabal picks its SPDX list by that number, and `BlueOak-1.0.0` came in with list 3.6;
  with `cabal-version: 2.4` it failed to parse). The four packages say `cabal-version: 3.0`, `license: BlueOak-1.0.0` and `license-file: LICENSE`,
  and Cabal 3.14's `cabal check` has no complaint about the licence. CI uses Cabal 3.10.3, which also reads 3.0 (not run here).
* Blue Oak is newer and less common than BSD or MIT, so some licence scanners may not know it. I have not tested any.
