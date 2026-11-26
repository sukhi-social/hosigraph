# What `Ecto.Adapters.Hosi` needs from hosigraph, and what was found when it was tried

hosigraph 0.2 has no way to hand Datalevin a rules input. This adapter needs one, so a small patch was
written and run: [`hosigraph-patches/hosigraph-rules-input.patch`](hosigraph-patches/hosigraph-rules-input.patch)
(about 70 lines; the design below is what that patch does). With it, the adapter's whole test suite
passes against a real hosigraph and a real Datalevin 1.2.10 (see "What was run").

## 1. The rules input

A fourth input variant, next to `[:scalar v]`, `[:coll vs]` and `[:rel rows]`:

```elixir
[:rules, "[[(knows ?a ?b) [?a :person/friend ?b]]
            [(knows ?a ?b) [?a :person/friend ?x] (knows ?x ?b)]]"]
```

* **Text, not data.** A rule head is an EDN *list* `(knows ?a ?b)`; `Hosi.Edn.encode/1` writes every
  Elixir list as a vector, so rules cannot travel as data. takka has `parseRulesText`, and no
  Clause-to-EDN renderer, so the patch keeps the text it was given: takka's `Input` gets a
  constructor `InRulesEdn Text`; `InRules [Rule]` is still refused. (A renderer would let the
  Haskell API take parsed rules too.)
* **Position.** Inputs are positional, as now; the rules input answers the `%` in `:in`:

  ```
  [:find ?b :in $ ?__p0 [?__p2 ...] % :where (reachable ?__p0 ?b)]
  inputs: [[:scalar, 1], [:coll, [1, 2]], [:rules, "[...]"]]
  ```
* **Checked before anything is sent:** the ABI codec runs `parseRulesText` on the text and refuses what
  does not parse (`rules: ...`). The query itself already passes `parseQuery`, which knows `%`.
* **To Datalevin:** `dtlv exec` *evaluates* the vector of inputs, so the text is sent **quoted**
  (`'[[(knows ?a ?b) ...]]`). Unquoted, the rule heads are read as calls and Datalevin answers
  `Unable to resolve symbol: knows`. Scalars and collections never showed this.
* `tyInput` gets `Variant "rules" [TStr]`. `hosigraph-abi-gen --check gen` still says
  "ok: 3 files are as they should be": the generated files do not hold `tyInput`.

### Found on the way (Datalevin 1.2.10)

* **Every clause of one rule must write its head variables the same way.** `(r ?a ?b)` in one clause
  and `(r ?a ?c)` in another is refused: `Can't sum relations with different attrs`. `Ecto.Hosi.Rules`
  renames head variables to `?__h0 ?__h1 ...` itself; anyone sending rules by hand has to take care.
* **Rules can only come in through `%`.** A rule call with no rules input is
  `Missing rules var '%' in :in`; the guide's "write the rules into the query" cannot work, and the
  patch does not try.
* **The rows of an answer are a set, in no particular order.**

## 2. What the adapter assumes of hosigraph, and how each was checked

Checked live means: run against hosigraph (built from source, with the patch above) and Datalevin 1.2.10.

| The adapter relies on | Result |
|---|---|
| `write` answers `{:ok, %{temps: [[n, id], ...]}}` (also `"first-seq"`, `"last-seq"`, `sealed`) | live: yes |
| a failed `when:` answers `{:error, {:app, :conflict, "[[:holds ...]]"}}` and writes nothing | live: yes |
| `get_entities` answers `[%{attr => [values]}]` | live: yes (`[69]` prints as `~c"E"`, it is a list) |
| `query_many` answers one list of rows per query; `?e` is the id `write` returned | live: yes |
| a refused unique value is `{:error, {:app, :"write-rejected", message}}` and the message names the attribute | live: `dtlv: Cannot add #datalevin/Datom [<id> :user/email "a@x"] because of unique constraint: <id>` |
| a `:ref` attribute takes `[:ref, id]` (and `[:ref, [:tmp, n]]`) in `create`/`delete`; `:holds` takes the plain id | live: yes; a plain id in `create` is accepted too |
| `[(= 0 1)]`, `or-join` with an input among its join variables, a coll input with `[(= ?e ?p)]`, `[(not= ?e ?p)]`, `not-join`, `_` | live: all accepted and right |
| `highest/4`, `history/2`, `erase/2` | live: used by the adapter's tests, pass |
| `unique: :value` refuses a second holder | live: yes |
| a recursive rule (with a cycle), a rule with `not`, a rule with a comparison | live: right answers, with the patch |

## 3. What was run

* hosigraph built from source with **GHC 9.4.7** (the hosigraph README says GHC 9.10.3), which needed
  [`hosigraph-patches/hosigraph-ghc94-compat.patch`](hosigraph-patches/hosigraph-ghc94-compat.patch):
  `unix-2.7`'s `openFd`, and `foldl'` imported from `Data.List`. Not needed with GHC 9.10.
* hosigraph's own suite: **147 passed, 0 failed** with the native `dtlv` 1.2.10 (including a new test for
  the rules input); with the resident JVM, the 20 scenarios that ran before a time limit all passed.
* hosigraph-ex's own tests: 22 passed, against that libhosi.
* this adapter: 53 tests against the in-memory stand-in; 52 against hosigraph + `dtlv` (the 53rd looks
  inside the stand-in).

Not run: macOS; GHC 9.10.3; this adapter's suite on the JVM mode; Ecto 3.12-3.14.
