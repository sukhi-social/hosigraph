# Changelog

## 0.0.1.0 (not released)

* `Setu.Spec`, `Setu.Validate`, `Setu.Wire`: the description of an API, and the check of values against it.
* `Setu.Ticket`, `Setu.Dispatch`, `Setu.Abi`: the table of numbered tickets, the table of operations, and connections with handles.
* `Setu.Gen`, `Setu.Gen.CHeader`, `Setu.Gen.CAbi`, `Setu.Gen.HsExport`: the C header, the C layer and the Haskell exports, as text.
* `Setu.Codec`: `ToEdn` and `FromEdn`, with a Generic way to write the instances of records and sums.
* Bodies are written with `Takka.Edn.renderEdn` (takka 0.1.0.1 or later: before it, a non-ASCII character in a string was written as `\26085`, which no EDN reader understands).
