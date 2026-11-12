# setu

*setu* is Pali for "bridge". A Haskell library says **once** what it offers: the operations, the shape of what goes in and
out of each, and the kinds of error. setu writes the doors other languages use to call it, from that one description.

What it writes (as text; there is no C in this package, and no Template Haskell):

* a **C header**: a thin interface that does not change when an operation is added, because the operation is named in the body of a request;
* the Haskell **`foreign export`s** that the header's functions call;
* a small **C layer** that starts the Haskell runtime, guards every call (before init, after shutdown), and passes the options that
  keep the host's signal handlers (`--install-signal-handlers=no`).

The door is the "ticket" interface: a request is handed in with a **number** the caller chooses and a body, and is answered
**exactly once**, through a callback, with a status and a body. Bodies are UTF-8 EDN: `{:op :add :args {:a 1 :b 2}}` goes in,
`{:ok 3}` or `{:error :kind :message "..."}` comes back. (A comma is whitespace in EDN, and the writer puts one between the pairs of a map,
as Clojure's printer does: `{:a 1, :b 2}`. A reader of the answers has to skip commas.) The rules of that interface (T-01 ... T-14: answered once, a cancel racing a finish,
callbacks never on the thread that called, `close` returning after the last callback) are kept by `Setu.Ticket`
and tried by its tests, so a library that uses setu does not have to get them right again.

## What you write

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Setu.Gen (runMain)
import Setu.Gen.CAbi (cAbi, defaultCAbiConfig)
import Setu.Gen.CHeader (cHeader)
import Setu.Gen.HsExport (HsExportConfig(..), hsExport)
import Setu.Spec

api :: Api
api = Api
  { apiName = "demo", apiVersion = 1, apiAppErr = "APP", apiAppErrDoc = "the library said no"
  , apiOps = [ Op "add" "Adds two integers." (TMap [Field "a" TInt, Field "b" TInt]) TInt ] }

-- cabal run demo-gen -- --out gen     write the files
-- cabal run demo-gen -- --check gen   fail if they are out of date (run it in CI)
main :: IO ()
main = runMain
  [ ("include/demo.h",       cHeader api)
  , ("cbits/demo_abi.c",     cAbi defaultCAbiConfig api)
  , ("Demo/Abi/Generated.hs", hsExport (HsExportConfig "Demo.Abi.Generated" "Demo.Abi.Library" "library") api) ]
```

The generated files are committed. Because the generators are pure functions from the description to text, the same description gives the
same files, and `--check` tells you when a change to the description was not followed by a new generation.

You also write a handler for each operation (`Setu.Dispatch`: EDN in, EDN or a failure out). The arguments are checked against the
description before a handler runs, and the answer against the result's shape before it is sent, so a handler that returns the wrong thing is
reported as the library's own mistake, not the caller's. `Setu.Codec` helps with the EDN: `ToEdn` and `FromEdn`, and a Generic way to
write the instance of a record or a sum in one line.

A reference that is built on it: [hosigraph-abi](https://github.com/sukhi-social/hosigraph/tree/main/hosigraph-abi) (hosigraph behind a C ABI).

## The modules

| Module | What it holds |
|---|---|
| `Setu.Spec` | the description: `Api`, `Op`, `Ty` (the shapes: nil, bool, integer, double, string, keyword, any, vec, map, dict, opt, union, enum, tag), and `checkApi` |
| `Setu.Validate` | a value against a `Ty`, with the path to the mistake |
| `Setu.Wire` | the envelope of a request and of an answer |
| `Setu.Codec` | `ToEdn`, `FromEdn`, and the Generic way to write their instances |
| `Setu.Ticket` | the table of numbered tickets: answered once, cancel, close (T-01 ... T-14) |
| `Setu.Dispatch` | the table of operations: bytes in, bytes out, and who is blamed |
| `Setu.Abi` | connections and handles over the above (`open` returns at once; the library starts on a thread of its own) |
| `Setu.Gen`, `Setu.Gen.*` | the generators, and `runMain` for a program that writes or checks them |

## Status

An early version (0.0.1). What has been seen working: the tests below, and one customer (hosigraph) through a shared library on macOS (arm64, GHC 9.6.7),
driven from Python's `ctypes`. What has not: Linux, a host other than Python, shipping the shared library to another machine (GHC's libraries are
named by absolute path), the GHC runtime started a second time after shutdown (it refuses, and the guard layer says so). T-13 and T-14 are additions
to the original twelve rules and have not been agreed by anyone but the author.

## Tests

```bash
cabal test    # about 140 checks; no database. The one check that needs a C compiler is skipped without one.
```

`takka` (EDN) is the one dependency that is not on Hackage yet: see `cabal.project.standalone`.
