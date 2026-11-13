-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hosigraph.Abi.Api
-- Description : What hosigraph offers through its C door: the operations and the shape of each.
--
-- This is the one place the shapes are written.  The C header, the Haskell exports and the
-- check of every request and every answer are made from it ("Setu.Gen", "Setu.Dispatch").
--
-- == How values go over the wire
--
-- Bodies are UTF-8 EDN (T-12).  A request is @{:op :write :args {...}}@, an answer is
-- @{:ok ...}@ or @{:error :kind :message "..."}@.
--
-- [value]   @42@, @1.5@, @"text"@, @true@, @:keyword@ (hosigraph's own 'Value': an integer is
--           64 bits, a float is finite).
-- [ref]     an entity: @1234@ (one that exists), or @[:tmp 1]@ (one this batch makes; the answer
--           of @write@ says which id it got).
-- [val]     what an event holds: a value, or @[:ref ref]@ (the id of another entity).
-- [op]      @[:create ref :attr val]@ or @[:delete ref :attr val]@.
-- [entity]  @{:person/name ["Frege"] :person/age [70]}@: attribute to every value it has now.
-- [input]   what a query's @:in@ is given: @[:scalar v]@, @[:coll [v ...]]@, @[:rel [[v ...] ...]]@.
--           (Rules are not offered.)
-- [cell]    a value, or @[:pull tree]@ for a @(pull ?e [...])@: the tree is a value, a vector
--           (many), or a map from attribute to tree (an entity), in the order the query gave.
--
-- == Opening
--
-- @open@ is given UTF-8 EDN text (see 'openTy'):
--
-- > {:dir "/var/lib/hosi"
-- >  :schema {:person/name {:type :string} :person/age {:type :long}}
-- >  :config {:khana [:jvm "java" "/opt/datalevin-standalone.jar"] :sync true}}
--
-- Without @:dir@ (@{}@) nothing is opened: the connection answers @:ping@ and @:abi@ and refuses
-- every other operation (@HOSI_ERR_APP@, kind @:bad-argument@).  A host can use it to check that
-- the library loads, and which contract it speaks, before it has a database to give.
module Hosigraph.Abi.Api
  ( api
  , openTy
  , tyValue, tyRef, tyVal, tyOp, tyCond, tyInput, tyCell, tyEntity, tyEvent
  , tyWriteReport, tyChunk, tySealReport, tyTrace, tyStats
  , tyErasure, tyEraseReport, tyCleanReport, tyFreezeReport
  ) where

import           Setu.Spec

api :: Api
api = Api
  { apiName      = "hosi"
  , apiVersion   = 1
  , apiAppErr    = "APP"
  , apiAppErrDoc = "hosigraph refused the request: a write it would not keep, a query it could not run, a file that is corrupt"
  , apiOps       =
      [ Op "write"
          "Write one batch, all of it or none. Gives the ids that the [:tmp n] names got. With :when, only if every condition still holds in now, checked and written under one lock, so that no other writer comes between: read, then write with :when what you read. If one does not hold nothing is written and the answer is the error kind :conflict, whose message is the conditions that failed, as EDN (read again, and try again)."
          (TMap [Field "ops" (TVec tyOp), Field "when" (TOpt (TVec tyCond))])
          tyWriteReport
      , Op "write-many"
          "Write several batches at once, each as if written alone; the answers come in the order given. A batch that is refused is an [:error kind message] in its place."
          (TMap [Field "batches" (TVec (TVec tyOp))])
          (TVec (TTag [Variant "ok" [tyWriteReport], Variant "error" [TKw, TStr]]))
      , Op "seal"
          "Freeze the tail into chunks. Nil if the tail was empty."
          TNil
          tySealReport
      , Op "get-entities"
          "What the khana (now) holds about each entity, all from the same instant, in the order asked."
          (TMap [Field "ids" (TVec TInt)])
          (TVec tyEntity)
      , Op "query-many"
          "Datalog queries against now, all from the same instant, in the order asked. Each answer is rows of values."
          (TMap [Field "queries" (TVec tyQuery)])
          (TVec (TVec (TVec tyValue)))
      , Op "query-cells-many"
          "Like query-many, with (pull ?e [...]) allowed in :find. Each answer is rows of cells."
          (TMap [Field "queries" (TVec tyQuery)])
          (TVec (TVec (TVec tyCell)))
      , Op "history"
          "Every event about one entity, oldest first, from the chunks and the tail."
          (TMap [Field "id" TInt])
          (TVec tyEvent)
      , Op "history-trace"
          "Like history, with what the lookup did: which chunks were skipped, and what each one asked answered."
          (TMap [Field "id" TInt])
          (TMap [Field "events" (TVec tyEvent), Field "trace" tyTrace])
      , Op "verify"
          "Check everything checkable. An empty vector means all is well; otherwise each string is a problem."
          TNil
          (TVec TStr)
      , Op "stats"
          "Counts: generations, chunks, the tail, what the khana has folded."
          TNil
          tyStats
      , Op "rebuild-khana"
          "Throw the khana away and fold it again from the chunks and the tail."
          TNil
          TNil
      , Op "erase"
          "Make entities gone, from the events and from now, in one step however many erasures are given. It hides them at once; their bytes leave the disk when clean has run. Gives the ids that owned-by matched, whether or not they were erased before."
          (TMap [Field "erasures" (TVec tyErasure)])
          tyEraseReport
      , Op "clean"
          "Let what erase took out go from the disk: write the chunks again without the erased events, then make the khana a new file. At most max-chunks chunks per call (all, if left out); records-left above 0 means call again. Call it when the host is quiet; writes go on meanwhile."
          (TOpt (TMap [Field "max-chunks" (TOpt TInt)]))
          tyCleanReport
      , Op "freeze"
          "Put a month that nobody has read or written for cold-after-ms (see open) into one cold chunk: a smaller footprint, and the same answers from history. Does nothing when open was not given cold-after-ms. At most max-units (kind, month) units per call (all, if left out)."
          (TOpt (TMap [Field "max-units" (TOpt TInt)]))
          tyFreezeReport
      ]
  }

-- ---------------------------------------------------------------------------
-- The words

-- | A hosigraph value.
tyValue :: Ty
tyValue = TUnion [TInt, TDouble, TStr, TBool, TKw]

-- | An existing entity, or one the batch makes.
tyRef :: Ty
tyRef = TUnion [TInt, TTag [Variant "tmp" [TInt]]]

-- | What an event holds: a value, or the id of another entity.
tyVal :: Ty
tyVal = TUnion [TInt, TDouble, TStr, TBool, TKw, TTag [Variant "ref" [tyRef]]]

tyOp :: Ty
tyOp = TTag
  [ Variant "create" [tyRef, TKw, tyVal]
  , Variant "delete" [tyRef, TKw, tyVal]
  ]

-- | A fact about now that a write asks to still be true: this entity has this value for this attribute,
-- does not have it, or has no value at all for the attribute.  No @[:tmp n]@: a new entity has no state.
tyCond :: Ty
tyCond = TTag
  [ Variant "holds" [TInt, TKw, tyValue]
  , Variant "lacks" [TInt, TKw, tyValue]
  , Variant "unset" [TInt, TKw]
  ]

tyInput :: Ty
tyInput = TTag
  [ Variant "scalar" [tyValue]
  , Variant "coll"   [TVec tyValue]
  , Variant "rel"    [TVec (TVec tyValue)]
  ]

-- | A cell of a row: a value, or a pulled tree (the tree is not described further).
tyCell :: Ty
tyCell = TUnion [TInt, TDouble, TStr, TBool, TKw, TTag [Variant "pull" [TAny]]]

tyQuery :: Ty
tyQuery = TMap [Field "q" TStr, Field "inputs" (TOpt (TVec tyInput))]

tyEntity :: Ty
tyEntity = TDict (TVec tyValue)

tyEvent :: Ty
tyEvent = TMap
  [ Field "seq" TInt, Field "kind" (TEnum ["create", "delete"]), Field "id" TInt
  , Field "attr" TKw, Field "val" tyValue ]

tyWriteReport :: Ty
tyWriteReport = TMap
  [ Field "temps" (TVec (TVec TInt))   -- pairs [n id]
  , Field "first-seq" TInt, Field "last-seq" TInt, Field "sealed" TBool ]

tyChunk :: Ty
tyChunk = TMap
  [ Field "name" TStr, Field "tier" TStr, Field "kind" (TEnum ["create", "delete"])
  , Field "month" TStr, Field "events" TInt, Field "ids" TInt
  , Field "min-id" TInt, Field "max-id" TInt, Field "min-seq" TInt, Field "max-seq" TInt
  , Field "hash" TStr ]

tySealReport :: Ty
tySealReport = TOpt (TMap [Field "events" TInt, Field "generation" TInt, Field "chunks" (TVec tyChunk)])

tyTrace :: Ty
tyTrace = TMap
  [ Field "month" TStr, Field "chunks" TInt
  , Field "skipped-by-month" TInt, Field "skipped-by-range" TInt, Field "skipped-by-erase" TInt, Field "skipped-by-filter" TInt
  , Field "probed" (TVec (TMap
      [ Field "chunk" TStr
      , Field "says" (TEnum ["not-star", "look-alike", "star", "empty-sky"])
      , Field "found" (TOpt TInt) ]))
  , Field "tail-events" TInt ]

-- | What to erase.  Times are milliseconds since 1970-01-01T00:00Z, and a span is @[from, to)@ on the
-- time an entity was /created/ (an id is a creation time).
tyErasure :: Ty
tyErasure = TTag
  [ Variant "ids"              [TVec TInt]                        -- these entities
  , Variant "range"            [TInt, TInt]                       -- every id from the first to the last, inclusive
  , Variant "between"          [TInt, TInt]                       -- every entity created in the span
  , Variant "owned-by"         [TVec TKw, TVec TInt]              -- attributes, owners: whatever has one of them with an owner as its value
  , Variant "owned-by-between" [TVec TKw, TVec TInt, TInt, TInt]  -- ...and was created in the span
  ]

tyEraseReport :: Ty
tyEraseReport = TMap
  [ Field "generation" TInt, Field "dropped" (TVec TStr), Field "recorded" (TVec TStr)
  , Field "retracted" TInt, Field "owned" (TVec TInt) ]

tyCleanReport :: Ty
tyCleanReport = TMap
  [ Field "rewritten" (TVec (TMap [Field "chunk" TStr, Field "into" (TOpt TStr)]))   -- into nil: nothing of it was left
  , Field "skipped" (TVec TStr), Field "records-left" TInt
  , Field "khana-cleaned" TBool, Field "khana-rebuilt" TBool ]

tyFreezeReport :: Ty
tyFreezeReport = TMap
  [ Field "frozen" (TVec (TMap [Field "kind" (TEnum ["create", "delete"]), Field "month" TStr, Field "into" (TOpt TStr)]))
  , Field "skipped" (TVec (TMap [Field "kind" (TEnum ["create", "delete"]), Field "month" TStr])) ]

tyStats :: Ty
tyStats = TMap
  [ Field "generation" TInt, Field "chunks" TInt, Field "sealed-seq" TInt, Field "next-seq" TInt
  , Field "tail-events" TInt, Field "tail-bytes" TInt, Field "folded-seq" TInt, Field "khana-datoms" TInt ]

-- | What @open@ is given (as EDN text).  Everything may be left out; without @:dir@ no database
-- is opened.
openTy :: Ty
openTy = TMap
  [ Field "dir" (TOpt TStr)
  , Field "schema" (TOpt (TDict attrSchema))
  , Field "config" (TOpt config)
  ]
  where
    attrSchema = TMap
      [ Field "type"   (TOpt (TEnum ["any", "string", "long", "double", "boolean", "keyword", "ref"]))
      , Field "card"   (TOpt (TEnum ["one", "many"]))
      , Field "unique" (TOpt (TEnum ["identity", "value"])) ]
    config = TMap
      [ Field "seal-events"       (TOpt TInt)
      , Field "seal-age-ms"       (TOpt TInt)
      , Field "block-events"      (TOpt TInt)
      , Field "cold-block-events" (TOpt TInt)
      , Field "cold-after-ms"     (TOpt TInt)
      , Field "sync"              (TOpt TBool)
      , Field "reader-cache-bytes" (TOpt TInt)
      , Field "khana" (TOpt (TTag [Variant "dtlv" [TStr], Variant "jvm" [TStr, TStr]])) ]
