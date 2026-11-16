-- SPDX-License-Identifier: BlueOak-1.0.0
-- |
-- Module      : Hosigraph.Backfill
-- Description : Fold events newest first (pure): what an older event may still add, and what it may not.
--
-- The khana is usually built from the oldest event to the newest, and every event is allowed to change what the
-- ones before it made.  Here the events come the other way: the newest first.  Then the rule is turned round:
-- /an older event never overrides a newer one/.  A fact is put in when the newest word about it is a Create, and
-- nothing older may change it.
--
-- This is why a khana built this way can be asked questions before it is done.  An update of an entity whose
-- Create has not been read yet is held as it is, and the entity shows with the facts known so far.  When the
-- Create comes, it only adds what no newer event has already said.
--
-- What the khana holds at any step is a part of what the finished khana will hold, never something else
-- (a fact that is put in is never taken out later).  That is what the tests check: the result of reading
-- newest first, in any steps, is the same as folding oldest first.
--
-- The rules, for one entity and one attribute, reading newest first:
--
-- * __cardinality one__: the first Create read settles the attribute, unless a newer Delete took out exactly that
--   value (then the attribute is settled as /absent/).  Everything older about it is ignored.  A Delete of a
--   value that is not there does nothing, as it does when folding forward.
-- * __cardinality many__: every value is settled by its own newest event, a Create (put in) or a Delete (left out).
--
-- Only this much has to be remembered between steps ('Back'): which attributes are settled, which values were
-- deleted, which values were seen.  It grows with the events read, not with what is left to read.
module Hosigraph.Backfill
  ( Fact, Back, emptyBack, backStep
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import           Hosigraph.Event
import           Takka.Types

-- | One fact of the khana: an entity, an attribute, a value.
type Fact = (HosiId, Keyword, Value)

-- | What is known of the events read so far (all newer than the ones still to come).
data Back = Back
  { bkDecided :: !(S.Set (HosiId, Keyword))   -- ^ cardinality one: the newest word about this attribute has been read
  , bkGone    :: !(S.Set Fact)                -- ^ cardinality one: values a (newer) Delete took out
  , bkSeen    :: !(S.Set Fact)                -- ^ cardinality many: values whose newest event has been read
  }

emptyBack :: Back
emptyBack = Back S.empty S.empty S.empty

-- | Take the next events of a newest-first reading.  They must be in descending seq order, and all older than
-- the events of the steps before.  Returns the facts to put in the khana (each is the final word about it, so
-- none is ever taken out), and what to remember.
backStep :: Schema -> Back -> [Event] -> ([Fact], Back)
backStep schema b0 evs = (reverse out, b1)
  where
    (out, b1) = foldl' step ([], b0) evs

    manyOf a = maybe False ((== Many) . asCard) (M.lookup a schema)

    step (acc, b) e =
      let f@(i, a, _) = (evId e, evAttr e, evVal e)
      in case evKind e of
        KDelete
          | manyOf a  -> (acc, b { bkSeen = S.insert f (bkSeen b) })
          | otherwise -> (acc, if S.member (i, a) (bkDecided b) then b else b { bkGone = S.insert f (bkGone b) })
        KCreate
          | manyOf a ->
              if S.member f (bkSeen b)
                then (acc, b)
                else (f : acc, b { bkSeen = S.insert f (bkSeen b) })
          | S.member (i, a) (bkDecided b) -> (acc, b)
          | S.member f (bkGone b)         -> (acc, b { bkDecided = S.insert (i, a) (bkDecided b) })    -- settled: taken out
          | otherwise                     -> (f : acc, b { bkDecided = S.insert (i, a) (bkDecided b) })
