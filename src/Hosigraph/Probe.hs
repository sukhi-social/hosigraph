-- SPDX-License-Identifier: BlueOak-1.0.0
-- |
-- Module      : Hosigraph.Probe
-- Description : 「星かな？」 What a point lookup finds out, one chunk at a time.
--
-- A minimal perfect hash function ("Hosigraph.Mphf") is a very confident guide: ask it
-- about any key at all, even one that was never in the chunk, and it points at a seat
-- and says "this one is yours!".  So after the MPHF has named a slot, we ask the slot
-- 「星かな？」 ("a star, is it?") in two steps, cheapest first:
--
-- 1. the 32-bit fingerprint stored at the slot: a stranger is turned away here
--    (almost always), without opening the chunk's body at all;
-- 2. the key itself, read at the slot: the last, certain check.
--
-- In plain terms: 'NotStar' = fingerprint mismatch, 'LookAlike' = fingerprint matched by
-- chance but the key was another one, 'Star' = it really is there, 'EmptySky' = the
-- chunk has no keys at all.
module Hosigraph.Probe (Probe(..), confirmSlot, probeSays) where

import Data.Word

data Probe a
  = NotStar        -- ^ not a star: the fingerprint said no (the body was not read)
  | LookAlike      -- ^ looked like a star: fingerprint matched by luck; the key at the slot was someone else's
  | Star a         -- ^ a star: found; carries what was found
  | EmptySky       -- ^ empty sky: a chunk with no keys
  deriving (Eq, Show, Functor)

-- | The two-step question.  @confirm@ reads the slot's body and returns @Just evidence@
-- only if the stored key really is the asked one; it is not run at all when the
-- fingerprints differ.
confirmSlot
  :: Monad m
  => Word32          -- ^ fingerprint of the key we are asking about
  -> Word32          -- ^ fingerprint stored at the slot the MPHF pointed to
  -> m (Maybe a)     -- ^ read the slot and check the real key
  -> m (Probe a)
confirmSlot asked stored confirm
  | asked /= stored = pure NotStar
  | otherwise       = maybe LookAlike Star <$> confirm

-- | A sentence for logs and demos.
probeSays :: Probe a -> String
probeSays p = case p of
  NotStar   -> "not a star (the fingerprint is different; the body was not read)"
  LookAlike -> "looked like a star (the fingerprint matched by chance, but the key is different)"
  Star _    -> "a star"
  EmptySky  -> "an empty chunk (no star at all)"
