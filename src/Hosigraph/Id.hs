-- SPDX-License-Identifier: BlueOak-1.0.0
-- |
-- Module      : Hosigraph.Id
-- Description : Time-sortable ids. The id alone says which month an entity was created in.
--
-- > id = (milliseconds since 2026-01-01 UTC) << 21  |  sequence within that millisecond
--
-- 42 + 21 = 63 bits, so it fits a positive 'Int64' (and the khana's entity id).
-- Sorting by id is sorting by creation time, and 'idMonth' needs no lookup, which is
-- what lets a point lookup skip every chunk of the wrong month (the zone map).
module Hosigraph.Id
  ( Month, showMonth, parseMonth
  , Clock, systemClock
  , epochMs, idMs, idSeqNo, idMonth, describeId, makeId, idRangeBetween
  , IdGen, newIdGen, nextId
  ) where

import           Data.Bits
import           Data.IORef
import           Data.Time.Calendar (toGregorian)
import           Data.Time.Clock (UTCTime(..))
import           Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import           Data.Time.Format (defaultTimeLocale, formatTime)

import           Hosigraph.Event (HosiId)

-- | (year, month)
type Month = (Int, Int)

showMonth :: Month -> String
showMonth (y, m) = pad 4 y ++ "-" ++ pad 2 m
  where pad n x = let s = show x in replicate (n - length s) '0' ++ s

parseMonth :: String -> Maybe Month
parseMonth s = case (reads (take 4 s), drop 4 s) of
  ([(y, "")], '-' : rest) | [(m, "")] <- reads rest, m >= 1, m <= 12 -> Just (y, m)
  _ -> Nothing

-- | Milliseconds since the POSIX epoch.  Tests pass their own to travel through time.
type Clock = IO Integer

systemClock :: Clock
systemClock = (\t -> floor (t * 1000)) <$> getPOSIXTime

-- | 2026-01-01T00:00:00Z in POSIX milliseconds.
epochMs :: Integer
epochMs = 1767225600000

seqBits :: Int
seqBits = 21

makeId :: Integer -> Integer -> HosiId
makeId ms sq = fromIntegral ((ms `shiftL` seqBits) .|. sq)

-- | The ids that entities created in @[from, to)@ have (UTC): from the first id of @from@'s millisecond
-- up to the last one before @to@'s.  An id is a creation time, so a span of time is one range of ids.
-- 'Nothing' when no id falls in it (an empty span, or one that ends before the first id the generator gives).
idRangeBetween :: UTCTime -> UTCTime -> Maybe (HosiId, HosiId)
idRangeBetween from to
  | loMs >= limit || toMs <= loMs = Nothing
  | otherwise                     = Just (makeId loMs 0, hi)
  where
    limit = 2 ^ (42 :: Int)                              -- 42 bits of milliseconds; more would not fit an Int64
    msOf t = floor (utcTimeToPOSIXSeconds t * 1000) - epochMs
    loMs = max 1 (msOf from)                             -- ms >= 1: the generator never gives 0 (see 'nextId')
    toMs = msOf to
    hi | toMs >= limit = maxBound
       | otherwise     = makeId toMs 0 - 1

-- | Milliseconds since 'epochMs' embedded in the id.
idMs :: HosiId -> Integer
idMs i = toInteger i `shiftR` seqBits

idSeqNo :: HosiId -> Integer
idSeqNo i = toInteger i .&. (bit seqBits - 1)

idTime :: HosiId -> UTCTime
idTime i = posixSecondsToUTCTime (fromRational (fromInteger (epochMs + idMs i) / 1000))

idMonth :: HosiId -> Month
idMonth i = let (y, m, _) = toGregorian (utctDay (idTime i)) in (fromIntegral y, m)

-- | A readable form for logs and demos: @2026-11-11 04:41:07.123 #5@.
describeId :: HosiId -> String
describeId i =
  formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (idTime i)
    ++ "." ++ pad3 (fromIntegral ((epochMs + idMs i) `mod` 1000)) ++ " #" ++ show (idSeqNo i)
  where pad3 :: Int -> String
        pad3 n = let s = show n in replicate (3 - length s) '0' ++ s

-- | Remembers the last (ms, seq) handed out, so ids never repeat and never go backwards,
-- even if the clock does.
newtype IdGen = IdGen (IORef (Integer, Integer))

-- | Start after the largest id seen so far (0 for a new database).
newIdGen :: HosiId -> IO IdGen
newIdGen maxSeen
  | maxSeen <= 0 = IdGen <$> newIORef (0, 0)
  | otherwise    = IdGen <$> newIORef (idMs maxSeen, idSeqNo maxSeen)

nextId :: IdGen -> Clock -> IO HosiId
nextId (IdGen ref) clock = do
  now <- clock
  atomicModifyIORef' ref $ \(lastMs, lastSq) ->
    let ms = max 1 (max (now - epochMs) lastMs)          -- ms >= 1 keeps small ids (like the meta entity 1) free
        (ms', sq) | ms == lastMs && lastMs > 0 = if lastSq + 1 < bit seqBits then (ms, lastSq + 1) else (ms + 1, 0)
                  | otherwise                 = (ms, 0)
    in ((ms', sq), makeId ms' sq)
