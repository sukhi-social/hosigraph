-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hosigraph.Tail
-- Description : The append-only log: where a write lands before it is sealed into a chunk.
--
-- > record = length:4  crc32:4  payload          payload = firstSeq:8 count:4 { event }*
--
-- One record is one batch (one 'Hosigraph.write' call): all of it or none of it.  A crash in
-- the middle of an append leaves a short or corrupt record at the end; 'readTail' notices
-- (length or CRC does not fit) and reports where the good part ends, so the caller can
-- cut the file there.  If a whole record follows the bad one it was not a crash but damage in the
-- middle of the log, and 'readTail' refuses ('Corrupt') instead of reporting a place to cut.
--
-- /The format has no header, and its first four bytes are reserved./  A file that begins with the
-- four bytes @HOSI@ is not a log of this format: read as a length it would be 1.1 GiB (a record is a
-- batch, and none is that big), so no record of this format starts that way.  A later format would
-- begin with a mark that starts with these four bytes, and this code refuses such a file ('Corrupt',
-- and it is not touched).  Without the refusal, an unreadable first record looks like a torn end, and
-- the caller cuts the file there: the unsealed writes of a newer world would be thrown away.
module Hosigraph.Tail
  ( TailLog, openTail, tailPath, tailSize, appendBatch, appendBatches, truncateTail, resetTail, closeTail
  , readTail, tailFile
  ) where

import           Control.Exception (throwIO)
import           Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.IORef
import           System.Directory (doesFileExist)
import           System.FilePath ((</>))
import           System.IO

import           Hosigraph.Bin
import           Hosigraph.Event
import           Hosigraph.Sync

tailFile :: FilePath
tailFile = "tail.log"

-- | The first four bytes that a newer format may begin with (see the module header).
reservedMark :: B.ByteString
reservedMark = "HOSI"

data TailLog = TailLog
  { tlPath   :: FilePath
  , tlHandle :: IORef Handle
  , tlSize   :: IORef Integer
  , tlSync   :: Bool
  }

tailPath :: TailLog -> FilePath
tailPath = tlPath

tailSize :: TailLog -> IO Integer
tailSize = readIORef . tlSize

-- | Open (creating if needed) for appending.  @sync@ = fsync after every batch.
openTail :: FilePath -> Bool -> IO TailLog
openTail dir sync = do
  let path = dir </> tailFile
  h <- openBinaryFile path ReadWriteMode
  size <- hFileSize h
  hSeek h SeekFromEnd 0
  TailLog path <$> newIORef h <*> newIORef size <*> pure sync

closeTail :: TailLog -> IO ()
closeTail tl = readIORef (tlHandle tl) >>= hClose

-- | Append one batch.  Returns the file size /before/ the append, which 'truncateTail'
-- can go back to if the batch must be taken back.
appendBatch :: TailLog -> [Event] -> IO Integer
appendBatch tl evs = head <$> appendBatches tl [evs]

-- | Append several batches with one write and one fsync.  Each batch is still its own record (so a
-- torn end loses at most the records that were not whole, never half of a batch).  Returns the file
-- size /before/ each batch, in order: cutting back to the first one undoes all of them.
appendBatches :: TailLog -> [[Event]] -> IO [Integer]
appendBatches _ [] = pure []
appendBatches tl batches = do
  when (any null batches) $ ioError (userError "appendBatch: empty batch")
  let encode evs =
        let payload = build (putU64 (evSeq (head evs)) <> putU32 (fromIntegral (length evs)) <> mconcat (map putEvent evs))
        in build (putU32 (fromIntegral (B.length payload)) <> putU32 (crc32 payload) <> put payload)
      records = map encode batches
  before <- readIORef (tlSize tl)
  h <- readIORef (tlHandle tl)
  B.hPut h (B.concat records)
  hFlush h
  when (tlSync tl) (syncFile (tlPath tl))
  let sizes = scanl (+) before (map (fromIntegral . B.length) records)
  writeIORef (tlSize tl) (last sizes)
  pure (init sizes)

-- | Cut the file back to a size (undoing an append, or dropping a torn tail).
truncateTail :: TailLog -> Integer -> IO ()
truncateTail tl size = do
  h <- readIORef (tlHandle tl)
  hFlush h
  hSetFileSize h size
  hSeek h AbsoluteSeek size
  when (tlSync tl) (syncFile (tlPath tl))
  writeIORef (tlSize tl) size

-- | Start over with exactly these batches (used after sealing, and after recovery).
-- Written to a new file and swapped in, so a crash cannot leave half a log.
resetTail :: TailLog -> [[Event]] -> IO ()
resetTail tl batches = do
  let tmp = tlPath tl ++ ".tmp"
      rec evs = let payload = build (putU64 (evSeq (head evs)) <> putU32 (fromIntegral (length evs)) <> mconcat (map putEvent evs))
                in build (putU32 (fromIntegral (B.length payload)) <> putU32 (crc32 payload) <> put payload)
      bytes = BL.fromChunks (map rec (filter (not . null) batches))
  readIORef (tlHandle tl) >>= hClose
  BL.writeFile tmp bytes
  atomicReplace tmp (tlPath tl)
  h <- openBinaryFile (tlPath tl) ReadWriteMode
  size <- hFileSize h
  hSeek h SeekFromEnd 0
  writeIORef (tlHandle tl) h
  writeIORef (tlSize tl) size

-- | Parse a tail file: the good batches, and the byte offset where the good part ends
-- (which is smaller than the file size if the end was torn).
--
-- The first record that cannot be read ends the good part, and what follows it is cut by the caller
-- /only if nothing whole follows/.  If a whole record is found after it (at any offset), the bad
-- record is in the middle of the log, not at its end: that is damage, not a crash, and the batches
-- after it were written and may have been told to the caller as kept.  Then this is 'Corrupt' and
-- the file is not touched.  (A damaged /last/ record cannot be told from a torn one, and is cut.)
readTail :: FilePath -> IO ([[Event]], Integer)
readTail dir = do
  let path = dir </> tailFile
  exists <- doesFileExist path
  if not exists then pure ([], 0) else do
    bs <- B.readFile path
    when (reservedMark `B.isPrefixOf` bs) $
      throwIO (Corrupt (path ++ ": starts with " ++ show reservedMark ++ ", which is not a record of this format "
                        ++ "(a log written by a newer hosigraph?); not read, and not cut"))
    let (batches, good) = go bs 0 []
    case [ p | p <- [good + 1 .. B.length bs - 8], wholeAt bs p ] of
      (p : _) -> throwIO (Corrupt (path ++ ": the record at byte " ++ show good ++ " is damaged, and a whole record follows it (at byte " ++ show p
                                   ++ "), so this is not a torn end.  Not read, and not cut: the file is as it was.  Everything before byte " ++ show good
                                   ++ " is fine.  To open the world anyway, keep a copy of the file and cut it at byte " ++ show good
                                   ++ " (what follows is lost)"))
      [] -> pure (batches, fromIntegral good)
  where
    go bs off acc = case recordAt bs off of
      Just (evs, end) -> go bs end (evs : acc)
      Nothing         -> (reverse acc, off)
    wholeAt bs p = maybe False (not . null . fst) (recordAt bs p)

-- | The whole record that starts at this offset (its events, and where it ends): length and CRC fit, and the payload parses.
recordAt :: B.ByteString -> Int -> Maybe ([Event], Int)
recordAt bs off
  | B.length bs - off < 8 = Nothing
  | otherwise = case runGet ((,) <$> getU32 <*> getU32) bs off of
      Left _ -> Nothing
      Right (len, crc) ->
        let len' = fromIntegral len
            payload = B.take len' (B.drop (off + 8) bs)
        in if B.length payload /= len' || crc32 payload /= crc
             then Nothing
             else either (const Nothing) (\evs -> Just (evs, off + 8 + len')) (runGet parseBatch payload 0)
  where
    parseBatch = do
      _first <- getU64
      n <- getU32
      replicateM (fromIntegral n) getEvent
