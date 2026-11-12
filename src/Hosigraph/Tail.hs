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
-- cut the file there.
module Hosigraph.Tail
  ( TailLog, openTail, tailPath, tailSize, appendBatch, appendBatches, truncateTail, resetTail, closeTail
  , readTail, tailFile
  ) where

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
readTail :: FilePath -> IO ([[Event]], Integer)
readTail dir = do
  let path = dir </> tailFile
  exists <- doesFileExist path
  if not exists then pure ([], 0) else do
    bs <- B.readFile path
    pure (go bs 0 [])
  where
    go bs off acc
      | B.length bs - off < 8 = (reverse acc, fromIntegral off)
      | otherwise =
          let hdr = runGet ((,) <$> getU32 <*> getU32) bs off
          in case hdr of
               Left _ -> (reverse acc, fromIntegral off)
               Right (len, crc) ->
                 let len' = fromIntegral len
                     payload = B.take len' (B.drop (off + 8) bs)
                 in if B.length payload /= len' || crc32 payload /= crc
                      then (reverse acc, fromIntegral off)
                      else case runGet parseBatch payload 0 of
                             Left _    -> (reverse acc, fromIntegral off)
                             Right evs -> go bs (off + 8 + len') (evs : acc)
    parseBatch = do
      _first <- getU64
      n <- getU32
      replicateM (fromIntegral n) getEvent
