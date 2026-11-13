-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Hosigraph.Manifest
-- Description : The list of chunks that make up "the world now", hash-signed, swapped atomically.
--
-- The file @manifest@ has two lines:
--
-- > {"format":"hosi-manifest/1","generation":3,"sealedSeq":120,...}      <- the body, JSON
-- > sha256:9f2c…                                                          <- hash of the body's exact bytes
--
-- A new manifest is written to @manifest.tmp@ and renamed over @manifest@, so a reader
-- (or a restart) sees either the old world or the new one, never a mix.  Chunks that
-- exist on disk but are not listed here are not part of the world.
--
-- /Erase records./  A chunk may list one more file, its erase record ('ceErase', a "Hosigraph.Erase"
-- file in @erase/@ next to @chunks/@): the ids of that chunk that are gone.  A world that has an
-- erase record is written as @hosi-manifest/2@, with an @eraseGen@ that only goes up; a world that
-- never had one stays @hosi-manifest/1@, byte for byte what it was.  A reader that does not know
-- @/2@ refuses the file instead of leaving the field out and showing what was erased.
module Hosigraph.Manifest
  ( Manifest(..), ChunkEntry(..), FileEntry(..), emptyManifest
  , readManifest, writeManifest, manifestFile, entryFromInfo
  , chunkEntryHash
  ) where

import           Control.Exception (throwIO)
import           Control.Monad (when)
import           Data.Aeson
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int64)
import           Data.Maybe (isJust)
import qualified Data.Text as T
import           Data.Word
import           System.Directory (doesFileExist)
import           System.FilePath ((</>))

import           Hosigraph.Chunk (ChunkInfo(..))
import           Hosigraph.Event
import           Hosigraph.Id (Month, parseMonth, showMonth)
import           Hosigraph.Sha256 (sha256Hex)
import           Hosigraph.Sync (atomicReplace)

manifestFile :: FilePath
manifestFile = "manifest"

data FileEntry = FileEntry { feName :: String, feBytes :: Int, feSha256 :: String }
  deriving (Eq, Show)

-- | One chunk, with its zone map (month, id range, seq range) and its content hash.
data ChunkEntry = ChunkEntry
  { ceName   :: String
  , ceTier   :: String            -- ^ "hot" for now ("cold" arrives with freezing)
  , ceKind   :: Kind
  , ceMonth  :: Month
  , ceEvents :: Int
  , ceIds    :: Int
  , ceMinId  :: HosiId
  , ceMaxId  :: HosiId
  , ceMinSeq :: Word64
  , ceMaxSeq :: Word64
  , ceFiles  :: [FileEntry]
  , ceHash   :: String            -- ^ hash over the files' names and hashes: the chunk's identity
  , ceErase  :: Maybe FileEntry   -- ^ the chunk's erase record (a file in @erase/@, not in the chunk's directory), if any.
                                  --   Not part of 'ceHash': the chunk stays what its files are; the record belongs to the world.
  } deriving (Eq, Show)

data Manifest = Manifest
  { mfGeneration :: Int           -- ^ +1 every time the world changes
  , mfSealedSeq  :: Word64        -- ^ every event with seq <= this lives in a chunk
  , mfNextChunk  :: Int           -- ^ counter for chunk directory names
  , mfChunks     :: [ChunkEntry]
  , mfEraseGen   :: Int           -- ^ counter for erase record names; only goes up (0: this world never had a record)
  } deriving (Eq, Show)

emptyManifest :: Manifest
emptyManifest = Manifest 0 0 1 [] 0

chunkEntryHash :: [FileEntry] -> String
chunkEntryHash fs = sha256Hex (BC.pack (concat [ feName f ++ ":" ++ feSha256 f ++ ";" | f <- fs ]))

entryFromInfo :: String -> ChunkInfo -> [FileEntry] -> ChunkEntry
entryFromInfo name ci fs = ChunkEntry
  { ceName = name, ceTier = "hot", ceKind = ciKind ci, ceMonth = ciMonth ci
  , ceEvents = ciEvents ci, ceIds = ciIds ci, ceMinId = ciMinId ci, ceMaxId = ciMaxId ci
  , ceMinSeq = ciMinSeq ci, ceMaxSeq = ciMaxSeq ci, ceFiles = fs, ceHash = chunkEntryHash fs, ceErase = Nothing }

-- ---------------------------------------------------------------------------
-- JSON

instance ToJSON FileEntry where
  toJSON (FileEntry n b h) = object ["name" .= n, "bytes" .= b, "sha256" .= h]

instance FromJSON FileEntry where
  parseJSON = withObject "file" $ \o -> FileEntry <$> o .: "name" <*> o .: "bytes" <*> o .: "sha256"

instance ToJSON ChunkEntry where
  toJSON c = object $
    [ "name" .= ceName c, "tier" .= ceTier c, "kind" .= kindName (ceKind c)
    , "month" .= showMonth (ceMonth c), "events" .= ceEvents c, "ids" .= ceIds c
    , "minId" .= (fromIntegral (ceMinId c) :: Int64), "maxId" .= (fromIntegral (ceMaxId c) :: Int64)
    , "minSeq" .= ceMinSeq c, "maxSeq" .= ceMaxSeq c
    , "files" .= ceFiles c, "hash" .= ceHash c ]
    ++ [ "erase" .= fe | Just fe <- [ceErase c] ]            -- no key when there is no record: a chunk without one is written as it always was

instance FromJSON ChunkEntry where
  parseJSON = withObject "chunk" $ \o -> do
    kindS  <- o .: "kind"
    monthS <- o .: "month"
    kind   <- maybe (fail ("unknown kind " ++ kindS)) pure (kindFromName kindS)
    month  <- maybe (fail ("bad month " ++ monthS)) pure (parseMonth monthS)
    ChunkEntry <$> o .: "name" <*> o .: "tier" <*> pure kind <*> pure month
               <*> o .: "events" <*> o .: "ids"
               <*> (fromIntegral <$> (o .: "minId" :: A.Parser Int64))
               <*> (fromIntegral <$> (o .: "maxId" :: A.Parser Int64))
               <*> o .: "minSeq" <*> o .: "maxSeq" <*> o .: "files" <*> o .: "hash"
               <*> o .:? "erase"

-- | Does this world carry erase state?  Then it is written as @/2@ (and stays @/2@: the counter only goes up,
-- and what is read back must be what is in memory).
hasEraseState :: Manifest -> Bool
hasEraseState m = mfEraseGen m > 0 || any (isJust . ceErase) (mfChunks m)

instance ToJSON Manifest where
  toJSON m = object $
    [ "format" .= (if hasEraseState m then "hosi-manifest/2" else "hosi-manifest/1" :: T.Text), "generation" .= mfGeneration m
    , "sealedSeq" .= mfSealedSeq m, "nextChunk" .= mfNextChunk m, "chunks" .= mfChunks m ]
    ++ [ "eraseGen" .= mfEraseGen m | hasEraseState m ]

instance FromJSON Manifest where
  parseJSON = withObject "manifest" $ \o -> do
    fmt <- o .: "format"
    let withGen g = Manifest <$> o .: "generation" <*> o .: "sealedSeq" <*> o .: "nextChunk" <*> o .: "chunks" <*> pure g
    case fmt :: T.Text of
      "hosi-manifest/1" -> do
        m     <- withGen 0
        stray <- o .:? "eraseGen" :: A.Parser (Maybe A.Value)
        -- we never write this: an erase record in a /1 is a file that was not written by us (or was edited)
        when (isJust stray || any (isJust . ceErase) (mfChunks m)) $
          fail "hosi-manifest/1 cannot list erase records (they belong to hosi-manifest/2)"
        pure m
      "hosi-manifest/2" -> o .: "eraseGen" >>= withGen
      _                 -> fail ("unknown manifest format " ++ T.unpack fmt)

-- ---------------------------------------------------------------------------
-- Files

-- | A missing manifest means a brand-new database.  A manifest whose signature does not
-- match its body is an error ('Corrupt'), never silently ignored.
readManifest :: FilePath -> IO Manifest
readManifest dir = do
  let path = dir </> manifestFile
  exists <- doesFileExist path
  if not exists then pure emptyManifest else do
    bs <- B.readFile path
    let (body, rest) = BC.break (== '\n') bs
        sigLine      = BC.takeWhile (/= '\n') (B.drop 1 rest)
    case BC.stripPrefix "sha256:" sigLine of
      Nothing  -> throwIO (Corrupt (path ++ ": no sha256 line"))
      Just sig
        | BC.unpack sig /= sha256Hex body -> throwIO (Corrupt (path ++ ": signature does not match the body"))
        | otherwise -> case eitherDecodeStrict body of
            Left e  -> throwIO (Corrupt (path ++ ": " ++ e))
            Right m -> pure m

-- | Write the new world and swap it in (tmp file, fsync, rename, fsync dir).
writeManifest :: FilePath -> Manifest -> IO ()
writeManifest dir m = do
  let body = BL.toStrict (encode m)
      tmp  = dir </> (manifestFile ++ ".tmp")
  B.writeFile tmp (B.concat [body, "\n", "sha256:", BC.pack (sha256Hex body), "\n"])
  atomicReplace tmp (dir </> manifestFile)
