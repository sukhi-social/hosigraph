{-# LANGUAGE OverloadedStrings #-}
-- A walk through hosigraph.  The clock moves by hand, from November to December.
module Main (main) where

import           Control.Monad
import           Data.IORef
import           Data.List (intercalate)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           System.Directory
import           System.Environment (getArgs, lookupEnv)
import           System.Exit (exitFailure)
import           System.FilePath ((</>))
import           System.IO

import           Hosigraph
import           Hosigraph.Id (epochMs, idMs, makeId)
import           Hosigraph.Manifest
import           Hosigraph.Tail (tailFile)
import           Takka.Types (renderValue)

schema :: Schema
schema = M.fromList
  [ ("person/name",   identity (attr TString))
  , ("person/age",    attr TLong)
  , ("person/friend", many ref) ]

step :: String -> IO ()
step s = putStrLn ("\n● " ++ s)

say :: String -> IO ()
say s = putStrLn ("    " ++ s)

showEntity :: M.Map Keyword [Value] -> String
showEntity m = intercalate ", " [ T.unpack (kwText k) ++ "=" ++ intercalate "|" (map (T.unpack . renderValue) vs) | (k, vs) <- M.toList m ]

showEvent :: Event -> String
showEvent e = "seq " ++ show (evSeq e) ++ "  " ++ (case evKind e of KCreate -> "Create"; KDelete -> "Delete")
  ++ "  " ++ T.unpack (kwText (evAttr e)) ++ " = " ++ T.unpack (renderValue (evVal e))

showTrace :: LookupTrace -> String
showTrace t = "month=" ++ showMonth (ltMonth t) ++ "  of " ++ show (ltChunks t) ++ " chunks, skipped by month=" ++ show (ltSkippedByMonth t)
  ++ ", skipped by ID range=" ++ show (ltSkippedByRange t) ++ ", probed=" ++ show (length (ltProbed t))
  ++ concat [ "\n      " ++ n ++ " → is it a star? " ++ probeSays r ++ (case r of Star c -> " (" ++ show c ++ " events)"; _ -> "") | (n, r) <- ltProbed t ]
  ++ (if ltTailEvents t > 0 then "\n      from the tail log: " ++ show (ltTailEvents t) ++ " events" else "")

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetBuffering stdout LineBuffering
  args <- getArgs
  let dir = case args of (d : _) -> d; _ -> "/tmp/hosigraph-demo"
  removePathForcibly dir
  clk <- newIORef (epochMs + 314 * 86400000)          -- 2026-11-11 00:00 UTC
  -- The khana is Datalevin in a resident JVM: HOSIGRAPH_DATALEVIN_JAR is the standalone jar,
  -- HOSIGRAPH_JAVA the java command ("java" if unset).
  mjar <- (>>= \j -> if null j then Nothing else Just j) <$> lookupEnv "HOSIGRAPH_DATALEVIN_JAR"
  java <- maybe "java" id <$> lookupEnv "HOSIGRAPH_JAVA"
  jar <- case mjar of
    Just j  -> pure j
    Nothing -> hPutStrLn stderr "set HOSIGRAPH_DATALEVIN_JAR to the Datalevin standalone jar (and HOSIGRAPH_JAVA to java)" >> exitFailure
  let cfg = defaultConfig { hcClock = readIORef clk, hcKhanaBackend = KhanaDatalevinJvm java jar }
      tickMin n = modifyIORef' clk (+ n * 60000)

  step "write: three logicians in November (Tmp 1.. become IDs with a time in them when written)"
  h <- openHosiWith cfg dir schema
  r <- write h [ Create (Tmp 1) "person/name" (Lit (VStr "Frege")),  Create (Tmp 1) "person/age" (Lit (VInt 70))
               , Create (Tmp 2) "person/name" (Lit (VStr "Peirce")), Create (Tmp 2) "person/age" (Lit (VInt 75))
               , Create (Tmp 1) "person/friend" (RefTo (Tmp 2)) ]
  let frege = wrTemps r M.! 1
  say ("ID of Frege = " ++ show frege ++ "   (" ++ describeId frege ++ ")")
  say "The creation time is in the ID, so the ID alone tells you the month"
  tickMin 20
  r2 <- write h [ Create (Tmp 1) "person/name" (Lit (VStr "Boole")), Create (Tmp 1) "person/age" (Lit (VInt 49)) ]
  let boole = wrTemps r2 M.! 1

  step "look at now (the khana)"
  getEntity h frege >>= say . showEntity
  rows <- query h "[:find ?n ?f :where [?e :person/name ?n] [?e :person/friend ?x] [?x :person/name ?f]]" []
  say ("Datalog (friends): " ++ intercalate " ; " [ intercalate " → " (map (T.unpack . renderValue) row) | row <- rows ])
  stats h >>= \s -> say ("tail log: " ++ show (stTailEvents s) ++ " events, no chunk yet")

  step "seal: seal the tail log into chunks"
  Just sr <- seal h
  forM_ (srChunks sr) $ \c ->
    say (ceName c ++ "  [" ++ kindNameOf (ceKind c) ++ " / " ++ showMonth (ceMonth c) ++ "]  " ++ show (ceEvents c) ++ " events  hash " ++ take 12 (ceHash c) ++ "…")
  mfText <- readFile (dir </> "events" </> manifestFile)
  say ("signature line of the manifest: " ++ last (lines mfText))
  listDirectory (dir </> "events" </> "chunks" </> ceName (head (srChunks sr))) >>= say . ("inside a chunk: " ++) . unwords
  listDirectory dir >>= say . ("folders of the database: " ++) . unwords . map (++ "/")
  say "events/ = the events (the truth), khana/ = now (built from the events)"

  step "history: look up the past (MPHF). It narrows by month, then ID range, then \"is it a star?\" (fingerprint, then key)"
  (evs, tr) <- historyTrace h frege
  forM_ evs (say . showEvent)
  say (showTrace tr)
  let ghost = makeId (idMs frege + 600000) 0
  (_, tg) <- historyTrace h ghost
  say ("an ID nobody wrote (10 minutes after Frege): " ++ showTrace tg)

  step "December: Frege is 71 (the newer Create wins), and the age of Boole is deleted"
  writeIORef clk (epochMs + 338 * 86400000)
  _ <- write h [Create (Id frege) "person/age" (Lit (VInt 71)), Delete (Id boole) "person/age" (Lit (VInt 49))]
  getEntity h frege >>= say . ("Frege (now): " ++) . showEntity
  getEntity h boole >>= say . ("Boole (now): " ++) . showEntity
  Just sr2 <- seal h
  forM_ (srChunks sr2) $ \c ->
    say (ceName c ++ "  [" ++ kindNameOf (ceKind c) ++ " / " ++ showMonth (ceMonth c) ++ "]")
  say "^ The new Create and the Delete go into the chunk of the month in which the person was created (November), not the month of the write"
  history h boole >>= mapM_ (say . ("history of Boole  " ++) . showEvent)

  step "restart: the same answers"
  closeHosi h
  h2 <- openHosiWith cfg dir schema
  getEntity h2 frege >>= say . ("Frege (now): " ++) . showEntity
  verify h2 >>= \p -> say (if null p then "verify: all correct (manifest, hashes, MPHF, khana)" else "verify: " ++ show p)

  step "a bad day: the process dies in the middle of a write"
  _ <- write h2 [Create (Tmp 1) "person/name" (Lit (VStr "Schroeder")), Create (Tmp 1) "person/age" (Lit (VInt 60))]
  closeHosi h2
  size <- getFileSize (dir </> "events" </> tailFile)
  withBinaryFile (dir </> "events" </> tailFile) ReadWriteMode $ \f -> hSetFileSize f (size - 7)
  say ("the last 7 bytes of the tail log are missing (" ++ show size ++ " -> " ++ show (size - 7) ++ " bytes)")
  h3 <- openHosiWith cfg dir schema
  n <- query h3 "[:find ?n :where [?e :person/name ?n]]" []
  say ("after reopen, the half record is cut off, and these remain: " ++ intercalate ", " [ T.unpack (renderValue v) | [v] <- n ])
  say "(The khana had seen that record, so it was ahead of the events. hosigraph noticed and built it again.)"

  step "the khana (now) always rises again from the events (setsuna-metsu)"
  closeHosi h3
  mapM_ (removePathForcibly . (dir </>)) ["khana-dtlv"]
  say "removed the whole khana (all the events stay in events/)"
  h4 <- openHosiWith cfg dir schema
  getEntity h4 frege >>= say . ("Frege (now): " ++) . showEntity
  verify h4 >>= \p -> say (if null p then "verify: all correct" else "verify: " ++ show p)
  s <- stats h4
  say ("generation " ++ show (stGeneration s) ++ ", " ++ show (stChunks s) ++ " chunks, the khana has " ++ show (stKhanaDatoms s) ++ " datoms")
  closeHosi h4
  putStrLn ""
  where kindNameOf KCreate = "create"
        kindNameOf KDelete = "delete"
