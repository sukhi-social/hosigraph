{-# LANGUAGE OverloadedStrings #-}
-- | A small tour.  Run:  build/takka-demo [dir]
module Main (main) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Directory
import           System.Environment (getArgs)
import           System.IO (hSetEncoding, stdout, utf8)
import           Control.Exception (try)
import           Control.Monad

import           Takka

schema :: Schema
schema = M.fromList
  [ ("person/name",   identity (attr TString))
  , ("person/alias",  many (attr TString))
  , ("person/age",    attr TLong)
  , ("person/friend", many ref)
  ]

say :: String -> IO ()
say = putStrLn . ("\n● " ++)

showRows :: [[Value]] -> IO ()
showRows rows = forM_ rows $ \r -> TIO.putStrLn ("    " <> T.unwords (map renderValue r))

main :: IO ()
main = do
  hSetEncoding stdout utf8
  args <- getArgs
  let dir = case args of (d : _) -> d; _ -> "/tmp/takka-demo"
  exists <- doesDirectoryExist dir
  when exists (removeDirectoryRecursive dir)

  withConn dir schema $ \conn -> do
    say "transact: four logicians (temp ids -1.. are resolved inside the transaction)"
    rep <- transactEdn conn $ T.unlines
      [ "[{:db/id -1 :person/name \"Frege\"     :person/alias [\"Gottlob Frege\" \"G. Frege\"] :person/age 70}"
      , " {:db/id -2 :person/name \"Peirce\"    :person/age 75}"
      , " {:db/id -3 :person/name \"De Morgan\" :person/age 65 :person/friend [-2]}"
      , " {:db/id -4 :person/name \"Boole\"     :person/age 49 :person/friend [-3]}]" ]
    putStrLn ("    temp ids -> " ++ show (M.toList (txTempids rep)) ++ ", " ++ show (length (txData rep)) ++ " datoms")

    say "q: names and ages of people older than 60"
    q conn "[:find ?n ?a :where [?e :person/name ?n] [?e :person/age ?a] [(> ?a 60)]]" [] >>= showRows

    say "q: who is a friend of a friend? (join on a ref, two hops)"
    q conn "[:find ?a ?c :where [?x :person/name ?a] [?x :person/friend ?y] [?y :person/friend ?z] [?z :person/name ?c]]" [] >>= showRows

    say "q: aggregates  (avg / max / count)"
    q conn "[:find (avg ?a) (max ?a) (count ?e) :where [?e :person/age ?a]]" [] >>= showRows

    say "q: scalar input with :in"
    q conn "[:find ?n :in $ ?min :where [?e :person/name ?n] [?e :person/age ?a] [(>= ?a ?min)]]" [InScalar (VInt 70)] >>= showRows

    say "q: negation — people nobody calls a friend"
    q conn "[:find ?n :where [?e :person/name ?n] (not [_ :person/friend ?e])]" [] >>= showRows

    say "rules: transitive closure over :person/friend (recursion → fixpoint)"
    rules <- rulesEdn $ T.unlines
      [ "[[(reach ?a ?b) [?a :person/friend ?b]]"
      , "  [(reach ?a ?b) [?a :person/friend ?c] (reach ?c ?b)]]" ]
    q conn "[:find ?from ?to :in $ % :where (reach ?x ?y) [?x :person/name ?from] [?y :person/name ?to]]"
      [InRules rules] >>= showRows

    say "explain: what the planner chose (estimates come from the counted B+tree)"
    (_, plan) <- withDb conn $ \db -> explainEdn db
      "[:find ?n :where [?e :person/name ?n] [?e :person/age 65]]" []
    mapM_ TIO.putStrLn plan

    say "upsert: same :person/name, so Frege (not a duplicate) gets a new age"
    _ <- transactEdn conn "[{:person/name \"Frege\" :person/age 71}]"
    q conn "[:find ?n ?a :where [?e :person/name ?n] [?e :person/age ?a] [(clojure.string/starts-with? ?n \"F\")]]" [] >>= showRows

    say "atomicity: this transaction fails halfway — nothing is written"
    r <- tryTakka (transactEdn conn "[{:person/name \"Ghost\" :person/age 1} {:person/name \"Boole\" :person/age \"not a number\"}]")
    putStrLn ("    " ++ r)
    q conn "[:find ?n :where [?e :person/name ?n] [(= ?n \"Ghost\")]]" [] >>= \rows -> putStrLn ("    Ghost rows: " ++ show (length rows))

    say "range predicate: only the part of the age index that matters is scanned"
    (rangeRows, rangePlan) <- withDb conn $ \db -> explainEdn db
      "[:find ?n :where [?e :person/name ?n] [?e :person/age ?a] [(> ?a 60)]]" []
    showRows rangeRows
    mapM_ TIO.putStrLn rangePlan

    say "or-join: Frege by his alias, or anyone older than 70 (the age variable stays local to its branch)"
    q conn "[:find ?n :where [?e :person/name ?n] (or-join [?e] [?e :person/alias \"G. Frege\"] (and [?e :person/age ?a] [(> ?a 70)]))]" []
      >>= showRows

    say "pull: entities as trees (nested, reverse references, defaults)"
    cells <- qCells conn
      "[:find (pull ?e [:person/name (:person/age :default 0) {:person/friend [:person/name]} :person/_friend]) :where [?e :person/name \"De Morgan\"]]" []
    forM_ cells $ \row -> forM_ row $ \cell -> case cell of
      CPull p -> TIO.putStrLn ("    " <> renderPulled p)
      CVal v  -> TIO.putStrLn ("    " <> renderValue v)

    say "a large value (100,000 characters) is stored out of line and still found by value"
    let bigText = T.replicate 100000 "x"
    _ <- transact conn [EntityMap ENew [("person/name", TVal (VStr "Bigtext")), ("person/alias", TMany [TVal (VStr bigText)])]]
    found <- q conn "[:find ?n :in $ ?b :where [?e :person/alias ?b] [?e :person/name ?n]]" [InScalar (VStr bigText)]
    showRows found

    say "JSON-LD in → datoms → JSON-LD out"
    let doc = A.object
          [ "@context" A..= A.object
              [ "schema" A..= ("http://schema.org/" :: Text)
              , "name"   A..= ("schema:name" :: Text)
              , "knows"  A..= A.object ["@id" A..= ("schema:knows" :: Text), "@type" A..= ("@id" :: Text)] ]
          , "@id" A..= ("http://example.org/alice" :: Text)
          , "@type" A..= ("schema:Person" :: Text)
          , "name" A..= ("Alice" :: Text)
          , "knows" A..= [ A.object ["@id" A..= ("http://example.org/bob" :: Text), "name" A..= ("Bob" :: Text)]
                         , A.String "http://example.org/carol" ] ]
    _ <- importJsonLD conn doc
    q conn "[:find ?who ?other :where [?a :http://schema.org/name ?who] [?a :http://schema.org/knows ?b] [?b :jsonld/id ?other]]" [] >>= showRows
    out <- withDb conn $ \db -> exportByIri db
             defaultExportOptions { eoContext = M.fromList [("schema", "http://schema.org/")] }
             ["http://example.org/alice"]
    BL.putStrLn (A.encode out)

    n <- withDb conn datomCount
    say ("closing; datoms stored: " ++ show n)

  withConn dir schema $ \conn -> do
    say "reopened the same directory — data and schema persisted"
    q conn "[:find (count ?e) :where [?e :person/name _]]" [] >>= showRows

-- | Run an action that is expected to be rejected by the database.
tryTakka :: IO a -> IO String
tryTakka act = either (\e -> "rejected: " ++ show (e :: TakkaError)) (const "no error?!") <$> try act
