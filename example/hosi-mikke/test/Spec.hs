-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- The tests of hosi-mikke.  A plain executable, like the one of hosigraph (its own small harness, no framework).
--
--   HOSIGRAPH_SUDACHI        the sudachi program (sudachi.rs)         -- the tokenizer tests
--   HOSIGRAPH_SUDACHI_DICT   its dictionary (system_core.dic)         -- (both are needed)
--   HOSIGRAPH_DTLV           the native dtlv                          -- the khana tests, KhanaDtlvRepl
--   HOSIGRAPH_DATALEVIN_JAR  the Datalevin standalone jar             -- ... and KhanaDatalevinJvm (HOSIGRAPH_JAVA: java)
--   HOSIGRAPH_TEST_ONLY      a text: run only the tests whose name has it in it
--
-- What is not set is skipped, and said so.  The pure test needs nothing.
module Main (main) where

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString as B
import           Data.IORef
import           Data.List (isInfixOf, sort)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           System.Directory
import           System.Environment (lookupEnv)
import           System.Exit
import           System.FilePath ((</>))
import qualified System.IO as SysIO
import           System.IO

import           Hosigraph
import           Hosigraph.Id (epochMs)
import           Hosigraph.Mikke.Sudachi

-- ---------------------------------------------------------------------------
-- A very small harness

type Test = (String, IO ())

check :: String -> Bool -> IO ()
check msg ok = unless ok (throwIO (userError msg))

checkEq :: (Eq a, Show a) => String -> a -> a -> IO ()
checkEq msg expected actual =
  unless (expected == actual) $
    throwIO (userError (msg ++ "\n      expected: " ++ show expected ++ "\n      actual:   " ++ show actual))

nonEmpty :: String -> Maybe String
nonEmpty x = if null x then Nothing else Just x

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetBuffering stdout LineBuffering
  mexe     <- (>>= nonEmpty) <$> lookupEnv "HOSIGRAPH_DTLV"
  mjar     <- (>>= nonEmpty) <$> lookupEnv "HOSIGRAPH_DATALEVIN_JAR"
  mjava    <- (>>= nonEmpty) <$> lookupEnv "HOSIGRAPH_JAVA"
  msudachi <- (>>= nonEmpty) <$> lookupEnv "HOSIGRAPH_SUDACHI"
  mdict    <- (>>= nonEmpty) <$> lookupEnv "HOSIGRAPH_SUDACHI_DICT"
  only     <- (>>= nonEmpty) <$> lookupEnv "HOSIGRAPH_TEST_ONLY"
  let java = maybe "java" id mjava
      tag t = map (\(n, a) -> (t ++ " " ++ n, a))
      sudachiRun = case (msudachi, mdict) of
        (Just sx, Just dic) ->
          let spec = SudachiSpec sx dic ModeA
          in tag "[sudachi]" (sudachiTests spec)
             ++ maybe [] (\exe -> tag "[dtlv-repl+sudachi]" (wordsTests (KhanaDtlvRepl exe) spec)) mexe
             ++ maybe [] (\jar -> tag "[jvm+sudachi]" (wordsTests (KhanaDatalevinJvm java jar) spec)) mjar
        _ -> []
  when (null sudachiRun) $ putStrLn "  (sudachi tests skipped: set HOSIGRAPH_SUDACHI to the sudachi program and HOSIGRAPH_SUDACHI_DICT to its dictionary)"
  when (not (null sudachiRun) && mexe == Nothing) $ putStrLn "  (khana tests skipped: set HOSIGRAPH_DTLV to the path of the dtlv executable)"
  let wanted (name, _) = maybe True (`isInfixOf` name) only
  results <- forM (filter wanted (pureTests ++ sudachiRun)) $ \(name, act) -> do
    r <- try act
    case r of
      Right () -> putStrLn ("  ok    " ++ name) >> pure True
      Left (e :: SomeException) -> putStrLn ("  FAIL  " ++ name ++ "\n      " ++ show e) >> pure False
  let failed = length (filter not results)
  putStrLn (show (length results - failed) ++ " passed, " ++ show failed ++ " failed")
  when (failed > 0) exitFailure

-- ---------------------------------------------------------------------------
-- Helpers (the same as in the tests of hosigraph)

-- | A fresh directory, removed afterwards.
withTmp :: (FilePath -> IO a) -> IO a
withTmp act = do
  base <- getTemporaryDirectory
  t <- getPOSIXTime
  let dir = base </> ("hosi-mikke-test-" ++ show (floor (t * 1000000) :: Integer))
  removePathForcibly dir
  createDirectoryIfMissing True dir
  act dir `finally` removePathForcibly dir

-- | A clock we can move by hand: 2026-11-11 00:00 UTC to begin with.
nov11 :: Integer
nov11 = epochMs + 314 * 86400000

newClock :: Integer -> IO (IORef Integer, Clock)
newClock t0 = do
  r <- newIORef t0
  pure (r, readIORef r)

backendCfg :: KhanaBackend -> HosiConfig
backendCfg be = defaultConfig { hcKhanaBackend = be }

-- | How many times these bytes stand in the files under a directory (all of them, however deep).  The tail log is
-- left out when it is open (GHC does not let one read a file another handle of the same process has open
-- for writing): the tests that count with an open world check that its tail is empty.
markerCount :: B.ByteString -> FilePath -> IO Int
markerCount marker root = do
  names <- filter (/= "tail.log") <$> listDirectory root
  fmap sum . forM names $ \n -> do
    let p = root </> n
    isDir <- doesDirectoryExist p
    if isDir then markerCount marker p else countFile p
  where
    -- read in blocks of 4 MB: on Linux Datalevin's data.mdb is a sparse file of 1 GB, and B.readFile took it whole
    countFile p = SysIO.withBinaryFile p SysIO.ReadMode (\hd -> blocks hd 0 B.empty)
    keep = B.length marker - 1
    blocks hd n carry = do
      blk <- B.hGet hd (4 * 1024 * 1024)
      if B.null blk then pure n else do
        let hay = carry <> blk
        let n' = n + countIn 0 hay
        n' `seq` blocks hd n' (B.copy (B.drop (B.length hay - keep) hay))
    countIn n hay =
      let rest = snd (B.breakSubstring marker hay)
      in if B.null rest then n else countIn (n + 1 :: Int) (B.drop (B.length marker) rest)

-- ---------------------------------------------------------------------------
-- Pure

pureTests :: [Test]
pureTests =
  [ ("words of an analysis: what is kept of what sudachi printed (the normal form; ASCII as written; no particles, auxiliaries, punctuation or blanks)", do
      let tsv surface pos norm = T.intercalate "\t" [surface, pos, norm, surface, "", "0", "[]", ""]
          analysis = [ tsv "東京" "名詞,固有名詞,地名,一般,*,*" "東京", tsv "に" "助詞,格助詞,*,*,*,*" "に"
                     , tsv "食べ" "動詞,一般,*,*,下一段-バ行,連用形-一般" "食べる", tsv "まし" "助動詞,*,*,*,助動詞-マス,連用形-一般" "ます"
                     , tsv "。" "補助記号,句点,*,*,*,*" "。", tsv " " "空白,*,*,*,*,*" " "
                     , tsv "ネコ" "名詞,普通名詞,一般,*,*,*" "猫", tsv "Hello" "名詞,普通名詞,一般,*,*,*" "ハロー", tsv "ABC" "名詞,普通名詞,一般,*,*,*" "ABC"
                       -- sudachi calls the colon of a custom emoji (:name:, with a zero-width space in it) a noun, and so a lone %: not words
                     , tsv ":\x200b" "名詞,普通名詞,サ変可能,*,*,*" ":\x200b", tsv "\x200b:" "名詞,普通名詞,サ変可能,*,*,*" "\x200b:", tsv "%" "名詞,普通名詞,一般,*,*,*" "%"
                     , tsv "2024" "名詞,数詞,*,*,*,*" "2024" ]
      checkEq "the words (a word with no letter or digit in it is not one)" ["東京", "食べる", "猫", "hello", "abc", "2024"] (wordsOfAnalysis analysis)
      checkEq "a short line is a line" ["abc"] (lineOfText "abc")
      checkEq "a newline or a tab is a space: it would end the text early" ["a b c"] (lineOfText "a\nb\tc")
      checkEq "an empty text is one empty line" [""] (lineOfText "")
      checkEq "a long text goes in pieces" [chunkChars, chunkChars, 10] (map T.length (lineOfText (T.replicate (2 * chunkChars + 10) "猫")))
      checkEq "...of the same text, in the same order" (T.replicate (2 * chunkChars + 10) "猫") (T.concat (lineOfText (T.replicate (2 * chunkChars + 10) "猫"))))
  ]

-- ---------------------------------------------------------------------------
-- The tokenizer by itself: it needs the program and the dictionary, and no khana

sudachiTests :: SudachiSpec -> [Test]
sudachiTests spec =
  [ ("texts become their words (the normal form, joined by a space)", do
      tk <- newSudachi spec
      [a, b, c, d, e] <- tkWords tk ["東京都に住んでいます。", "Hello, World!", "", "a\nb", "食べました"]
      check "東京都 is 東京 and 都 (mode A)" (all (`elem` T.words a) ["東京", "都", "住む"])
      check "the particle is not a word" ("に" `notElem` T.words a)
      checkEq "English stays as written, in lower case" "hello world" b
      checkEq "an empty text has no words" "" c
      checkEq "a newline is a space" "a b" d
      checkEq "an inflected word is its dictionary form" "食べる" e
      -- many at once, in the order given; a text in pieces is one text
      many' <- tkWords tk (replicate 500 "猫と犬。")
      checkEq "five hundred texts, five hundred answers" 500 (length many')
      check "each is its own" (all (== "猫 犬") many')
      [long] <- tkWords tk [T.replicate 2000 "猫と犬。"]
      checkEq "8000 characters, in three pieces, and every word is there" 4000 (length (T.words long))
      -- closed, it is asked again: the process starts again
      tkClose tk
      checkEq "after tkClose the next text starts it again" ["猫"] =<< tkWords tk ["猫"]
      tkClose tk)

  , ("the mark says which dictionary and which way of cutting, not where the program is", do
      a <- sudachiMark spec
      b <- sudachiMark spec { sudachiExe = "/elsewhere/sudachi" }
      c <- sudachiMark spec { sudachiMode = ModeC }
      checkEq "the path of the program is not in it" a b
      check "the way of cutting is" (a /= c)
      tk <- newSudachi spec
      checkEq "tkMark is that mark" a (tkMark tk))

  , ("a dictionary that cannot be read is an error at the first text, and the tokenizer is not left half open", withTmp $ \dir -> do
      let bad = dir </> "not-a-dictionary"
      B.writeFile bad "this is not a dictionary"
      tk <- newSudachi spec { sudachiDict = bad }
      r <- try (tkWords tk ["猫"])
      check "SudachiError" (case r of Left (SudachiError _) -> True; Right _ -> False)
      tkClose tk)

  , ("a program or a dictionary that is not there is BackendMissing, found before anything is started", do
      let missing sp = either (\e -> case e of BackendMissing _ -> True; _ -> False) (const False)
                         <$> (try (newSudachi sp) :: IO (Either HosiError Tokenizer))
      checkEq "no program" True =<< missing spec { sudachiExe = "/no/such/sudachi" }
      checkEq "no dictionary" True =<< missing spec { sudachiDict = "/no/such/system.dic" }
      checkEq "(and the ones that are there are not)" False =<< missing spec)
  ]

-- ---------------------------------------------------------------------------
-- Fulltext attributes whose words sudachi makes, on a khana of any backend

wordsTests :: KhanaBackend -> SudachiSpec -> [Test]
wordsTests be spec =
  [ ("words: Japanese is found by its words (no 京都 inside 東京都; one character; another form; another spelling); an erased note is found no more, and clean leaves its words in no file", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      tk <- newSudachi spec
      let cfg = (backendCfg be) { hcClock = clock, hcTokenizer = Just tk }
          sch = M.fromList [ ("note/body", fulltext (attr TString)), ("note/by", attr TString) ]
          search h q = sort <$> query h "[:find ?e :in $ ?q :where [(fulltext $ :note/body ?q) [[?e ?a ?v]]]]" [InScalar (VStr q)]
          note h t = (\r -> wrTemps r M.! 1) <$> write h [Create (Tmp 1) "note/body" (Lit (VStr t)), Create (Tmp 1) "note/by" (Lit (VStr "x"))]
          ids = map (\i -> [VInt (fromIntegral i)])
      h <- openHosiWith cfg dir sch
      tokyo <- note h "東京都の人口が増えているそうです。"
      kyoto <- note h "来月、京都に旅行する予定です。"
      eat   <- note h "昨日のランチは寿司を食べました。"
      cat   <- note h "うちの猫が毎朝起こしに来る。"
      neko  <- note h "ネコ動画を見て癒やされる。"
      mov1  <- note h "来週引っ越しをします。"
      mov2  <- note h "引越し業者の見積もりを取った。"
      en    <- note h "Hello, World! Rust 3 jumps"
      secret <- note h "秘密の言葉を話す"
      checkEq "京都 is the note that says 京都, not the one that says 東京都" (ids [kyoto]) =<< search h "京都"
      checkEq "東京 is found in 東京都" (ids [tokyo]) =<< search h "東京"
      checkEq "a word in another form: 食べる finds 食べました" (ids [eat]) =<< search h "食べる"
      checkEq "one character is a word: 猫 finds 猫 and ネコ" (ids [cat, neko]) =<< search h "猫"
      checkEq "a spelling: 引っ越し finds 引越し" (ids [mov1, mov2]) =<< search h "引っ越し"
      checkEq "English as written, in any case" (ids [en]) =<< search h "WORLD"
      checkEq "a number" (ids [en]) =<< search h "3"
      checkEq "a word that is nowhere" [] =<< search h "ユニコーン"
      checkEq "the text in a search is made into words (a phrase: both words)" (ids [kyoto]) =<< search h "来月の京都"
      checkEq "the text may be written in the query" (ids [kyoto])
        =<< (sort <$> query h "[:find ?e :where [(fulltext $ :note/body \"京都\") [[?e ?a ?v]]]]" [])
      checkEq "with no attribute, every fulltext attribute is searched" (ids [kyoto])
        =<< (sort <$> query h "[:find ?e :in $ ?q :where [(fulltext $ ?q) [[?e _ _]]]]" [InScalar (VStr "京都")])
      checkEq "{:top 1} gives the best one only" 1 . length
        =<< query h "[:find ?e :in $ ?q :where [(fulltext $ :note/body ?q {:top 1}) [[?e ?a ?v]]]]" [InScalar (VStr "猫")]
      checkEq "the other attribute is not searched" [] =<< search h "x"
      -- the text is kept as written: the words are beside it
      ent <- getEntity h cat
      checkEq "the attribute has the text as written" (Just [VStr "うちの猫が毎朝起こしに来る。"]) (M.lookup "note/body" ent)
      check "...and its words are in another attribute" (M.member "note/body.words" ent)
      checkEq "verify" [] =<< verify h
      -- it follows the data: a value taken out is not found, a new one is
      _ <- write h [Delete (Id cat) "note/body" (Lit (VStr "うちの猫が毎朝起こしに来る。")), Create (Id cat) "note/body" (Lit (VStr "うちの犬が吠える。"))]
      checkEq "the old words of a changed value are gone" (ids [neko]) =<< search h "猫"
      checkEq "...and the new ones are there" (ids [cat]) =<< search h "犬"
      -- a value taken out and nothing put in its place (a new value would replace the words by itself: this is the Delete alone)
      gone <- note h "象の鼻が長い"
      checkEq "(found before)" (ids [gone]) =<< search h "象"
      _ <- write h [Delete (Id gone) "note/body" (Lit (VStr "象の鼻が長い"))]
      checkEq "a value taken out is found no more" [] =<< search h "象"
      -- a note with no words, and then with some: the old words do not stay
      empty <- note h ""
      _ <- write h [Delete (Id empty) "note/body" (Lit (VStr "")), Create (Id empty) "note/body" (Lit (VStr "雨の日"))]
      checkEq "a text that had no words, and now has" (ids [empty]) =<< search h "雨"
      -- an erased note is not found, and after clean its words are in no file
      before <- markerCount (TE.encodeUtf8 "秘密") dir
      check "(the words are in some file before an erase)" (before > 0)
      _ <- erase h [EraseIds [secret]]
      checkEq "an erased note is not found" [] =<< search h "秘密"
      rep <- clean h defaultCleanOptions
      check "clean made the khana again, as it must for an index of words" (crKhanaCleaned rep && crKhanaRebuilt rep)
      checkEq "its words are in no file" 0 =<< markerCount (TE.encodeUtf8 "秘密") dir
      checkEq "the others are still found" (ids [kyoto]) =<< search h "京都"
      checkEq "verify after clean" [] =<< verify h
      closeHosi h
      -- closed and opened again, with the same words (the world stopped the process; it starts again): the khana is kept, not made again
      h2 <- openHosiWith cfg dir sch
      checkEq "found after a reopen" (ids [kyoto]) =<< search h2 "京都"
      closeHosi h2
      -- other words (the longest unit: 東京都 is one word) are another khana, made again from the events
      tkC <- newSudachi spec { sudachiMode = ModeC }
      h3 <- openHosiWith (cfg { hcTokenizer = Just tkC }) dir sch
      checkEq "mode C: 東京都 is one word, found whole" (ids [tokyo]) =<< search h3 "東京都"
      checkEq "...and 東京 alone is not in it" [] =<< search h3 "東京"
      closeHosi h3
      -- no tokenizer: the schema is a fulltext one, and only the JVM has an analyzer of its own
      r <- try (openHosiWith (cfg { hcTokenizer = Nothing }) dir sch) :: IO (Either HosiError Hosi)
      case be of
        KhanaDatalevinJvm _ _ -> either (const (pure ())) closeHosi r
        _ -> check "without a tokenizer the native dtlv refuses a fulltext attribute" (case r of Left (SchemaRefused _) -> True; _ -> False))

  , ("words: a Japanese word that mode A cuts into several (誕生日 is 誕生 and 日) finds the notes that have all of its words", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      tk <- newSudachi spec
      let sch = M.fromList [ ("note/body", fulltext (attr TString)) ]
          search h q = sort <$> query h "[:find ?e :in $ ?q :where [(fulltext $ :note/body ?q) [[?e ?a ?v]]]]" [InScalar (VStr q)]
          note h t = (\r -> wrTemps r M.! 1) <$> write h [Create (Tmp 1) "note/body" (Lit (VStr t))]
          ids = sort . map (\i -> [VInt (fromIntegral i)])
      h <- openHosiWith ((backendCfg be) { hcClock = clock, hcTokenizer = Just tk }) dir sch
      party <- note h "今日は誕生日パーティーをします。"
      apart <- note h "誕生を祝う日が来た。"
      birth <- note h "誕生を祝う歌を歌った。"
      rainy <- note h "雨の日は傘をさす。"
      checkEq "誕生日 finds the notes that have 誕生 and 日, in a row or not, and not the one that has 誕生 or the one that has 日" (ids [party, apart]) =<< search h "誕生日"
      checkEq "...the same when the words are written apart" (ids [party, apart]) =<< search h "誕生 日"
      checkEq "one of its words is still found on its own" (ids [party, apart, birth]) =<< search h "誕生"
      checkEq "...and the other" (ids [party, apart, rainy]) =<< search h "日"
      one <- sort <$> query h "[:find ?e :in $ ?q :where [(fulltext $ :note/body ?q {:top 1}) [[?e ?a ?v]]]]" [InScalar (VStr "誕生日")]
      check "{:top 1}: at most one, and one that has both" (length one <= 1 && all (`elem` ids [party, apart]) one)
      closeHosi h)

  , ("words: a runtime made with a tokenizer is the one a Hosi opened on it uses", withTmp $ \dir -> do
      (_, clock) <- newClock nov11
      let sch = M.fromList [ ("note/body", fulltext (attr TString)) ]
      tk <- newSudachi spec
      rt <- newKhanaRuntimeWith be (Just tk)
      h <- openHosiShared rt ((backendCfg be) { hcClock = clock }) dir sch
      r <- write h [Create (Tmp 1) "note/body" (Lit (VStr "京都に行く"))]
      found <- query h "[:find ?e :in $ ?q :where [(fulltext $ :note/body ?q) [[?e]]]]" [InScalar (VStr "京都")]
      checkEq "found" [[VInt (fromIntegral (wrTemps r M.! 1))]] found
      closeHosi h
      closeKhanaRuntime rt)
  ]
