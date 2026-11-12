-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- setu tests.  A plain executable, QuickCheck where "for every input" is the point.
-- Nothing here needs a database; the one check that wants a C compiler is skipped without one.
module Main (main) where

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString as B
import           Data.Char (isAlphaNum, isSpace)
import           Data.Int (Int64)
import           Data.List (nub)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import           System.Directory (findExecutable, getTemporaryDirectory, removeFile)
import           System.Exit
import           System.IO
import           System.Process (readProcessWithExitCode)
import           Test.QuickCheck

import           Takka.Edn (Edn(..), readEdn, renderEdn)
import           AbiTests
import           CodecTests
import           DispatchTests
import           Harness
import           TicketTests
import           Setu.Gen.CHeader
import           Setu.Spec
import           Setu.Validate
import           Setu.Wire

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetBuffering stdout LineBuffering
  results <- forM tests $ \(name, act) -> do
    r <- try act
    case r of
      Right () -> putStrLn ("  ok    " ++ name) >> pure True
      Left (e :: SomeException) -> putStrLn ("  FAIL  " ++ name ++ "\n      " ++ show e) >> pure False
  let failed = length (filter not results)
  putStrLn (show (length results - failed) ++ " passed, " ++ show failed ++ " failed")
  when (failed > 0) exitFailure

tests :: [Test]
tests = specTests ++ validateTests ++ wireTests ++ codecTests ++ headerTests ++ ticketTests ++ dispatchTests ++ abiTests

-- ---------------------------------------------------------------------------
-- Fixtures: two APIs.  dtlv is the second customer (the requirements' section 6.1 is its header).

dtlvApi :: Api
dtlvApi = Api
  { apiName      = "dtlv"
  , apiVersion   = 1
  , apiAppErr    = "SERVER"
  , apiAppErrDoc = "the server returned an error (the body is EDN)"
  , apiOps =
      [ Op "list-databases" "The databases on the server." TNil (TVec TStr)
      , Op "q" "A Datalog query."
          (TMap [Field "db" TStr, Field "query" TAny, Field "inputs" (TOpt (TVec TAny))]) TAny
      , Op "transact" "Transact data."
          (TMap [Field "db" TStr, Field "tx-data" (TVec TAny)]) TAny
      ]
  }

demoApi :: Api
demoApi = dtlvApi
  { apiName = "demo", apiAppErr = "APP", apiAppErrDoc = "the library refused or failed (the body is EDN)"
  , apiOps  = [ Op "write" "Write events." (TMap [Field "ops" (TVec (TTag [Variant "create" [TInt, TKw, TAny], Variant "delete" [TInt]]))]) TAny ]
  }

edn :: Text -> Edn
edn t = either (\e -> error ("test fixture is not EDN: " ++ e)) id (readEdn t)

-- ---------------------------------------------------------------------------
-- Setu.Spec

specTests :: [Test]
specTests =
  [ ("spec: the example APIs are well formed", do
      checkEq "dtlv" [] (checkApi dtlvApi)
      checkEq "demo" [] (checkApi demoApi))
  , ("spec: reserved operations are in every API, in front", do
      checkEq "names" ["ping", "abi", "list-databases", "q", "transact"] (map opName (allOps dtlvApi))
      -- :abi names the API; its answer must have the shape it promises
      check ":abi answer" (accepts (opResult (reservedOps !! 1)) (edn "{:name \"dtlv\" :version 1 :ops [:q :transact]}")))
  , ("spec: bad names are refused before any C is written", do
      refuses "upper case prefix" "api name" dtlvApi { apiName = "Dtlv" }
      refuses "empty prefix" "api name" dtlvApi { apiName = "" }
      refuses "dash in prefix" "api name" dtlvApi { apiName = "d-tlv" }
      refuses "version zero" "version" dtlvApi { apiVersion = 0 }
      refuses "lower case app error" "app error name" dtlvApi { apiAppErr = "server" }
      refuses "app error that is a fixed status" "fixed statuses" dtlvApi { apiAppErr = "IO" }
      refuses "operation name with a space" "kebab-case" dtlvApi { apiOps = [Op "Bad Name" "" TNil TNil] })
  , ("spec: reserved and repeated operations are refused", do
      refuses "ping" "reserved" dtlvApi { apiOps = [Op "ping" "" TNil TNil] }
      refuses "abi" "reserved" dtlvApi { apiOps = [Op "abi" "" TNil TNil] }
      refuses "twice" "given twice" dtlvApi { apiOps = [Op "a" "" TNil TNil, Op "a" "" TNil TNil] })
  , ("spec: shapes are checked too, and the problem says where", do
      let one t = dtlvApi { apiOps = [Op "x" "" t TNil] }
      refuses "twice field" "field \"a\" is given twice" (one (TMap [Field "a" TInt, Field "a" TStr]))
      refuses "empty enum" "at least one name" (one (TEnum []))
      refuses "enum twice" "enum name \"a\" is given twice" (one (TEnum ["a", "a"]))
      refuses "empty tag" "at least one tag" (one (TTag []))
      refuses "lonely union" "a union needs at least two shapes" (one (TUnion [TInt]))
      refuses "inside a dict" "operation \"x\", args, value: an enum needs" (one (TDict (TEnum [])))
      refuses "inside a union" "alternative 2: an enum needs" (one (TUnion [TInt, TEnum []]))
      refuses "nested" "operation \"x\", args, field \"v\", element: an enum needs" (one (TMap [Field "v" (TVec (TEnum []))]))
      refuses "bad field name" "field name \"Bad\"" (one (TMap [Field "Bad" TInt])))
  ]
  where
    refuses what needle api =
      let ps = checkApi api
      in check (what ++ ": expected a problem mentioning " ++ show needle ++ ", got " ++ show ps)
               (any (T.isInfixOf needle) ps)

-- ---------------------------------------------------------------------------
-- Setu.Validate

-- (label, shape, value, Nothing if it should be accepted, Just message if refused)
validateCases :: [(String, Ty, Edn, Maybe Text)]
validateCases =
  [ ("nil", TNil, ENil, Nothing)
  , ("bool", TBool, EBool False, Nothing)
  , ("int", TInt, edn "5", Nothing)
  , ("int max", TInt, edn "9223372036854775807", Nothing)
  , ("int min", TInt, edn "-9223372036854775808", Nothing)
  , ("int too big", TInt, edn "9223372036854775808", Just "integer out of 64-bit range: 9223372036854775808")
  , ("int too small", TInt, edn "-9223372036854775809", Just "integer out of 64-bit range: -9223372036854775809")
  , ("int is not a string", TInt, EStr "x", Just "expected integer, got string")
  , ("float", TDouble, EFloat 1.5, Nothing)
  , ("an integer is not a float", TDouble, edn "1", Just "expected float, got integer")
  , ("infinity is not a float here", TDouble, EFloat (1 / 0), Just "float is not finite")
  , ("nan is not a float here", TDouble, EFloat (0 / 0), Just "float is not finite")
  , ("string", TStr, EStr "", Nothing)
  , ("keyword", TKw, edn ":a/b", Nothing)
  , ("a symbol is not a keyword", TKw, edn "a", Just "expected keyword, got symbol")
  , ("optional, nil", TOpt TStr, ENil, Nothing)
  , ("optional, present", TOpt TStr, EStr "x", Nothing)
  , ("optional, wrong", TOpt TStr, edn "1", Just "expected string, got integer")
  , ("any", TAny, edn "{:a [1 #{2}]}", Nothing)
  , ("vector", TVec TInt, edn "[1 2 3]", Nothing)
  , ("empty vector", TVec TInt, edn "[]", Nothing)
  , ("vector, element", TVec TInt, edn "[1 \"x\"]", Just "at [1]: expected integer, got string")
  , ("a list is not a vector", TVec TInt, edn "()", Just "expected vector, got list")
  , ("enum", TEnum ["x", "y"], edn ":y", Nothing)
  , ("enum, other keyword", TEnum ["x", "y"], edn ":z", Just "expected one of :x :y, got :z")
  , ("enum, not a keyword", TEnum ["x", "y"], EStr "x", Just "expected one of :x :y, got string")
  , ("map", m1, edn "{:a 1}", Nothing)
  , ("map, key order does not matter", m2, edn "{:b \"s\" :a 1}", Nothing)
  , ("map, wrong value", m1, edn "{:a \"x\"}", Just "at [:a]: expected integer, got string")
  , ("map, missing key", m1, edn "{}", Just "missing key :a")
  , ("map, optional key left out", TMap [Field "a" (TOpt TInt)], edn "{}", Nothing)
  , ("map, optional key given nil", TMap [Field "a" (TOpt TInt)], edn "{:a nil}", Nothing)
  , ("map, key of type any left out", TMap [Field "a" TAny], edn "{}", Nothing)
  , ("map, unknown key", m1, edn "{:a 1 :b 2}", Just "at [:b]: unknown key :b, expected :a")
  , ("map, key twice", m1, edn "{:a 1 :a 1}", Just "at [:a]: key :a is given twice")
  , ("map, key that is not a keyword", m1, edn "{\"a\" 1}", Just "map keys must be keywords, got string")
  , ("map, not a map", m1, edn "[1]", Just "expected map, got vector")
  , ("dict", TDict TInt, edn "{:a 1 :b/c 2}", Nothing)
  , ("dict, empty", TDict TInt, edn "{}", Nothing)
  , ("dict, wrong value", TDict TInt, edn "{:a 1 :b \"x\"}", Just "at [:b]: expected integer, got string")
  , ("dict, key twice", TDict TInt, edn "{:a 1 :a 2}", Just "at [:a]: key :a is given twice")
  , ("dict, key that is not a keyword", TDict TInt, edn "{1 1}", Just "map keys must be keywords, got integer")
  , ("dict, not a map", TDict TInt, edn "[1]", Just "expected map, got vector")
  , ("union, first", un, edn "1", Nothing)
  , ("union, second", un, edn "\"s\"", Nothing)
  , ("union, third", un, edn "[:tmp 3]", Nothing)
  , ("union, none, says all of them", un, edn ":k", Just "expected integer or string or a tagged vector like [:tag ...], got keyword")
  , ("union, the one of the value's own kind says what is wrong", un, edn "[:tmp \"x\"]", Just "at [1]: expected integer, got string")
  , ("union, a tag that is not offered", un, edn "[:other 1]", Just "unknown tag :other, expected one of :tmp")
  , ("union with optional", TUnion [TOpt TInt, TStr], ENil, Nothing)
  , ("tag", tg, edn "[:create 1 \"a\"]", Nothing)
  , ("tag without arguments to spare", tg, edn "[:delete 1]", Nothing)
  , ("tag, unknown", tg, edn "[:nope]", Just "unknown tag :nope, expected one of :create :delete")
  , ("tag, too few arguments", tg, edn "[:delete]", Just "[:delete ...] takes 1 argument(s), got 0")
  , ("tag, too many arguments", tg, edn "[:delete 1 2]", Just "[:delete ...] takes 1 argument(s), got 2")
  , ("tag, wrong argument", tg, edn "[:create 1 2]", Just "at [2]: expected string, got integer")
  , ("tag, empty vector", tg, edn "[]", Just "expected a tagged vector like [:tag ...], got vector")
  , ("tag, first is not a keyword", tg, edn "[1 2]", Just "expected a tagged vector like [:tag ...], got vector")
  , ("tag, not a vector", tg, EStr "x", Just "expected a tagged vector like [:tag ...], got string")
  , ("deep: the path goes all the way down", deepTy, edn "{:args {:ops [[:create 1 2]]}}",
      Just "at [:args :ops 0 2]: expected string, got integer")
  , ("deep, accepted", deepTy, edn "{:args {:ops [[:create 1 \"a\"] [:delete 2]]}}", Nothing)
  ]
  where
    m1 = TMap [Field "a" TInt]
    m2 = TMap [Field "a" TInt, Field "b" TStr]
    tg = TTag [Variant "create" [TInt, TStr], Variant "delete" [TInt]]
    un = TUnion [TInt, TStr, TTag [Variant "tmp" [TInt]]]
    deepTy = TMap [Field "args" (TMap [Field "ops" (TVec tg)])]

validateTests :: [Test]
validateTests =
  [ ("validate: " ++ label, checkEq "result" expected (either (Just . renderTyError) (const Nothing) (validate ty e)))
  | (label, ty, e, expected) <- validateCases ]
  ++
  [ ("validateAt: the path starts where the value sits", do
      checkEq "one step" (Left (TyError [PKey "args"] "expected integer, got string")) (validateAt [PKey "args"] TInt (EStr "x"))
      checkEq "rendered" (Just "at [:args 0 :x]: expected integer, got string")
        (either (Just . renderTyError) (const Nothing) (validateAt [PKey "args", PIdx 0] (TMap [Field "x" TInt]) (edn "{:x \"s\"}"))))
  , ("validate: every value made for a shape passes that shape (300 shapes x values)",
      prop 300 $ forAll (genTy 3) $ \ty -> forAll (genEdnFor ty) $ \e ->
        counterexample (show ty ++ "\n" ++ show e) (validate ty e === Right ()))
  , ("validate: a wrong value in a map field is reported at that field",
      prop 300 $ forAll (genFieldsTy 3) $ \fs -> forAll (genEdnFor (TMap fs)) $ \e ->
        case e of
          EMap ps -> conjoin
            [ counterexample ("field " ++ T.unpack (fieldName f)) $
                validate (TMap fs) (EMap (replaceKey (fieldName f) bad ps ++ [(EKw (fieldName f), bad) | not (hasKey (fieldName f) ps)]))
                  === Left (TyError [PKey (fieldName f)] (either tyErrMessage (const "") (validate (fieldTy f) bad)))
            | f <- fs, Just bad <- [badFor (fieldTy f)] ]
          _ -> property False)
  , ("validate: anything is accepted for any", prop 200 $ forAll (genEdnAny 3) $ \e -> accepts TAny e)
  ]
  where
    hasKey k ps = any ((== EKw k) . fst) ps
    replaceKey k v ps = [(a, if a == EKw k then v else b) | (a, b) <- ps]

-- ---------------------------------------------------------------------------
-- Setu.Wire

wireTests :: [Test]
wireTests =
  [ ("edn: a string is written as it is, in UTF-8, and read back", do
      checkEq "text" "\"日本語\\n\\\"q\\\"\"" (renderEdn (EStr "日本語\n\"q\""))
      checkEq "read back" (Right (EStr "日本語\n\"q\"")) (readEdn (renderEdn (EStr "日本語\n\"q\""))))
  , ("edn: control characters are written as \\uXXXX", do
      checkEq "nul" "\"\\u0000\"" (renderEdn (EStr "\NUL"))
      checkEq "del" "\"\\u007f\"" (renderEdn (EStr "\DEL"))
      checkEq "read back" (Right (EStr "\NUL\DEL\ESC")) (readEdn (renderEdn (EStr "\NUL\DEL\ESC"))))
  , ("edn: a float that is not finite is written the way Clojure does", do
      checkEq "nan" "##NaN" (renderEdn (EFloat (0 / 0)))
      checkEq "inf" "##Inf" (renderEdn (EFloat (1 / 0)))
      checkEq "-inf" "##-Inf" (renderEdn (EFloat (-1 / 0))))
  , ("edn: reading back what was written (any EDN, 500 values)",
      prop 500 $ forAll (genEdnAny 3) $ \e -> readEdn (renderEdn e) === Right e)
  , ("wire: a request goes through bytes and comes back (300)",
      prop 300 $ forAll genName $ \op -> forAll (genEdnAny 2) $ \a ->
        let r = Request op a in decodeRequest (encodeRequest r) === Right r)
  , ("wire: an answer goes through bytes and comes back (300)",
      prop 300 $ forAll (oneof [ReplyOk <$> genEdnAny 2, ReplyErr <$> genName <*> (T.pack <$> arbitrary)]) $ \r ->
        decodeReply (encodeReply r) === Right r)
  , ("wire: :args may be left out, and means nil", do
      checkEq "left out" (Right (Request "ping" ENil)) (decodeRequest "{:op :ping}")
      checkEq "nil" (Right (Request "ping" ENil)) (decodeRequest "{:op :ping :args nil}")
      checkEq "written without" "{:op :ping}" (TE.decodeUtf8 (encodeRequest (Request "ping" ENil))))
  , ("wire: requests that are not envelopes", do
      checkEq "not utf-8" (Left NotUtf8) (decodeRequest (B.pack [0xff, 0xfe, 0x7b]))
      isNotEdn "unfinished" (decodeRequest "{:op")
      bad "not a map" "[1 2]" "request must be a map"
      bad "no op" "{}" ":op is missing"
      bad "op not a keyword" "{:op \"a\"}" ":op must be a keyword"
      bad "other key" "{:op :a :extra 1}" "unknown key :extra"
      bad "op twice" "{:op :a :op :b}" "key :op is given twice"
      bad "key not a keyword" "{\"op\" :a}" "keys must be keywords")
  , ("wire: answers that are not envelopes", do
      checkEq "ok" (Right (ReplyOk (EInt 1))) (decodeReply "{:ok 1}")
      checkEq "ok nil" (Right (ReplyOk ENil)) (decodeReply "{:ok nil}")
      checkEq "error" (Right (ReplyErr "boom" "x")) (decodeReply "{:error :boom :message \"x\"}")
      badR "both" "{:ok 1 :error :x :message \"m\"}" "both :ok and :error"
      badR "neither" "{}" "neither :ok nor :error"
      badR "no message" "{:error :x}" ":message must be a string"
      badR "error not a keyword" "{:error \"x\" :message \"m\"}" ":error must be a keyword"
      badR "extra" "{:ok 1 :more 2}" "unknown key :more")
  ]
  where
    isNotEdn what r = case r of
      Left (NotEdn _) -> pure ()
      other -> throwIO (userError (what ++ ": expected NotEdn, got " ++ show other))
    bad what body needle = case decodeRequest body of
      Left (BadEnvelope m) | needle `T.isInfixOf` m -> pure ()
      other -> throwIO (userError (what ++ ": expected a bad envelope mentioning " ++ show needle ++ ", got " ++ show other))
    badR what body needle = case decodeReply body of
      Left (BadEnvelope m) | needle `T.isInfixOf` m -> pure ()
      other -> throwIO (userError (what ++ ": expected a bad envelope mentioning " ++ show needle ++ ", got " ++ show other))

-- ---------------------------------------------------------------------------
-- Setu.Gen.CHeader

headerTests :: [Test]
headerTests =
  [ ("header: the shape is the one in section 6.1 of the requirements (comments and spacing aside)", do
      golden <- (cTokens . T.unpack . TE.decodeUtf8) <$> B.readFile "test/golden/dtlv-6.1.h"
      checkTokens golden (cTokens (T.unpack (cShape dtlvApi))))
  , ("header: it holds that shape as it is, and the version", do
      let h = cHeader dtlvApi
      check "includes" (cIncludes `T.isInfixOf` h)
      check "declarations" (cDecls dtlvApi `T.isInfixOf` h)
      check "version macro" ("#define DTLV_ABI_VERSION 1\n" `T.isInfixOf` h)
      check "version function" ("uint32_t dtlv_abi_version(void);" `T.isInfixOf` h)
      check "guard" ("#ifndef DTLV_H\n#define DTLV_H\n" `T.isInfixOf` h && "#endif /* DTLV_H */" `T.isInfixOf` h)
      check "c++" ("extern \"C\" {" `T.isInfixOf` h))
  , ("header: adding an operation does not change it", do
      checkEq "no ops" (cHeader dtlvApi) (cHeader dtlvApi { apiOps = [] })
      checkEq "other ops" (cHeader dtlvApi) (cHeader dtlvApi { apiOps = apiOps demoApi }))
  , ("header: the prefix, the version and the app error are the API's", do
      let h = cHeader demoApi { apiVersion = 3 }
      check "app error" ("DEMO_ERR_APP," `T.isInfixOf` h)
      check "function" ("demo_submit(demo_conn *c, demo_id id," `T.isInfixOf` h)
      check "version" ("#define DEMO_ABI_VERSION 3\n" `T.isInfixOf` h)
      check "nothing of the other API" (not ("dtlv" `T.isInfixOf` T.toLower h) && not ("SERVER" `T.isInfixOf` h)))
  , ("header: the status numbers are in the order of the ABI, and the header is C and C++", headerCompiles)
  ]

-- | C text as tokens, comments dropped: two texts that differ only in white space and comments
-- give the same tokens.
cTokens :: String -> [String]
cTokens s = case s of
  [] -> []
  ('/' : '/' : r) -> cTokens (dropWhile (/= '\n') r)
  ('/' : '*' : r) -> cTokens (dropBlock r)
  (c : r)
    | isSpace c -> cTokens r
    | isWordChar c -> let (w, r') = span isWordChar s in w : cTokens r'
    | otherwise -> [c] : cTokens r
  where
    dropBlock ('*' : '/' : r) = r
    dropBlock (_ : r) = dropBlock r
    dropBlock [] = []
    isWordChar c = isAlphaNum c || c == '_'

-- | Equal token lists; if not, say where they part.
checkTokens :: [String] -> [String] -> IO ()
checkTokens want got = unless (want == got) $ do
  let n = length (takeWhile id (zipWith (==) want got))
      around xs = unwords (take 8 (drop (max 0 (n - 4)) xs))
  throwIO (userError ("the tokens part at number " ++ show n ++ "\n      want: ... " ++ around want
                      ++ "\n      got:  ... " ++ around got))

-- | The header is read by the C compiler as C and as C++ (when a compiler is there), and the
-- statuses have the numbers the ABI fixes.
headerCompiles :: IO ()
headerCompiles = do
  mcc <- findExecutable "cc"
  case mcc of
    Nothing -> putStrLn "          (skipped: no cc on the PATH)"
    Just cc -> do
      tmp <- getTemporaryDirectory
      (hp, hh) <- openTempFile tmp "setu-test.h"
      hSetEncoding hh utf8 >> TIO.hPutStr hh (cHeader dtlvApi) >> hClose hh
      (cp, ch) <- openTempFile tmp "setu-test.c"
      hSetEncoding ch utf8 >> hPutStr ch (userOfHeader hp) >> hClose ch
      flip finally (mapM_ removeFile [hp, cp]) $
        forM_ [("C", ["-x", "c", "-std=c99"]), ("C++", ["-x", "c++", "-std=c++11"])] $ \(lang, flags) -> do
          (code, _, err) <- readProcessWithExitCode cc (flags ++ ["-Wall", "-Wextra", "-Werror", "-fsyntax-only", cp]) ""
          when (code /= ExitSuccess) $ throwIO (userError (lang ++ " compiler said:\n" ++ err))

userOfHeader :: FilePath -> String
userOfHeader hp = unlines
  [ "#include \"" ++ hp ++ "\""
  , "static void on_done(dtlv_id id, dtlv_status st, const uint8_t *buf, size_t n, void *ud) {"
  , "  (void)id; (void)st; (void)buf; (void)n; (void)ud;"
  , "}"
  , "typedef char status_numbers[(DTLV_OK == 0 && DTLV_ERR_SERVER == 1 && DTLV_ERR_PROTOCOL == 2"
  , "  && DTLV_ERR_IO == 3 && DTLV_ERR_TIMEOUT == 4 && DTLV_ERR_CANCELLED == 5"
  , "  && DTLV_ERR_CLOSED == 6 && DTLV_ERR_INTERNAL == 7) ? 1 : -1];"
  , "int setu_test_use(void) {"
  , "  dtlv_done cb = on_done;"
  , "  const uint8_t req[1] = {0};"
  , "  dtlv_conn *c;"
  , "  int r;"
  , "  if (dtlv_runtime_init() != 0) return 1;"
  , "  c = dtlv_open(\"x\", NULL);"
  , "  r = dtlv_submit(c, 1, req, sizeof req, cb, NULL);"
  , "  dtlv_cancel(c, 1);"
  , "  dtlv_close(c);"
  , "  dtlv_thread_done();"
  , "  dtlv_runtime_shutdown();"
  , "  return r + (int)dtlv_abi_version() + DTLV_ABI_VERSION;"
  , "}"
  ]

-- ---------------------------------------------------------------------------
-- Generators

genName :: Gen Text
genName = do
  c <- elements ['a' .. 'z']
  r <- vectorOf' 6 (elements (['a' .. 'z'] ++ ['0' .. '9'] ++ "-"))
  pure (T.pack (c : r))

-- | Up to n elements (zero allowed).
vectorOf' :: Int -> Gen a -> Gen [a]
vectorOf' n g = choose (0, n) >>= \k -> vectorOf k g

genSym :: Gen Text
genSym = do
  c <- elements ['a' .. 'z']
  r <- vectorOf' 6 (elements (['a' .. 'z'] ++ ['0' .. '9'] ++ "-_?!*."))
  pure (T.pack (c : r)) `suchThat` (`notElem` ["nil", "true", "false"])

genFinite :: Gen Double
genFinite = arbitrary `suchThat` (\d -> not (isNaN d || isInfinite d))

-- | Between lo and hi distinct names.
genNames :: Int -> Int -> Gen [Text]
genNames lo hi = do
  k <- choose (lo, hi)
  ns <- nub <$> vectorOf k genName
  if length ns >= lo then pure ns else genNames lo hi

genTy :: Int -> Gen Ty
genTy d
  | d <= 0 = elements [TNil, TBool, TInt, TDouble, TStr, TKw, TAny]
  | otherwise = frequency
      [ (4, genTy 0)
      , (1, TVec <$> genTy (d - 1))
      , (1, TOpt <$> genTy (d - 1))
      , (1, TDict <$> genTy (d - 1))
      , (1, do n <- choose (2, 3); TUnion <$> vectorOf n (genTy (d - 1)))
      , (2, TMap <$> genFieldsTy (d - 1))
      , (1, TEnum <$> genNames 1 4)
      , (1, TTag <$> genVariants (d - 1))
      ]

genFieldsTy :: Int -> Gen [Field]
genFieldsTy d = do
  ns <- genNames 0 4
  forM ns $ \n -> Field n <$> genTy d

genVariants :: Int -> Gen [Variant]
genVariants d = do
  ns <- genNames 1 3
  forM ns $ \n -> do
    k <- choose (0, 3)
    Variant n <$> vectorOf k (genTy d)

-- | A value of the shape (keys in any order, optional keys sometimes left out).
genEdnFor :: Ty -> Gen Edn
genEdnFor ty = case ty of
  TNil    -> pure ENil
  TBool   -> EBool <$> arbitrary
  TInt    -> (EInt . toInteger) <$> (arbitrary :: Gen Int64)
  TDouble -> EFloat <$> genFinite
  TStr    -> (EStr . T.pack) <$> arbitrary
  TKw     -> EKw <$> genName
  TAny    -> genEdnAny 2
  TVec t  -> EVec <$> vectorOf' 4 (genEdnFor t)
  TOpt t  -> oneof [pure ENil, genEdnFor t]
  TDict t -> do
    ns <- genNames 0 3
    ps <- forM ns $ \n -> (,) (EKw n) <$> genEdnFor t
    EMap <$> shuffle ps
  TUnion ts -> oneof (map genEdnFor ts)
  TEnum ns -> EKw <$> elements ns
  TMap fs -> do
    ps <- fmap concat $ forM fs $ \f -> do
      v <- genEdnFor (fieldTy f)
      -- a key may be left out when nil is fine for it
      skip <- if accepts (fieldTy f) ENil then arbitrary else pure False
      pure [(EKw (fieldName f), v) | not skip]
    EMap <$> shuffle ps
  TTag vs -> do
    v <- elements vs
    args <- mapM genEdnFor (variantArgs v)
    pure (EVec (EKw (variantTag v) : args))

-- | Any EDN that takka's reader can read (so, no tagged literals), of depth at most d.
genEdnAny :: Int -> Gen Edn
genEdnAny d = frequency $
  [ (2, pure ENil), (2, EBool <$> arbitrary), (3, EInt <$> arbitrary), (1, (EInt . (* (10 ^ (30 :: Int)))) <$> arbitrary)
  , (2, EFloat <$> genFinite), (4, (EStr . T.pack) <$> arbitrary), (3, EKw <$> genName), (2, ESym <$> genSym) ]
  ++ [ x | d > 0, x <- [ (2, EVec <$> items), (1, EList <$> items), (1, ESet <$> items)
                       , (2, EMap <$> (vectorOf' 3 ((,) <$> genEdnAny (d - 1) <*> genEdnAny (d - 1)))) ] ]
  where items = vectorOf' 4 (genEdnAny (d - 1))

-- | A value that does not have the shape (Nothing for 'TAny', which has none).
badFor :: Ty -> Maybe Edn
badFor ty = case ty of
  TAny    -> Nothing
  TOpt t  -> badFor t
  TUnion _ -> Nothing                                  -- which value is wrong for all of them depends on all of them; see the table
  TNil    -> Just (EBool True)
  TStr    -> Just (EInt 0)
  TEnum _ -> Just (EKw "not-one-of-the-names-zzz-0")   -- genName makes at most 7 characters
  _       -> Just (EStr "not this")                    -- a string is wrong for everything else
