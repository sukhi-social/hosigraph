{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
-- | Tests of "Setu.Codec": what the Generic way writes, what it reads back, and what it refuses.
module CodecTests (codecTests) where

import           Data.Int (Int64)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Test.QuickCheck

import           Harness
import           Setu.Codec
import           Takka.Edn (Edn(..), readEdn, renderEdn)
import           Takka.Types (Keyword(..))

-- ---------------------------------------------------------------------------
-- Types to write and read

data Stats = Stats
  { stTailBytes :: Integer, stChunks :: Int, stLabel :: String, stFlags :: [Bool], stRatio :: Double
  , stNote :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToEdn Stats where toEdn = genericToEdn defaultStyle
instance FromEdn Stats where fromEdn = genericFromEdn defaultStyle

data Shape = Dot | Circle Int | Rect Int Int | Group Text [Shape]
  deriving (Eq, Show, Generic)

instance ToEdn Shape where toEdn = genericToEdn defaultStyle
instance FromEdn Shape where fromEdn = genericFromEdn defaultStyle

data Color = Red | DarkBlue deriving (Eq, Show, Generic)

instance ToEdn Color where toEdn = genericToEdn defaultStyle
instance FromEdn Color where fromEdn = genericFromEdn defaultStyle

-- | A record with a field that stays at home.
data Account = Account { acName :: Text, acKey :: Text } deriving (Eq, Show, Generic)

instance ToEdn Account where toEdn = genericToEdn defaultStyle { styleSkip = (== "acKey") }
instance FromEdn Account where fromEdn = genericFromEdn defaultStyle { styleSkip = (== "acKey") }

genText :: Gen Text
genText = T.pack <$> arbitrary

genStats :: Gen Stats
genStats = Stats <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                 <*> (arbitrary `suchThat` (\d -> not (isNaN d || isInfinite d)))
                 <*> oneof [pure Nothing, Just <$> genText]

-- | A tree at most @depth@ deep, at most three children to a group.  (Not @listOf@ at the size of
-- the outer test: the children would be as many as the size, level after level, and the tree
-- would not fit in memory.)
genShape :: Int -> Gen Shape
genShape depth = oneof $
  [pure Dot, Circle <$> arbitrary, Rect <$> arbitrary <*> arbitrary]
  ++ [Group <$> genText <*> (choose (0, 3) >>= \k -> vectorOf k (genShape (depth - 1))) | depth > 0]

-- | Through the text that goes on the wire and back.
viaText :: (ToEdn a, FromEdn a) => a -> Either Text a
viaText = either (Left . T.pack) fromEdn . readEdn . renderEdn . toEdn

-- ---------------------------------------------------------------------------

codecTests :: [Test]
codecTests =
  [ ("codec: a record is a map in the order of its fields, without the prefix, with hyphens", do
      checkEq "stats"
        (EMap [ (EKw "tail-bytes", EInt 40), (EKw "chunks", EInt 3), (EKw "label", EStr "a")
              , (EKw "flags", EVec [EBool True]), (EKw "ratio", EFloat 0.5), (EKw "note", ENil) ])
        (toEdn (Stats 40 3 "a" [True] 0.5 Nothing)))
  , ("codec: a constructor is a keyword, with its fields a vector that starts with it", do
      checkEq "no fields" (EKw "dot") (toEdn Dot)
      checkEq "one" (EVec [EKw "circle", EInt 3]) (toEdn (Circle 3))
      checkEq "two" (EVec [EKw "rect", EInt 1, EInt 2]) (toEdn (Rect 1 2))
      checkEq "nested" (EVec [EKw "group", EStr "g", EVec [EKw "dot", EVec [EKw "circle", EInt 1]]])
        (toEdn (Group "g" [Dot, Circle 1]))
      checkEq "hyphens" (EKw "dark-blue") (toEdn DarkBlue))
  , ("codec: a record goes out and comes back, also through the text of the wire (300)",
      prop 300 $ forAll genStats $ \s -> fromEdn (toEdn s) === Right s .&&. viaText s === Right s)
  , ("codec: a sum goes out and comes back, also through the text of the wire (300)",
      prop 300 $ forAll (genShape 4) $ \s -> fromEdn (toEdn s) === Right s .&&. viaText s === Right s)
  , ("codec: nil is Nothing, and a key that is missing reads as nil", do
      checkEq "nil" (Right Nothing) (fromEdn ENil :: Either Text (Maybe Int))
      checkEq "a value" (Right (Just 4)) (fromEdn (EInt 4) :: Either Text (Maybe Int))
      let withoutNote = EMap [ (EKw "tail-bytes", EInt 1), (EKw "chunks", EInt 2), (EKw "label", EStr "")
                             , (EKw "flags", EVec []), (EKw "ratio", EFloat 1.5) ]
      checkEq "no :note" (Right (Stats 1 2 "" [] 1.5 Nothing)) (fromEdn withoutNote))
  , ("codec: what does not fit is refused, and the message says where", do
      checkEq "a tag nobody has" (Left "unknown tag :hexagon") (fromEdn (EVec [EKw "hexagon", EInt 1]) :: Either Text Shape)
      checkEq "a keyword nobody has" (Left "unknown keyword :green") (fromEdn (EKw "green") :: Either Text Color)
      checkEq "not a tagged vector" (Left "not a tagged vector") (fromEdn (EInt 1) :: Either Text Shape)
      checkEq "too few" (Left "too few arguments") (fromEdn (EVec [EKw "rect", EInt 1]) :: Either Text Shape)
      checkEq "too many" (Left "too many arguments to :circle") (fromEdn (EVec [EKw "circle", EInt 1, EInt 2]) :: Either Text Shape)
      checkEq "the field" (Left ":chunks: not an integer")
        (fromEdn (EMap [ (EKw "tail-bytes", EInt 1), (EKw "chunks", EStr "x") ]) :: Either Text Stats)
      checkEq "not a map" (Left "not a map") (fromEdn (EVec []) :: Either Text Stats))
  , ("codec: an integer that does not fit is refused, not wrapped around", do
      checkEq "largest" (Right maxBound) (fromEdn (EInt (toInteger (maxBound :: Int64))) :: Either Text Int64)
      checkEq "one more" (Left "an integer out of range") (fromEdn (EInt (toInteger (maxBound :: Int64) + 1)) :: Either Text Int64)
      checkEq "below zero" (Left "an integer out of range") (fromEdn (EInt (-1)) :: Either Text Word64)
      checkEq "an Integer has no limit" (Right (2 ^ (100 :: Int))) (fromEdn (EInt (2 ^ (100 :: Int))) :: Either Text Integer)
      checkEq "a float is not an integer" (Left "not an integer") (fromEdn (EFloat 1) :: Either Text Int)
      checkEq "an integer is not a float" (Left "not a float") (fromEdn (EInt 1) :: Either Text Double))
  , ("codec: a field that is skipped is not written, and so not read", do
      checkEq "written" (EMap [(EKw "name", EStr "a")]) (toEdn (Account "a" "secret"))
      checkEq "read" (Left ":key: not a string") (fromEdn (EMap [(EKw "name", EStr "a")]) :: Either Text Account))
  , ("codec: a list of characters is a string, and other lists are vectors", do
      checkEq "string" (EStr "ab") (toEdn ("ab" :: String))
      checkEq "strings" (EVec [EStr "a", EStr "b"]) (toEdn ["a", "b" :: String])
      checkEq "ints" (EVec [EInt 1, EInt 2]) (toEdn [1, 2 :: Int])
      checkEq "back" (Right ["a", "bc"]) (fromEdn (EVec [EStr "a", EStr "bc"]) :: Either Text [String])
      checkEq "not a vector" (Left "not a vector") (fromEdn (EInt 1) :: Either Text [Int])
      checkEq "not a string" (Left "not a string") (fromEdn (EInt 1) :: Either Text String))
  , ("codec: a map with keyword keys goes out in the order of the keys and comes back", do
      let m = M.fromList [(Keyword "b", [1 :: Int]), (Keyword "a", [2, 3])]
      checkEq "out" (EMap [(EKw "a", EVec [EInt 2, EInt 3]), (EKw "b", EVec [EInt 1])]) (toEdn m)
      checkEq "back" (Right m) (fromEdn (toEdn m)))
  , ("codec: the words of a name", do
      checkEq "prefix" "FirstSeq" (withoutPrefix "wrFirstSeq")
      checkEq "a longer prefix" "MinId" (withoutPrefix "ceMinId")
      checkEq "no prefix" "name" (withoutPrefix "name")
      checkEq "no lower-case letters before the capital" "Name" (withoutPrefix "Name")
      checkEq "digits are not a prefix" "a1B" (withoutPrefix "a1B")
      checkEq "hyphens" "first-seq" (kebab "FirstSeq")
      checkEq "the first letter has none before it" "dark-blue" (kebab "DarkBlue"))
  ]
