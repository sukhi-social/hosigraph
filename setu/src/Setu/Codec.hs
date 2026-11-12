{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module      : Setu.Codec
-- Description : Between EDN and Haskell types: two classes, and a Generic way to write the instances of records and sums.
--
-- A handler of an API turns the EDN it is given into its own types and its own types back into
-- EDN.  'ToEdn' and 'FromEdn' are the two directions.  For a record or a sum the instance can be
-- one line:
--
-- > data Stats = Stats { stChunks :: Int, stTailBytes :: Integer } deriving Generic
-- > instance ToEdn Stats where toEdn = genericToEdn defaultStyle
-- >
-- > toEdn (Stats 3 40)   -- {:chunks 3, :tail-bytes 40}
--
-- What the Generic way writes:
--
-- * a record (one constructor, with field names) is a map with keyword keys, in the order of the
--   fields; 'defaultStyle' drops the lower-case prefix of a field name (@stChunks@ is @:chunks@)
--   and spells the rest with hyphens (@stTailBytes@ is @:tail-bytes@);
-- * a constructor without fields is a keyword (@DarkBlue@ is @:dark-blue@);
-- * a constructor with fields is a vector that starts with its keyword (@Rect 1 2@ is @[:rect 1 2]@).
--
-- A record is for a type with one constructor: it is written without the constructor's name, so a
-- record among several constructors could not be told apart when it is read.
--
-- What does not fit these shapes (a type that is read by the shape of the EDN, like a value that
-- is an integer, a string, ...) is a plain instance, written by hand.
module Setu.Codec
  ( ToEdn(..), FromEdn(..)
    -- * Instances by Generic
  , Style(..), defaultStyle, withoutPrefix, kebab
  , genericToEdn, genericFromEdn
  ) where

import           Data.Char (isLower, isUpper, toLower)
import           Data.Int (Int64)
import           Data.Kind (Constraint, Type)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word (Word64)
import           GHC.Generics

import           Takka.Edn (Edn(..))
import           Takka.Types (Keyword(..))

-- ---------------------------------------------------------------------------
-- The two classes

class ToEdn a where
  toEdn :: a -> Edn
  -- | How a list of this type is written.  (There so that a list of characters can be a string.)
  toEdnList :: [a] -> Edn
  toEdnList = EVec . map toEdn

class FromEdn a where
  -- | The error is a message for people (the dispatcher tells it as the library's own mistake:
  -- the shape of the EDN has been checked against its 'Setu.Spec.Ty' before a handler reads it).
  fromEdn :: Edn -> Either Text a
  fromEdnList :: Edn -> Either Text [a]
  fromEdnList e = case e of
    EVec xs -> traverse fromEdn xs
    _       -> Left "not a vector"

instance ToEdn Edn where toEdn = id
instance FromEdn Edn where fromEdn = Right

instance ToEdn Bool where toEdn = EBool
instance FromEdn Bool where
  fromEdn e = case e of
    EBool b -> Right b
    _       -> Left "not a boolean"

instance ToEdn Int where toEdn = EInt . toInteger
instance FromEdn Int where fromEdn = boundedInteger
instance ToEdn Int64 where toEdn = EInt . toInteger
instance FromEdn Int64 where fromEdn = boundedInteger
instance ToEdn Word64 where toEdn = EInt . toInteger
instance FromEdn Word64 where fromEdn = boundedInteger
instance ToEdn Integer where toEdn = EInt
instance FromEdn Integer where
  fromEdn e = case e of
    EInt n -> Right n
    _      -> Left "not an integer"

-- | An integer that does not fit is refused, not wrapped around.
boundedInteger :: forall a. (Integral a, Bounded a) => Edn -> Either Text a
boundedInteger e = case e of
  EInt n
    | n >= toInteger (minBound :: a) && n <= toInteger (maxBound :: a) -> Right (fromInteger n)
    | otherwise -> Left "an integer out of range"
  _ -> Left "not an integer"

instance ToEdn Double where toEdn = EFloat
-- | A float is a float: an integer is not accepted (as 'Setu.Spec.TDouble' does not accept it).
instance FromEdn Double where
  fromEdn e = case e of
    EFloat d -> Right d
    _        -> Left "not a float"

instance ToEdn Text where toEdn = EStr
instance FromEdn Text where
  fromEdn e = case e of
    EStr t -> Right t
    _      -> Left "not a string"

instance ToEdn Char where
  toEdn c = EStr (T.singleton c)
  toEdnList = EStr . T.pack
instance FromEdn Char where
  fromEdn e = case e of
    EStr t | [c] <- T.unpack t -> Right c
    _                          -> Left "not a character"
  fromEdnList e = case e of
    EStr t -> Right (T.unpack t)
    _      -> Left "not a string"

instance ToEdn Keyword where toEdn (Keyword t) = EKw t
instance FromEdn Keyword where
  fromEdn e = case e of
    EKw t -> Right (Keyword t)
    _     -> Left "not a keyword"

instance ToEdn a => ToEdn [a] where toEdn = toEdnList
instance FromEdn a => FromEdn [a] where fromEdn = fromEdnList

-- | @nil@ is 'Nothing'.
instance ToEdn a => ToEdn (Maybe a) where toEdn = maybe ENil toEdn
instance FromEdn a => FromEdn (Maybe a) where
  fromEdn e = case e of
    ENil -> Right Nothing
    _    -> Just <$> fromEdn e

-- | A map with keyword keys.
instance ToEdn v => ToEdn (M.Map Keyword v) where
  toEdn m = EMap [(toEdn k, toEdn v) | (k, v) <- M.toList m]
instance FromEdn v => FromEdn (M.Map Keyword v) where
  fromEdn e = case e of
    EMap ps -> M.fromList <$> traverse (\(k, v) -> (,) <$> fromEdn k <*> fromEdn v) ps
    _       -> Left "not a map"

-- ---------------------------------------------------------------------------
-- Generic

-- | How the names of the Haskell type become the words of the EDN.
data Style = Style
  { styleField :: String -> Text   -- ^ a field name (@stTailBytes@) to its key (@tail-bytes@)
  , styleCtor  :: String -> Text   -- ^ a constructor name (@DarkBlue@) to its keyword (@dark-blue@)
  , styleSkip  :: String -> Bool   -- ^ field names that are not written (so nothing reads them back either)
  }

-- | A field name loses its lower-case prefix and is spelled with hyphens; a constructor name is
-- spelled with hyphens; every field is written.
defaultStyle :: Style
defaultStyle = Style
  { styleField = kebab . withoutPrefix
  , styleCtor  = kebab
  , styleSkip  = const False
  }

-- | @"wrFirstSeq"@ is @"FirstSeq"@: the lower-case letters before the first capital go (the
-- habit of giving the fields of a record a prefix).  A name with no such prefix stays as it is.
withoutPrefix :: String -> String
withoutPrefix s = case span isLower s of
  (_ : _, rest@(c : _)) | isUpper c -> rest
  _                                 -> s

-- | @"FirstSeq"@ is @"first-seq"@.
kebab :: String -> Text
kebab = T.pack . concat . zipWith one [0 :: Int ..]
  where one i c | isUpper c = [ '-' | i > 0 ] ++ [toLower c]
                | otherwise = [c]

genericToEdn :: (Generic a, GToEdn (Rep a)) => Style -> a -> Edn
genericToEdn st = gToEdn st . from

-- | Unknown keys of a map are not looked at (the shape was checked, closed, before); a key that is
-- missing reads as @nil@, which a 'Maybe' accepts.
genericFromEdn :: (Generic a, GFromEdn (Rep a)) => Style -> Edn -> Either Text a
genericFromEdn st e = case gTry st e of
  Just r  -> to <$> r
  Nothing -> Left $ case e of
    EKw t               -> "unknown keyword :" <> t
    EVec (EKw t : _)    -> "unknown tag :" <> t
    _                   -> "not a tagged vector"

type GToEdn :: (Type -> Type) -> Constraint
class GToEdn f where
  gToEdn :: Style -> f p -> Edn

instance GToEdn f => GToEdn (D1 d f) where
  gToEdn st (M1 x) = gToEdn st x

instance (GToEdn f, GToEdn g) => GToEdn (f :+: g) where
  gToEdn st (L1 x) = gToEdn st x
  gToEdn st (R1 x) = gToEdn st x

instance (Constructor c, GFields f) => GToEdn (C1 c f) where
  gToEdn st m@(M1 x)
    | conIsRecord m = EMap [(EKw (styleField st n), v) | (n, v) <- fs, not (styleSkip st n)]
    | otherwise     = case map snd fs of
        [] -> EKw tag
        vs -> EVec (EKw tag : vs)
    where
      fs  = gFields x
      tag = styleCtor st (conName m)

-- | The fields of a constructor: the name of each (empty when it has none) and its EDN.
type GFields :: (Type -> Type) -> Constraint
class GFields f where
  gFields :: f p -> [(String, Edn)]

instance GFields U1 where
  gFields _ = []

instance (GFields f, GFields g) => GFields (f :*: g) where
  gFields (a :*: b) = gFields a ++ gFields b

instance (Selector s, ToEdn a) => GFields (S1 s (K1 i a)) where
  gFields m@(M1 (K1 x)) = [(selName m, toEdn x)]

type GFromEdn :: (Type -> Type) -> Constraint
class GFromEdn f where
  -- | 'Nothing' when the EDN is not about one of these constructors (another tag); 'Just' when
  -- it is, whether it reads or not.
  gTry :: Style -> Edn -> Maybe (Either Text (f p))

instance GFromEdn f => GFromEdn (D1 d f) where
  gTry st e = fmap (fmap M1) (gTry st e)

instance (GFromEdn f, GFromEdn g) => GFromEdn (f :+: g) where
  gTry st e = case gTry st e of
    Just r  -> Just (L1 <$> r)
    Nothing -> fmap (fmap R1) (gTry st e)

instance (Constructor c, GFromFields f) => GFromEdn (C1 c f) where
  gTry st e
    | conIsRecord m = Just $ case e of
        EMap ps -> M1 . fst <$> gFromFields st (Keys ps)
        _       -> Left "not a map"
    | otherwise = case e of
        EKw t            | t == tag -> Just (args [])
        EVec (EKw t : xs) | t == tag -> Just (args xs)
        _                           -> Nothing
    where
      m   = undefined :: C1 c f ()
      tag = styleCtor st (conName m)
      args xs = do
        (v, rest) <- gFromFields st (Args xs)
        case rest of
          Args [] -> Right (M1 v)
          _       -> Left ("too many arguments to :" <> tag)

-- | Where the fields of a constructor are read from.
data Source = Keys [(Edn, Edn)] | Args [Edn]

type GFromFields :: (Type -> Type) -> Constraint
class GFromFields f where
  gFromFields :: Style -> Source -> Either Text (f p, Source)

instance GFromFields U1 where
  gFromFields _ src = Right (U1, src)

instance (GFromFields f, GFromFields g) => GFromFields (f :*: g) where
  gFromFields st src = do
    (a, src1) <- gFromFields st src
    (b, src2) <- gFromFields st src1
    Right (a :*: b, src2)

instance (Selector s, FromEdn a) => GFromFields (S1 s (K1 i a)) where
  gFromFields st src = case src of
    Args (x : xs) -> (\v -> (M1 (K1 v), Args xs)) <$> fromEdn x
    Args []       -> Left "too few arguments"
    Keys ps       ->
      let key = styleField st (selName (undefined :: S1 s (K1 i a) ()))
          x   = maybe ENil id (lookup (EKw key) ps)
      in case fromEdn x of
           Left why -> Left (":" <> key <> ": " <> why)
           Right v  -> Right (M1 (K1 v), src)
