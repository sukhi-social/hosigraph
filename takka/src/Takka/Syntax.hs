-- SPDX-License-Identifier: BlueOak-1.0.0
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Module      : Takka.Syntax
-- Description : What a Datalog query and a pull pattern look like, and how to read them from EDN.
--
-- No database here: the shapes of queries, rules, inputs, result cells and pull patterns, and the
-- parsers from EDN.  hosigraph sends them to Datalevin and reads the answers back as 'Cell's.
--
-- Supported (Datomic/Datalevin flavoured) query syntax:
--
-- > [:find ?name (count ?f)           ; variables and aggregates
-- >  :with ?f                         ; optional
-- >  :in $ ?min [?tag ...] %          ; db, scalars, collections, relations, rules
-- >  :where [?e :person/name ?name]   ; patterns (2 or 3 components)
-- >         [?e :age ?a] [(> ?a ?min)]        ; predicates
-- >         [(+ ?a 1) ?next]                  ; function binding
-- >         (not [?e :banned true])           ; negation
-- >         (ancestor ?e ?x)]                 ; rule calls (recursive OK)
--
-- and pull patterns:
--
-- > [:person/name                         ; an attribute
-- >  {:person/friend [:person/name]}      ; follow a reference, with its own pattern
-- >  {:person/friend 2}                   ; ... recursively, to a depth
-- >  {:person/friend ...}                 ; ... recursively, without limit (cycles are cut)
-- >  :person/_friend                      ; reverse reference: who points at me
-- >  (:person/age :default 0)             ; default when missing
-- >  (:person/name :as "n")               ; rename in the result
-- >  (limit :person/alias 2)              ; at most two values
-- >  :db/id                               ; the entity id
-- >  *]                                   ; every attribute (explicit items override it)
module Takka.Syntax
  ( -- * Datalog queries
    Term(..), Clause(..), Agg(..), FindElem(..), FindShape(..)
  , InSpec(..), Input(..), Query(..), Rule(..), Cell(..)
  , parseQuery, parseRules, parseRulesText
  , termVars, clauseVars, clauseVarsDeep
    -- * Pull patterns
  , Pulled(..), PullItem(..), PullAttr(..), Recur(..), Pattern
  , parsePull, parsePullText, plain
  ) where

import           Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T

import           Takka.Edn
import           Takka.Types

-- ---------------------------------------------------------------------------
-- AST

data Term = TVar Text | TConst Value | TBlank deriving (Eq, Show)

data Clause
  = CPattern Term Term Term
  | CPred    Text [Term]
  | CFn      Text [Term] Text
  | CNot     [Clause]
  | CNotJoin [Text] [Clause]        -- ^ @not-join@: only the listed variables are shared with the outside
  | COr      [Text] [[Clause]]      -- ^ @or@ / @or-join@: the shared variables and the branches
  | CRule    Text [Term]
  deriving (Eq, Show)

data Agg = ACount | ACountDistinct | ASum | AMin | AMax | AAvg deriving (Eq, Show)

data FindElem
  = FVar Text
  | FAgg Agg Text
  | FPull Text Pattern        -- ^ @(pull ?e [pattern])@
  deriving (Eq, Show)

-- | One cell of a result row: a plain value, or a pulled tree.
data Cell = CVal Value | CPull Pulled deriving (Eq, Show)

data FindShape = Rel | Scalar | Coll deriving (Eq, Show)

data InSpec = ISDb | ISScalar Text | ISColl Text | ISRel [Text] | ISRules deriving (Eq, Show)

-- | Values supplied for the @:in@ clause, positionally (the @$@ db is implicit).
data Input
  = InScalar Value
  | InColl   [Value]
  | InRel    [[Value]]
  | InRules  [Rule]
  deriving (Eq, Show)

data Query = Query
  { qFind  :: [FindElem]
  , qShape :: FindShape
  , qWith  :: [Text]
  , qIn    :: [InSpec]
  , qWhere :: [Clause]
  } deriving (Eq, Show)

data Rule = Rule
  { ruleName :: Text
  , ruleHead :: [Text]
  , ruleBody :: [Clause]
  } deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Parsing

isVar :: Text -> Bool
isVar s = "?" `T.isPrefixOf` s && T.length s > 1

termOf :: Edn -> Either String Term
termOf e = case e of
  ESym "_" -> Right TBlank
  ESym s | isVar s   -> Right (TVar s)
         | otherwise -> Left ("unexpected symbol in clause: " ++ T.unpack s)
  _ -> TConst <$> ednToValue e

parseClause :: Edn -> Either String Clause
parseClause e = case e of
  EList (ESym "not" : cs)       -> CNot <$> mapM parseClause cs
  EList (ESym "or" : branches)  -> do
    when (null branches) (Left "`or` needs at least one branch")
    bs <- mapM branch branches
    let sets = [ S.fromList (concatMap clauseVarsDeep b) | b <- bs ]
    when (any (/= head sets) sets) $
      Left "all branches of `or` must use the same variables (use `or-join` to hide local ones)"
    Right (COr (S.toList (head sets)) bs)
  EList (ESym "or-join" : EVec vs : branches) -> do
    when (null branches) (Left "`or-join` needs at least one branch")
    js <- mapM joinVar vs
    COr js <$> mapM branch branches
  EList (ESym "not-join" : EVec vs : cs) -> CNotJoin <$> mapM joinVar vs <*> mapM parseClause cs
  EList (ESym n : args)         -> CRule n <$> mapM termOf args
  EVec [EList (ESym f : args)]  -> CPred f <$> mapM termOf args
  EVec [EList (ESym f : args), ESym out]
    | isVar out                 -> (\as -> CFn f as out) <$> mapM termOf args
  EVec (ESym s : rest)
    | "$" `T.isPrefixOf` s      -> pattern rest
  EVec items                    -> pattern items
  _ -> Left ("cannot parse clause: " ++ T.unpack (renderEdn e))
  where
    branch b = case b of
      EList (ESym "and" : cs) -> mapM parseClause cs
      _                       -> (: []) <$> parseClause b
    joinVar v = case v of
      ESym x | isVar x -> Right x
      _                -> Left ("expected a variable in the join list, got " ++ T.unpack (renderEdn v))
    pattern items = do
      ts <- mapM termOf items
      case ts of
        [a, b, c] -> Right (CPattern a b c)
        [a, b]    -> Right (CPattern a b TBlank)
        [a]       -> Right (CPattern a TBlank TBlank)
        _         -> Left ("a pattern needs 1 to 3 components: " ++ T.unpack (renderEdn e))

splitSections :: [Edn] -> Either String (M.Map Text [Edn])
splitSections = go Nothing M.empty
  where
    sections = ["find", "with", "in", "where"] :: [Text]
    go _ acc [] = Right acc
    go _ acc (EKw k : rest)
      | k `elem` sections = go (Just k) (M.insertWith (\_ old -> old) k [] acc) rest
    go (Just k) acc (x : rest) = go (Just k) (M.insertWith (\new old -> old ++ new) k [x] acc) rest
    go Nothing _ (x : _) = Left ("expected :find, :with, :in or :where before " ++ T.unpack (renderEdn x))

parseFindElem :: Edn -> Either String FindElem
parseFindElem e = case e of
  ESym v | isVar v -> Right (FVar v)
  EList [ESym "pull", ESym v, pat] | isVar v -> FPull v <$> parsePull pat
  EList [ESym f, ESym v] | isVar v ->
    case f of
      "count"          -> Right (FAgg ACount v)
      "count-distinct" -> Right (FAgg ACountDistinct v)
      "sum"            -> Right (FAgg ASum v)
      "min"            -> Right (FAgg AMin v)
      "max"            -> Right (FAgg AMax v)
      "avg"            -> Right (FAgg AAvg v)
      _                -> Left ("unknown aggregate: " ++ T.unpack f)
  _ -> Left ("cannot parse :find element: " ++ T.unpack (renderEdn e))

parseFind :: [Edn] -> Either String ([FindElem], FindShape)
parseFind items0 = do
  let items = case items0 of [EVec xs] -> xs; _ -> items0
  case reverse items of
    (ESym "..." : r) -> (, Coll)   <$> mapM parseFindElem (reverse r)
    (ESym "."   : r) -> (, Scalar) <$> mapM parseFindElem (reverse r)
    _                -> (, Rel)    <$> mapM parseFindElem items

parseIn :: [Edn] -> Either String [InSpec]
parseIn = mapM one
  where
    one e = case e of
      ESym "$"           -> Right ISDb
      ESym "%"           -> Right ISRules
      ESym v | isVar v   -> Right (ISScalar v)
      EVec [ESym v, ESym "..."] | isVar v -> Right (ISColl v)
      EVec [EVec vs] -> ISRel <$> mapM var vs
      _ -> Left ("cannot parse :in binding: " ++ T.unpack (renderEdn e))
    var (ESym v) | isVar v = Right v
    var x = Left ("expected a variable, got " ++ T.unpack (renderEdn x))

parseQuery :: Edn -> Either String Query
parseQuery e = do
  secs <- case e of
    EVec items -> splitSections items
    EMap ps    -> fmap M.fromList . forM ps $ \(k, v) -> case (k, v) of
                    (EKw name, EVec xs) -> Right (name, xs)
                    _ -> Left "map-form query expects keyword keys with vector values"
    _ -> Left "a query must be a vector or a map"
  findItems <- maybe (Left "query has no :find") Right (M.lookup "find" secs)
  (fes, shape) <- parseFind findItems
  when (null fes) (Left ":find is empty")
  withVars <- forM (M.findWithDefault [] "with" secs) $ \x -> case x of
                ESym v | isVar v -> Right v
                _ -> Left ":with expects variables"
  ins <- case M.lookup "in" secs of
           Nothing -> Right [ISDb]
           Just is -> parseIn is
  cls <- mapM parseClause (M.findWithDefault [] "where" secs)
  pure (Query fes shape withVars ins cls)

parseRules :: Edn -> Either String [Rule]
parseRules e = case e of
  EVec rs -> mapM parseRule rs
  _       -> Left "rules must be a vector of rule vectors"
  where
    parseRule r = case r of
      EVec (EList (ESym n : hd) : body) -> do
        hv  <- forM hd $ \h -> case h of
                 ESym v | isVar v -> Right v
                 _ -> Left "rule head arguments must be variables"
        cls <- mapM parseClause body
        Right (Rule n hv cls)
      _ -> Left ("cannot parse rule: " ++ T.unpack (renderEdn r))

parseRulesText :: Text -> Either String [Rule]
parseRulesText t = readEdn t >>= parseRules

termVars :: Term -> [Text]
termVars (TVar v) = [v]
termVars _        = []

-- | Variables a clause shares with its surroundings (for @or-join@ / @not-join@ only the
-- listed ones; their local variables stay hidden).
clauseVars :: Clause -> [Text]
clauseVars c = case c of
  CPattern a b d -> concatMap termVars [a, b, d]
  CPred _ ts     -> concatMap termVars ts
  CFn _ ts o     -> concatMap termVars ts ++ [o]
  CNot cs        -> concatMap clauseVars cs
  CNotJoin js _  -> js
  COr js _       -> js
  CRule _ ts     -> concatMap termVars ts

-- | Every variable mentioned anywhere inside, including local ones.
clauseVarsDeep :: Clause -> [Text]
clauseVarsDeep c = case c of
  CNot cs        -> concatMap clauseVarsDeep cs
  CNotJoin js cs -> js ++ concatMap clauseVarsDeep cs
  COr js bs      -> js ++ concatMap (concatMap clauseVarsDeep) bs
  _              -> clauseVars c

-- ---------------------------------------------------------------------------
-- Pull patterns

-- | The result: an ordered tree.
data Pulled
  = PScalar Value
  | PMany   [Pulled]
  | PEntity [(Keyword, Pulled)]
  deriving (Eq, Show)

data Recur = RNone | RDepth Int | RUnlimited deriving (Eq, Show)

data PullItem = PWild | PAttr PullAttr deriving (Eq, Show)

data PullAttr = PullAttr
  { paName    :: Keyword          -- ^ as written (the key in the result, unless renamed)
  , paKey     :: Keyword          -- ^ the attribute it reads
  , paReverse :: Bool
  , paAs      :: Maybe Text
  , paDefault :: Maybe Value
  , paLimit   :: Maybe Int
  , paSub     :: Maybe Pattern
  , paRecur   :: Recur
  } deriving (Eq, Show)

type Pattern = [PullItem]

parsePullText :: Text -> Either String Pattern
parsePullText t = readEdn t >>= parsePull

parsePull :: Edn -> Either String Pattern
parsePull e = case e of
  EVec items -> concat <$> mapM item items
  _          -> Left "a pull pattern must be a vector"

plain :: Keyword -> PullAttr
plain k = PullAttr k k False Nothing Nothing Nothing Nothing RNone

-- | @:ns/_name@ reads the reverse of @:ns/name@.
attrOf :: Text -> PullAttr
attrOf txt =
  let (ns, name) = T.breakOnEnd "/" txt
  in if "_" `T.isPrefixOf` name && T.length name > 1
       then (plain (Keyword txt)) { paKey = Keyword (ns <> T.drop 1 name), paReverse = True }
       else plain (Keyword txt)

item :: Edn -> Either String [PullItem]
item e = case e of
  ESym "*"  -> Right [PWild]
  EKw k     -> Right [PAttr (attrOf k)]
  EList xs  -> (\a -> [PAttr a]) <$> expr xs
  EMap ps   -> forM ps $ \(k, v) -> fmap PAttr $ do
    base <- case k of
      EKw n    -> Right (attrOf n)
      EList xs -> expr xs
      _        -> Left ("pull: a map key must be an attribute, got " ++ T.unpack (renderEdn k))
    case v of
      EVec _     -> (\sub -> base { paSub = Just sub }) <$> parsePull v
      EInt n | n >= 0 -> Right base { paRecur = if n == 0 then RNone else RDepth (fromInteger n) }
      ESym "..." -> Right base { paRecur = RUnlimited }
      _          -> Left ("pull: expected a nested pattern, a depth or ... after " ++ T.unpack (renderEdn k))
  _ -> Left ("cannot parse pull item: " ++ T.unpack (renderEdn e))

-- | @(limit :attr n)@ or @(:attr :as "x" :default v :limit n)@.
expr :: [Edn] -> Either String PullAttr
expr xs = case xs of
  [ESym "limit", EKw k, EInt n] -> Right (attrOf k) { paLimit = Just (fromInteger n) }
  [ESym "limit", EKw k, ENil]   -> Right (attrOf k)
  (EKw k : opts)                -> options (attrOf k) opts
  _ -> Left ("cannot parse pull expression: " ++ T.unpack (renderEdn (EList xs)))
  where
    options it [] = Right it
    options it (EKw "as" : v : r) = case v of
      EStr s -> options it { paAs = Just s } r
      EKw s  -> options it { paAs = Just s } r
      ESym s -> options it { paAs = Just s } r
      _      -> Left ":as expects a name"
    options it (EKw "default" : v : r) = do
      d <- ednToValue v
      options it { paDefault = Just d } r
    options it (EKw "limit" : EInt n : r) = options it { paLimit = Just (fromInteger n) } r
    options _ (o : _) = Left ("unknown pull option: " ++ T.unpack (renderEdn o))
