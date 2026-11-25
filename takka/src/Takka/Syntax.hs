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
-- >         (ancestor ?e ?x)                  ; rule calls (recursive OK)
-- >  :order-by [?age :desc ?name]     ; optional: a variable of :find, then :asc (the default) or :desc
-- >  :limit 20                        ; optional: at least 1, or a scalar of :in (?n)
-- >  :offset 40]                      ; optional: 0 or more, or a scalar of :in
--
-- @:order-by@, @:limit@ and @:offset@ are Datalevin's, and are for a relation @:find@ (@:find ?a ?b@) only:
-- with @:find ?x .@ or @:find [?x ...]@ Datalevin takes no notice of them, and says nothing, so they are refused here.
-- In the map form @:order-by@ is a vector of pairs, @{:order-by [[?age :desc] [?name]]}@; Datalevin reads a flat
-- vector there as ascending whatever it says, so that is refused too.
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
  , Binding(..), InSpec(..), Input(..), Query(..), Dir(..), Amount(..), Rule(..), Cell(..)
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

data Term
  = TVar Text
  | TConst Value
  | TBlank
  | TSrc Text                     -- ^ a database, as an argument of a function: @$@
  | TOpts [(Text, Value)]         -- ^ a map of options, as the last argument of a function: @{:top 10}@
  deriving (Eq, Show)

-- | Where a function puts what it gives back (@[(f ?x) ?y]@, @[?a ?b]@, @[?x ...]@, @[[?a ?b]]@); @_@ keeps nothing.
data Binding
  = BScalar Text
  | BTuple  [Text]
  | BColl   Text
  | BRel    [Text]
  deriving (Eq, Show)

data Clause
  = CPattern Term Term Term
  | CPred    Text [Term]
  | CFn      Text [Term] Binding
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
  | InRulesEdn Text   -- ^ rules as EDN text (a vector of rules), checked by 'parseRulesText' and sent on as written
  deriving (Eq, Show)

-- | Which way an @:order-by@ variable sorts.
data Dir = Asc | Desc deriving (Eq, Show)

-- | What @:limit@ and @:offset@ take: a number written in the query, or a scalar variable of @:in@.
data Amount = AInt Int | AVar Text deriving (Eq, Show)

data Query = Query
  { qFind    :: [FindElem]
  , qShape   :: FindShape
  , qWith    :: [Text]
  , qIn      :: [InSpec]
  , qWhere   :: [Clause]
  , qOrderBy :: [(Text, Dir)]        -- ^ empty: no @:order-by@
  , qLimit   :: Maybe Amount
  , qOffset  :: Maybe Amount
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

-- | An argument of a function: a term, or a database (@$@), or a map of options.
argOf :: Edn -> Either String Term
argOf e = case e of
  ESym s | "$" `T.isPrefixOf` s -> Right (TSrc s)
  EMap ps -> TOpts <$> forM ps (\(k, v) -> case k of
               EKw n -> (,) n <$> ednToValue v
               _     -> Left ("an option map takes keywords as keys: " ++ T.unpack (renderEdn e)))
  _ -> termOf e

-- | Where the result of a function goes.
bindingOf :: Edn -> Either String Binding
bindingOf e = case e of
  ESym v | slot v                      -> Right (BScalar v)
  EVec [ESym v, ESym "..."] | slot v   -> Right (BColl v)
  EVec [EVec vs]                       -> BRel <$> mapM slotOf vs
  EVec vs                              -> BTuple <$> mapM slotOf vs
  _ -> Left ("cannot parse a binding: " ++ T.unpack (renderEdn e))
  where
    slot v = isVar v || v == "_"
    slotOf x = case x of
      ESym v | slot v -> Right v
      _               -> Left ("expected a variable or _ in a binding, got " ++ T.unpack (renderEdn x))

bindingVars :: Binding -> [Text]
bindingVars b = filter (/= "_") $ case b of
  BScalar v -> [v]
  BTuple vs -> vs
  BColl v   -> [v]
  BRel vs   -> vs

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
  EVec [EList (ESym f : args), out] -> CFn f <$> mapM argOf args <*> bindingOf out
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
    sections = ["find", "with", "in", "where", "order-by", "limit", "offset"] :: [Text]
    go _ acc [] = Right acc
    go _ acc (EKw k : rest)
      | k `elem` sections = go (Just k) (M.insertWith (\_ old -> old) k [] acc) rest
    go (Just k) acc (x : rest) = go (Just k) (M.insertWith (\new old -> old ++ new) k [x] acc) rest
    go Nothing _ (x : _) = Left ("expected :find, :with, :in, :where, :order-by, :limit or :offset before " ++ T.unpack (renderEdn x))

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
  (secs, ob, lim, off) <- case e of
    EVec items -> do
      secs <- splitSections items
      ob   <- case M.lookup "order-by" secs of
                Nothing        -> Right []
                Just [EVec xs] -> orderFlat xs
                Just _         -> Left ":order-by takes one vector: [?x :desc ?y]"
      lim  <- single "limit" secs
      off  <- single "offset" secs
      pure (M.delete "order-by" (M.delete "limit" (M.delete "offset" secs)), ob, lim, off)
    EMap ps -> do
      kvs  <- forM ps $ \(k, v) -> case k of
                EKw name -> Right (name, v)
                _        -> Left "map-form query expects keyword keys"
      secs <- fmap M.fromList . forM [ kv | kv@(n, _) <- kvs, n `notElem` ["order-by", "limit", "offset"] ] $ \(n, v) -> case v of
                EVec xs -> Right (n, xs)
                _       -> Left "map-form query expects keyword keys with vector values"
      ob   <- maybe (Right []) orderPairs (lookup "order-by" kvs)
      lim  <- maybe (Right Nothing) (fmap Just . amountOf "limit" 1) (lookup "limit" kvs)
      off  <- maybe (Right Nothing) (fmap Just . amountOf "offset" 0) (lookup "offset" kvs)
      pure (secs, ob, lim, off)
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
  when ((not (null ob) || lim /= Nothing || off /= Nothing) && shape /= Rel) $
    Left (":order-by, :limit and :offset need a relation :find (:find ?a ?b); with "
          ++ (if shape == Scalar then "`.`" else "`[?x ...]`") ++ " Datalevin ignores them without saying so.  "
          ++ "Ask for the rows (:find ?x ?y) and take the column you want")
  forM_ [ (n, a) | (n, Just a) <- [(":limit" :: String, lim), (":offset", off)] ] $ \(n, a) -> case a of
    AVar v | v `notElem` [ x | ISScalar x <- ins ] -> Left (n ++ " " ++ T.unpack v ++ ": a variable here has to be a scalar of :in")
    _                                             -> Right ()
  pure (Query fes shape withVars ins cls ob lim off)
  where
    single name secs = case M.lookup name secs of
      Nothing  -> Right Nothing
      Just [x] -> Just <$> amountOf name (if name == "limit" then 1 else 0) x
      Just _   -> Left (":" ++ T.unpack name ++ " takes one number or one variable")

-- | A direction: @:asc@ or @:desc@.
dirOf :: Text -> Either String Dir
dirOf d = case d of
  "asc"  -> Right Asc
  "desc" -> Right Desc
  _      -> Left ("an :order-by direction is :asc or :desc, not :" ++ T.unpack d)

-- | The vector form: @[?x :desc ?y ?z :asc]@, a variable and then, if it likes, a direction.
orderFlat :: [Edn] -> Either String [(Text, Dir)]
orderFlat xs = case xs of
  []                              -> Left ":order-by is empty"
  _                               -> go xs
  where
    go [] = Right []
    go (ESym v : EKw d : r) | isVar v = (\dir rest -> (v, dir) : rest) <$> dirOf d <*> go r
    go (ESym v : r)         | isVar v = ((v, Asc) :) <$> go r
    go (x : _) = Left ("cannot parse :order-by at " ++ T.unpack (renderEdn x) ++ ": expected a variable, then :asc or :desc")

-- | The map form: @[[?x :desc] [?y]]@.
orderPairs :: Edn -> Either String [(Text, Dir)]
orderPairs v = case v of
  EVec [] -> Left ":order-by is empty"
  EVec ps -> forM ps $ \p -> case p of
    EVec [ESym x]        | isVar x -> Right (x, Asc)
    EVec [ESym x, EKw d] | isVar x -> (,) x <$> dirOf d
    ESym _ -> Left "in a map-form query :order-by is a vector of pairs, [[?x :desc]]; a flat [?x :desc] sorts ascending whatever it says"
    _      -> Left ("cannot parse an :order-by pair: " ++ T.unpack (renderEdn p))
  _ -> Left ":order-by takes a vector of pairs: [[?x :desc] [?y]]"

-- | The number after @:limit@ (at least 1: Datalevin refuses 0, and reads a negative one as no limit at all) or @:offset@
-- (0 or more), or a variable.
amountOf :: Text -> Integer -> Edn -> Either String Amount
amountOf name lo x = case x of
  EInt n | n >= lo && n <= toInteger (maxBound :: Int) -> Right (AInt (fromInteger n))
         | otherwise -> Left (":" ++ T.unpack name ++ " must be " ++ show lo ++ " or more, got " ++ show n
                              ++ (if n < 0 then " (Datalevin reads a negative one as no limit at all)" else ""))
  ESym v | isVar v -> Right (AVar v)
  _ -> Left (":" ++ T.unpack name ++ " takes a whole number or a variable of :in, not " ++ T.unpack (renderEdn x))

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
  CFn _ ts o     -> concatMap termVars ts ++ bindingVars o
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
