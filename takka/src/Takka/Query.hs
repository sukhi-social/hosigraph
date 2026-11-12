{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Module      : Takka.Query
-- Description : Datalog queries over a 'Db' snapshot.
--
-- Supported (Datomic/Datalevin flavoured) syntax:
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
-- Not yet: @or@/@or-join@, @not-join@, multiple sources, pull in @:find@.
--
-- Evaluation is index-nested-loop join.  The planner picks the next clause
-- greedily from /counts read off the counted B+tree/ (see 'countDatoms') and,
-- for joins on a bound value, a rank-sampled distinct-value estimate
-- (see 'distinctEstimate').  Filters run as soon as their variables are bound.
-- Rules are evaluated bottom-up before the main query: rules are grouped into
-- strongly connected components, evaluated dependencies-first (stratified), and
-- each recursive component runs a semi-naive fixpoint (each round joins only the
-- tuples that are new since the previous round).  Negation through recursion is
-- rejected.  There is no magic-set rewriting yet, so a rule call with a bound
-- argument still computes the whole relation.
module Takka.Query
  ( -- * AST
    Term(..), Clause(..), Agg(..), FindElem(..), FindShape(..)
  , InSpec(..), Input(..), Query(..), Rule(..), Cell(..)
    -- * Parsing
  , parseQuery, parseRules, parseRulesText
    -- * Running
  , runQuery, runQueryCells, qEdn, qCellsEdn, explainEdn
  , renderClause
  ) where

import           Control.Exception (evaluate, throwIO)
import           Control.Monad
import           Data.Graph (SCC(..), stronglyConnComp)
import           Data.IORef
import           Data.Int (Int64)
import           Data.List (elemIndex, minimumBy, nub)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import           Data.Ord (comparing)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T

import           Takka.Edn
import           Takka.Pull
import           Takka.Store
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

-- ---------------------------------------------------------------------------
-- Variables

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

-- | Variables bound after the clause has run.
clauseOut :: Clause -> [Text]
clauseOut c = case c of
  CPattern a b d -> concatMap termVars [a, b, d]
  CFn _ _ o      -> [o]
  CRule _ ts     -> concatMap termVars ts
  COr js _       -> js
  CPred _ _      -> []
  CNot _         -> []
  CNotJoin _ _   -> []

ruleRefs :: [Clause] -> [Text]
ruleRefs = concatMap go
  where go (CRule n _)     = [n]
        go (CNot cs)       = ruleRefs cs
        go (CNotJoin _ cs) = ruleRefs cs
        go (COr _ bs)      = concatMap ruleRefs bs
        go _               = []

mapVars :: (Text -> Text) -> Clause -> Clause
mapVars f c = case c of
  CPattern a b d -> CPattern (mt a) (mt b) (mt d)
  CPred p ts     -> CPred p (map mt ts)
  CFn p ts o     -> CFn p (map mt ts) (f o)
  CNot cs        -> CNot (map (mapVars f) cs)
  CNotJoin js cs -> CNotJoin (map f js) (map (mapVars f) cs)
  COr js bs      -> COr (map f js) (map (map (mapVars f)) bs)
  CRule n ts     -> CRule n (map mt ts)
  where mt t = case t of TVar v -> TVar (f v); _ -> t

-- | Rewrite top-level @or@ clauses of a rule body into one rule per branch, so that the
-- semi-naive evaluation sees every recursive call as a plain literal.  Variables that are
-- local to a branch are renamed so that branches cannot capture each other's names.
expandOrs :: Rule -> [Rule]
expandOrs r = [ r { ruleBody = b } | b <- expandBody "" (ruleBody r) ]

expandBody :: String -> [Clause] -> [[Clause]]
expandBody pre = go (0 :: Int)
  where
    go _ [] = [[]]
    go k (c : cs) = case c of
      COr js brs ->
        [ br' ++ rest
        | (bi, br) <- zip [0 :: Int ..] brs
        , let tag    = pre ++ "_o" ++ show k ++ "_" ++ show bi
              locals = S.fromList (concatMap clauseVarsDeep br) `S.difference` S.fromList js
              rename v = if v `S.member` locals then v <> T.pack tag else v
        , br' <- expandBody tag (map (mapVars rename) br)
        , rest <- go (k + 1) cs ]
      _ -> [ c : rest | rest <- go (k + 1) cs ]

-- ---------------------------------------------------------------------------
-- Planning items

data Item = Item
  { itIx     :: !Int
  , itClause :: !Clause
  , itNeeds  :: !(S.Set Text)    -- ^ variables that must be bound before it can run
  }

mkItems :: [Text] -> [Clause] -> [Item]
mkItems outer cs = [ Item i c (needs c (others i)) | (i, c) <- zip [0 ..] cs ]
  where
    others i = S.fromList (outer ++ concat [ clauseVars c' | (j, c') <- zip [0 :: Int ..] cs, j /= i ])
    needs c oth = case c of
      CPred _ ts -> S.fromList (concatMap termVars ts)
      CFn _ ts _ -> S.fromList (concatMap termVars ts)
      CNot inner -> S.fromList (filter (`S.member` oth) (concatMap clauseVars inner))
      CNotJoin js _ -> S.fromList js
      COr js _   -> S.fromList js
      _          -> S.empty

isFilterLike :: Clause -> Bool
isFilterLike c = case c of CPred{} -> True; CFn{} -> True; CNot{} -> True; CNotJoin{} -> True; _ -> False

-- | An @or@ whose shared variables are all bound behaves like a filter; otherwise it generates.
isOr :: Clause -> Bool
isOr c = case c of COr{} -> True; _ -> False

-- ---------------------------------------------------------------------------
-- Execution context

type Env = M.Map Text Value
type Rel = S.Set [Value]

data Ctx = Ctx
  { cxDb     :: Db
  , cxRels   :: M.Map Text Rel
  , cxRelIdx :: M.Map Text (M.Map Value [[Value]])   -- ^ relation indexed on its first column
  , cxTrace  :: Maybe (IORef [Text])
  }

indexRel :: Rel -> M.Map Value [[Value]]
indexRel r = M.fromListWith (++) [ (x, [row]) | row@(x : _) <- S.toList r ]   -- (++): O(1) per row; order is irrelevant

mkCtx :: Db -> M.Map Text Rel -> Maybe (IORef [Text]) -> Ctx
mkCtx db rels tr = Ctx db rels (M.map indexRel rels) tr

qerr :: String -> IO a
qerr = throwIO . QueryError

logTrace :: Ctx -> Text -> IO ()
logTrace ctx msg = forM_ (cxTrace ctx) $ \r -> modifyIORef' r (msg :)

-- ---------------------------------------------------------------------------
-- Range hints: comparison predicates narrow the index range a pattern scans

data Cmp = CmpLt | CmpLe | CmpGt | CmpGe | CmpEq deriving (Eq, Show)

-- | \"@var \<cmp\> other@\", read off a predicate such as @[(> ?a 97)]@ or @[(< 2 ?a 5)]@.
data Hint = Hint { hVar :: Text, hCmp :: Cmp, hOther :: Term }

flipCmp :: Cmp -> Cmp
flipCmp c = case c of CmpLt -> CmpGt; CmpLe -> CmpGe; CmpGt -> CmpLt; CmpGe -> CmpLe; CmpEq -> CmpEq

-- | Hints from the top-level predicates of a clause list (never from inside @not@).
rangeHints :: [Clause] -> [Hint]
rangeHints cls = concat [ concat (zipWith (pairHints cmp) args (drop 1 args))
                        | CPred op args <- cls, Just cmp <- [cmpOf op] ]
  where
    cmpOf op = lookup op [("<", CmpLt), ("<=", CmpLe), (">", CmpGt), (">=", CmpGe), ("=", CmpEq), ("==", CmpEq)]
    pairHints cmp a b =
      [ Hint x cmp b            | TVar x <- [a], other b x ] ++
      [ Hint x (flipCmp cmp) a  | TVar x <- [b], other a x ]
    other t x = case t of TConst _ -> True; TVar y -> y /= x; TBlank -> False

-- | Lower / upper bound implied by the hints whose other side is known in this environment.
-- 'Nothing' if the bounds cannot be compared with each other.
hintBounds :: [(Cmp, Value)] -> Maybe (Maybe Bound, Maybe Bound)
hintBounds rs = do
  lo <- tightest GT [ Bound v i | (c, v) <- rs, Just i <- [lowerIncl c] ]
  hi <- tightest LT [ Bound v i | (c, v) <- rs, Just i <- [upperIncl c] ]
  pure (lo, hi)
  where
    lowerIncl c = case c of CmpGt -> Just False; CmpGe -> Just True; CmpEq -> Just True; _ -> Nothing
    upperIncl c = case c of CmpLt -> Just False; CmpLe -> Just True; CmpEq -> Just True; _ -> Nothing
    -- want = GT keeps the larger value (lower bounds), LT the smaller (upper bounds)
    tightest _ [] = Just Nothing
    tightest want (x : xs) = Just <$> foldM (pick want) x xs
    pick want a b = case compareValues (boundValue a) (boundValue b) of
      Nothing -> Nothing
      Just o | o == EQ   -> Just (if boundInclusive a then b else a)    -- exclusive is tighter
             | o == want -> Just a
             | otherwise -> Just b

-- | If this pattern binds a fresh value variable of a known attribute, and predicates
-- bound that variable, the index range to scan instead of the whole attribute.
rangeFor :: [Hint] -> Env -> Term -> Term -> Term -> Maybe (Keyword, Maybe Bound, Maybe Bound)
rangeFor hints env te ta tv = case tv of
  TVar x | x `M.notMember` env, eFree x, Just (VKw k) <- resolveT ta -> do
    let known = [ (hCmp h, v) | h <- hints, hVar h == x, Just v <- [resolveT (hOther h)] ]
    when (null known) Nothing
    (lo, hi) <- hintBounds known
    when (isNothing lo && isNothing hi) Nothing
    Just (k, lo, hi)
  _ -> Nothing
  where
    eFree x = case te of
      TBlank -> True
      TVar y -> y /= x && y `M.notMember` env
      TConst _ -> False
    resolveT t = case t of TConst v -> Just v; TVar y -> M.lookup y env; TBlank -> Nothing

-- ---------------------------------------------------------------------------
-- Cost estimation (the interesting part: counts come from the counted B+tree)

clauseCost :: Ctx -> S.Set Text -> [Hint] -> Env -> Clause -> IO Double
clauseCost ctx bound hints env c = case c of
  CPattern te ta tv -> patternCost ctx bound hints env te ta tv
  CRule n _         -> pure (maybe 1e9 (fromIntegral . S.size) (M.lookup n (cxRels ctx)))
  COr _ _           -> pure 1e6          -- a generator of last resort: let cheap patterns bind variables first
  _                 -> pure 0

patternCost :: Ctx -> S.Set Text -> [Hint] -> Env -> Term -> Term -> Term -> IO Double
patternCost ctx bound hints env te ta tv = do
  ranged <- case rangeFor hints env te ta tv of
    Just (k, lo, hi) -> fmap fromIntegral <$> countInRange (cxDb ctx) k lo hi
    Nothing          -> pure Nothing
  maybe plain pure ranged
  where
    plain = do
      let db = cxDb ctx
          constE = case te of TConst (VInt n) -> Just n; _ -> Nothing
          constA = case ta of TConst (VKw k) -> Just k; _ -> Nothing
          constV = case tv of TConst v -> Just v; _ -> Nothing
          isB t  = case t of TVar x -> x `S.member` bound; _ -> False
      base <- fromIntegral <$> countDatoms db constE constA constV
      let card = do a <- constA
                    sch <- M.lookup a (dbSchema db)
                    pure (asCard sch)
      if base == (0 :: Double) then pure 0
      else if isB te then
        pure $ case (constA, card, isB tv || constV /= Nothing) of
          (Just _, _, True)         -> min base 1
          (Just _, Just Many, _)    -> min base 3
          (Just _, _, _)            -> min base 1
          _                         -> min base 8
      else case constA of
        Just a | isB tv, constE == Nothing, constV == Nothing -> do
          d <- distinctEstimate db a
          pure (max 1 (base / d))
        _ -> pure base

-- ---------------------------------------------------------------------------
-- Solving

solveItems :: Ctx -> S.Set Text -> [Env] -> [Item] -> IO [Env]
solveItems _ _ [] _ = pure []
solveItems _ _ envs [] = pure envs
solveItems ctx bound envs items =
  case [ it | it <- items, isFilterLike (itClause it) || isOr (itClause it), itNeeds it `S.isSubsetOf` bound ] of
    (it : _) -> step it Nothing
    [] -> do
      let gens = [ it | it <- items, not (isFilterLike (itClause it)) ]
      when (null gens) $
        qerr ("unbound variable(s) in: " ++ T.unpack (T.intercalate ", "
              [ renderClause (itClause it) | it <- items ]))
      costs <- forM gens $ \it -> (, it) <$> clauseCost ctx bound hints env0 (itClause it)
      let (cost, best) = minimumBy (comparing fst) costs
      step best (Just cost)
  where
    hints = rangeHints (map itClause items)
    env0  = fromMaybe M.empty (listToMaybe envs)

    step :: Item -> Maybe Double -> IO [Env]
    step it mcost = do
      envs' <- evalClause ctx bound hints envs (itClause it)
      logTrace ctx $ T.concat
        [ "  ", renderClause (itClause it)
        , maybe "" (\c -> "   est=" <> fmt c) mcost
        , case itClause it of
            CPattern te ta tv | isJust (rangeFor hints env0 te ta tv) -> "   [range scan]"
            _ -> ""
        , "   rows ", T.pack (show (length envs)), " -> ", T.pack (show (length envs')) ]
      solveItems ctx (bound `S.union` S.fromList (clauseOut (itClause it))) envs'
                 [ x | x <- items, itIx x /= itIx it ]
    fmt d = T.pack (show (fromIntegral (round (d * 10) :: Integer) / 10 :: Double))

evalClause :: Ctx -> S.Set Text -> [Hint] -> [Env] -> Clause -> IO [Env]
evalClause ctx bound hints envs cl = case cl of
  CPattern te ta tv -> concat <$> mapM (evalPattern ctx hints te ta tv) envs
  CPred f args -> filterM (\env -> do
      vals <- resolveArgs env args
      either (qerr . (\m -> f' ++ ": " ++ m)) pure (predicate f vals)) envs
    where f' = T.unpack f
  CFn f args out -> fmap concat . forM envs $ \env -> do
      vals <- resolveArgs env args
      v <- either (qerr . (\m -> T.unpack f ++ ": " ++ m)) pure (function f vals)
      pure $ case M.lookup out env of
        Just old | old /= v  -> []
                 | otherwise -> [env]
        Nothing -> [M.insert out v env]
  COr js brs -> concat <$> mapM (evalOr ctx js brs) envs
  CNotJoin js inner -> filterM (\env -> do
      let envJ = M.filterWithKey (\k _ -> k `elem` js) env
      null <$> solveItems ctx (M.keysSet envJ) [envJ] (mkItems js inner)) envs
  CNot inner -> filterM (\env ->
      null <$> solveItems ctx bound [env] (mkItems (S.toList bound) inner)) envs
  CRule n args -> evalRuleCall ctx n args envs

-- | Evaluate every branch with only the shared variables visible, then merge the new bindings
-- back into the environment (as a set: two branches giving the same binding count once).
evalOr :: Ctx -> [Text] -> [[Clause]] -> Env -> IO [Env]
evalOr ctx js brs env = do
  let envJ  = M.filterWithKey (\k _ -> k `elem` js) env
      fresh = [ v | v <- js, v `M.notMember` env ]
  exts <- fmap concat . forM brs $ \br -> do
    res <- solveItems ctx (M.keysSet envJ) [envJ] (mkItems js br)
    forM res $ \r -> forM fresh $ \v ->
      maybe (qerr ("variable " ++ T.unpack v ++ " is not bound by every branch of an or"))
            (pure . (,) v) (M.lookup v r)
  pure [ M.union (M.fromList ext) env | ext <- S.toList (S.fromList exts) ]

resolveArgs :: Env -> [Term] -> IO [Value]
resolveArgs env = mapM $ \t -> case t of
  TConst v -> pure v
  TVar x   -> maybe (qerr ("unbound variable " ++ T.unpack x)) pure (M.lookup x env)
  TBlank   -> qerr "`_` is not allowed as a function argument"

evalPattern :: Ctx -> [Hint] -> Term -> Term -> Term -> Env -> IO [Env]
evalPattern ctx hints te ta tv env = do
  let resolve t = case t of
        TConst v -> Just v
        TVar x   -> M.lookup x env
        TBlank   -> Nothing
      eRes = case resolve te of Nothing -> Right Nothing; Just (VInt n) -> Right (Just n); Just _ -> Left ()
      aRes = case resolve ta of Nothing -> Right Nothing; Just (VKw k)  -> Right (Just k); Just _ -> Left ()
  case (eRes, aRes) of
    (Right me, Right ma) -> do
      let plain = datoms (cxDb ctx) me ma (resolve tv)
      ds <- case rangeFor hints env te ta tv of
              Just (k, lo, hi) -> datomsInRange (cxDb ctx) k lo hi >>= maybe plain pure
              Nothing          -> plain
      pure (mapMaybe (\(Datom e a v) ->
              bindT te (VInt e) env >>= bindT ta (VKw a) >>= bindT tv v) ds)
    _ -> pure []
  where
    -- Terms already fixed before the scan were enforced by the scan itself
    -- (possibly after type coercion), so only fresh variables are bound here.
    bindT t val acc = case t of
      TVar x | M.member x env -> Just acc
             | otherwise -> case M.lookup x acc of
                 Just v' -> if v' == val then Just acc else Nothing
                 Nothing -> Just (M.insert x val acc)
      _ -> Just acc

evalRuleCall :: Ctx -> Text -> [Term] -> [Env] -> IO [Env]
evalRuleCall ctx n args envs = do
  rel <- maybe (qerr ("unknown rule: " ++ T.unpack n)) pure (M.lookup n (cxRels ctx))
  let idx = M.findWithDefault M.empty n (cxRelIdx ctx)
  fmap concat . forM envs $ \env -> do
    let first = case args of
          (TConst v : _) -> Just v
          (TVar x : _)   -> M.lookup x env
          _              -> Nothing
        candidates = case first of
          Just v  -> M.findWithDefault [] v idx
          Nothing -> S.toList rel
    pure (mapMaybe (unify env) candidates)
  where
    unify env row
      | length row /= length args = Nothing
      | otherwise = foldM bind env (zip args row)
    bind acc (t, val) = case t of
      TBlank   -> Just acc
      TConst c -> if c == val then Just acc else Nothing
      TVar x   -> case M.lookup x acc of
                    Just v' -> if v' == val then Just acc else Nothing
                    Nothing -> Just (M.insert x val acc)

-- ---------------------------------------------------------------------------
-- Predicates and functions

compareValues :: Value -> Value -> Maybe Ordering
compareValues a b = case (a, b) of
  (VInt x, VInt y)       -> Just (compare x y)
  (VDouble x, VDouble y) -> Just (compare x y)
  (VInt x, VDouble y)    -> Just (compare (fromIntegral x) y)
  (VDouble x, VInt y)    -> Just (compare x (fromIntegral y))
  (VStr x, VStr y)       -> Just (compare x y)
  (VKw x, VKw y)         -> Just (compare x y)
  (VBool x, VBool y)     -> Just (compare x y)
  _                      -> Nothing

valEq :: Value -> Value -> Bool
valEq a b = maybe (a == b) (== EQ) (compareValues a b)

predicate :: Text -> [Value] -> Either String Bool
predicate name vals = case (name, vals) of
  ("=",    _ : _ : _) -> Right (chain valEq)
  ("==",   _ : _ : _) -> Right (chain valEq)
  ("not=", [a, b])    -> Right (not (valEq a b))
  ("!=",   [a, b])    -> Right (not (valEq a b))
  ("<",    _ : _ : _) -> Right (chain (cmp (== LT)))
  ("<=",   _ : _ : _) -> Right (chain (cmp (/= GT)))
  (">",    _ : _ : _) -> Right (chain (cmp (== GT)))
  (">=",   _ : _ : _) -> Right (chain (cmp (/= LT)))
  ("even?", [VInt n]) -> Right (even n)
  ("odd?",  [VInt n]) -> Right (odd n)
  ("pos?",  [v])      -> Right (cmp (== GT) v (VInt 0))
  ("neg?",  [v])      -> Right (cmp (== LT) v (VInt 0))
  ("zero?", [v])      -> Right (cmp (== EQ) v (VInt 0))
  (n, [VStr s, VStr t])
    | n `elem` ["includes?", "clojure.string/includes?"]    -> Right (t `T.isInfixOf` s)
    | n `elem` ["starts-with?", "clojure.string/starts-with?"] -> Right (t `T.isPrefixOf` s)
    | n `elem` ["ends-with?", "clojure.string/ends-with?"]  -> Right (t `T.isSuffixOf` s)
  _ -> Left ("unsupported predicate or argument types: " ++ T.unpack name)
  where
    chain f = and (zipWith f vals (drop 1 vals))
    cmp f a b = maybe False f (compareValues a b)

numeric :: (Int64 -> Int64 -> Int64) -> (Double -> Double -> Double) -> Value -> Value -> Either String Value
numeric fi fd a b = case (a, b) of
  (VInt x, VInt y)       -> Right (VInt (fi x y))
  (VDouble x, VDouble y) -> Right (VDouble (fd x y))
  (VInt x, VDouble y)    -> Right (VDouble (fd (fromIntegral x) y))
  (VDouble x, VInt y)    -> Right (VDouble (fd x (fromIntegral y)))
  _ -> Left "expected numbers"

function :: Text -> [Value] -> Either String Value
function name vals = case (name, vals) of
  ("+", _ : _)     -> foldM (numeric (+) (+)) (VInt 0) vals
  ("*", _ : _)     -> foldM (numeric (*) (*)) (VInt 1) vals
  ("-", [a])       -> numeric (-) (-) (VInt 0) a
  ("-", a : rest@(_ : _)) -> foldM (numeric (-) (-)) a rest
  ("/", [a, b])    -> case (a, b) of
      (_, VInt 0)  -> Left "division by zero"
      (VInt x, VInt y) | x `mod` y == 0 -> Right (VInt (x `div` y))
      _            -> numeric (\_ _ -> 0) (/) (toD a) (toD b)
  ("quot", [VInt a, VInt b]) | b /= 0 -> Right (VInt (a `quot` b))
  ("mod",  [VInt a, VInt b]) | b /= 0 -> Right (VInt (a `mod` b))
  ("rem",  [VInt a, VInt b]) | b /= 0 -> Right (VInt (a `rem` b))
  ("inc", [a])     -> numeric (+) (+) a (VInt 1)
  ("dec", [a])     -> numeric (-) (-) a (VInt 1)
  ("abs", [VInt a])    -> Right (VInt (abs a))
  ("abs", [VDouble a]) -> Right (VDouble (abs a))
  ("max", _ : _)   -> foldM (pick GT) (head vals) (tail vals)
  ("min", _ : _)   -> foldM (pick LT) (head vals) (tail vals)
  ("str", _)       -> Right (VStr (T.concat (map str vals)))
  ("ground", [a])  -> Right a
  ("clojure.string/upper-case", [VStr s]) -> Right (VStr (T.toUpper s))
  ("clojure.string/lower-case", [VStr s]) -> Right (VStr (T.toLower s))
  _ -> Left ("unsupported function or argument types: " ++ T.unpack name)
  where
    toD v = case v of VInt x -> VDouble (fromIntegral x); _ -> v
    str v = case v of VStr s -> s; VKw (Keyword k) -> k; _ -> renderValue v
    pick want a b = case compareValues a b of
      Just o  -> Right (if o == want then a else b)
      Nothing -> Left "values are not comparable"

aggregate :: Agg -> [Value] -> Either String Value
aggregate agg vs = case agg of
  ACount         -> Right (VInt (fromIntegral (length vs)))
  ACountDistinct -> Right (VInt (fromIntegral (S.size (S.fromList vs))))
  ASum           -> foldM (numeric (+) (+)) (VInt 0) vs
  AAvg           -> do s <- foldM (numeric (+) (+)) (VInt 0) vs
                       pure (VDouble (toDouble s / fromIntegral (length vs)))
  AMin           -> extreme LT
  AMax           -> extreme GT
  where
    toDouble v = case v of VInt x -> fromIntegral x; VDouble d -> d; _ -> 0
    extreme want = case vs of
      []       -> Left "aggregate over no values"
      (x : xs) -> foldM (\a b -> case compareValues a b of
                            Just o  -> Right (if o == want then a else b)
                            Nothing -> Left "values are not comparable") x xs

-- ---------------------------------------------------------------------------
-- Rules: stratified, semi-naive bottom-up evaluation

deltaName :: Text -> Text
deltaName n = "delta$" <> n

-- | Rule names that appear under a @not@ (at any depth).
negRefs :: [Clause] -> [Text]
negRefs cs = concatMap go cs
  where go c = case c of
          CNot i       -> ruleRefs i ++ negRefs i
          CNotJoin _ i -> ruleRefs i ++ negRefs i
          COr _ bs     -> concatMap negRefs bs
          _            -> []

computeRules :: Db -> Maybe (IORef [Text]) -> M.Map Text [Rule] -> S.Set Text -> IO (M.Map Text Rel)
computeRules db tr rm names = foldM stratum M.empty sccs
  where
    depsOf n = [ m | m <- nub (concatMap (ruleRefs . ruleBody) (M.findWithDefault [] n rm))
                   , m `S.member` names ]
    -- dependencies first
    sccs = stronglyConnComp [ (n, n, depsOf n) | n <- S.toList names ]
    stratum done scc = do
      let members = case scc of AcyclicSCC n -> [n]; CyclicSCC ns -> ns
      forM_ members $ \n -> forM_ (M.findWithDefault [] n rm) $ \r ->
        forM_ (negRefs (ruleBody r)) $ \m -> when (m `elem` members) $
          qerr ("rule " ++ T.unpack n ++ " uses negation through recursion on " ++ T.unpack m
                ++ " (the program is not stratified)")
      rels <- evalStratum db tr rm done members
      pure (M.union rels done)

evalStratum :: Db -> Maybe (IORef [Text]) -> M.Map Text [Rule] -> M.Map Text Rel -> [Text]
            -> IO (M.Map Text Rel)
evalStratum db tr rm done members = do
  let empty0 = M.fromList [ (n, S.empty) | n <- members ]
  rels0 <- M.fromList <$> forM members (\n -> (,) n <$> runRules (ctxFor empty0 M.empty M.empty) (rulesOf n))
  (final, rounds) <-
    if recursive then loop 1 rels0 (M.map indexRel rels0) rels0 else pure (rels0, 1 :: Int)
  forM_ tr $ \r -> modifyIORef' r
    (T.concat [ "  rules {", T.intercalate "," members, "}: "
              , if recursive then "semi-naive, " else "non-recursive, "
              , T.pack (show rounds), " round(s), "
              , T.pack (show (sum (map S.size (M.elems final)))), " tuples" ] :)
  pure final
  where
    memberSet = S.fromList members
    rulesOf n = M.findWithDefault [] n rm
    doneIdx   = M.map indexRel done
    isRec c   = case c of CRule m _ -> m `S.member` memberSet; _ -> False
    rename c  = case c of CRule m as -> CRule (deltaName m) as; _ -> c
    -- one variant per recursive literal: that literal reads the delta, the others the full relation
    variants r = [ r { ruleBody = [ if k == j then rename c else c | (k, c) <- zip [0 :: Int ..] (ruleBody r) ] }
                 | (j, c) <- zip [0 :: Int ..] (ruleBody r), isRec c ]
    recursive = any (not . null . variants) (concatMap rulesOf members)

    -- relations visible to rule bodies: this stratum (full + deltas) and finished strata
    ctxFor rels idx deltas =
      let dIdx = M.map indexRel deltas
      in Ctx db (M.unions [rels, M.mapKeys deltaName deltas, done])
                (M.unions [idx, M.mapKeys deltaName dIdx, doneIdx]) Nothing

    runRules ctx rs = fmap (S.fromList . concat) . forM rs $ \r -> do
      envs <- solveItems ctx S.empty [M.empty] (mkItems [] (ruleBody r))
      forM envs $ \env -> forM (ruleHead r) $ \v ->
        maybe (qerr ("rule " ++ T.unpack (ruleName r) ++ ": head variable " ++ T.unpack v
                     ++ " is not bound by the body")) pure (M.lookup v env)

    loop i rels idx delta
      | all S.null (M.elems delta) = pure (rels, i)
      | otherwise = do
          let ctx = ctxFor rels idx delta
          news <- forM members $ \n -> do
            ts <- runRules ctx (concatMap variants (rulesOf n))
            pure (n, S.difference ts (M.findWithDefault S.empty n rels))
          let delta' = M.fromList news
              rels'  = M.unionWith S.union rels delta'
              idx'   = M.unionWith (M.unionWith (flip (++))) idx (M.map indexRel delta')   -- new rows in front: O(|delta|)
          loop (i + 1) rels' idx' delta'

reachableRules :: M.Map Text [Rule] -> [Text] -> Either String (S.Set Text)
reachableRules rm = go S.empty
  where
    go seen [] = Right seen
    go seen (n : rest)
      | n `S.member` seen = go seen rest
      | otherwise = case M.lookup n rm of
          Nothing -> Left ("unknown rule: " ++ T.unpack n ++ " (pass rules with `%` in :in)")
          Just rs -> go (S.insert n seen) (concatMap (ruleRefs . ruleBody) rs ++ rest)

-- ---------------------------------------------------------------------------
-- Running

bindInputs :: [InSpec] -> [Input] -> Either String ([Rule], [Env], [Text])
bindInputs specs0 inputs0 = go specs0 inputs0 ([], [M.empty], [])
  where
    go [] [] acc = Right acc
    go [] _  _   = Left "more inputs than :in bindings"
    go (ISDb : ss) is acc = go ss is acc
    go (ISRules : ss) (InRules rs : is) (rl, es, vs) = go ss is (rl ++ rs, es, vs)
    go (ISScalar v : ss) (InScalar x : is) (rl, es, vs) = go ss is (rl, map (M.insert v x) es, v : vs)
    go (ISColl v : ss) (InColl xs : is) (rl, es, vs) =
      go ss is (rl, [ M.insert v x e | e <- es, x <- xs ], v : vs)
    go (ISRel cols : ss) (InRel rows : is) (rl, es, vs)
      | any ((/= length cols) . length) rows = Left "relation input has rows of the wrong width"
      | otherwise = go ss is (rl, [ M.union (M.fromList (zip cols row)) e | e <- es, row <- rows ], cols ++ vs)
    go (s : _) _ _ = Left ("input does not match :in binding " ++ show s)

-- | Run a parsed query.  Rows are de-duplicated and sorted (Datalog set semantics).
runQuery :: Db -> Query -> [Input] -> IO [[Value]]
runQuery db q inputs = do
  when (any isPull (qFind q)) $
    qerr "this query uses (pull …) in :find; use runQueryCells / qCells to get the pulled trees"
  cells <- runWith Nothing db q inputs
  forM cells $ mapM $ \c -> case c of
    CVal v  -> pure v
    CPull _ -> qerr "unexpected pull result"
  where isPull fe = case fe of FPull{} -> True; _ -> False

-- | Like 'runQuery', but @(pull ?e [pattern])@ is allowed in @:find@.
runQueryCells :: Db -> Query -> [Input] -> IO [[Cell]]
runQueryCells = runWith Nothing

-- What a find element contributes before pull trees are fetched.
data Raw = RVal Value | RPull Pattern Value

runWith :: Maybe (IORef [Text]) -> Db -> Query -> [Input] -> IO [[Cell]]
runWith tr db q inputs = do
  (rules, envs0, inVars) <- either (throwIO . QueryError) pure (bindInputs (qIn q) inputs)
  let rm = M.map reverse (M.fromListWith (++) [ (ruleName r', [r']) | r <- rules, r' <- expandOrs r ])
  needed <- either (throwIO . QueryError) pure (reachableRules rm (ruleRefs (qWhere q)))
  rels   <- computeRules db tr rm needed
  let ctx    = mkCtx db rels tr
      keyOf fe = case fe of FVar v -> [v]; FPull v _ -> [v]; FAgg{} -> []
      keyVs  = nub (concatMap keyOf (qFind q))          -- grouping variables, in :find order
      aggVs  = [ v | FAgg _ v <- qFind q ]
      cols   = nub (keyVs ++ aggVs ++ qWith q)
      outer  = cols ++ inVars
  -- static check (independent of the data, so an empty database reports it too)
  let bindable = S.fromList (inVars ++ concatMap clauseOut (qWhere q))
  forM_ cols $ \v -> unless (v `S.member` bindable) $
    qerr ("variable " ++ T.unpack v ++ " in :find/:with is not bound by :in or :where")
  envs <- solveItems ctx (S.fromList inVars) envs0 (mkItems outer (qWhere q))
  let rows = S.toList (S.fromList [ [ env M.! v | v <- cols ] | env <- envs ])
      ixOf v = maybe 0 id (elemIndex v cols)
      hasAgg = not (null aggVs)
  raw <- if not hasAgg
    then pure [ [ case fe of
                    FVar v    -> RVal (row !! ixOf v)
                    FPull v p -> RPull p (row !! ixOf v)
                    FAgg{}    -> RVal (VBool False)         -- unreachable: no aggregates here
                | fe <- qFind q ] | row <- rows ]
    else do
      let groups = M.fromListWith (++) [ (map (\v -> row !! ixOf v) keyVs, [row]) | row <- rows ]   -- aggregates ignore order
          keyed key v = key !! maybe 0 id (elemIndex v keyVs)
      forM (M.toList groups) $ \(key, grp) ->
        forM (qFind q) $ \fe -> case fe of
          FVar v    -> pure (RVal (keyed key v))
          FPull v p -> pure (RPull p (keyed key v))
          FAgg a v  -> RVal <$> either (throwIO . QueryError) pure (aggregate a (map (\r -> r !! ixOf v) grp))
  let raw' = if qShape q == Scalar then take 1 raw else raw
  -- Materialize everything now (values and pulled trees), so that the work and any error
  -- happen inside this call instead of surfacing wherever the caller first looks at the rows.
  cells <- forM raw' $ \r -> forM r $ \x -> case x of
    RVal v -> CVal <$> evaluate v
    RPull pat (VInt e) -> CPull <$> (pull db pat e >>= evaluate)
    RPull _ v -> qerr ("pull needs an entity id, got " ++ T.unpack (renderValue v))
  pure cells

-- | Parse an EDN query string and run it.
qEdn :: Db -> Text -> [Input] -> IO [[Value]]
qEdn db txt inputs = do
  q <- parseText txt
  runQuery db q inputs

-- | Parse and run, allowing @(pull …)@ in @:find@.
qCellsEdn :: Db -> Text -> [Input] -> IO [[Cell]]
qCellsEdn db txt inputs = do
  q <- parseText txt
  runQueryCells db q inputs

parseText :: Text -> IO Query
parseText txt = either (throwIO . ParseError) pure (readEdn txt >>= parseQuery)

-- | Run a query and also return the plan that was chosen: each step shows the
-- clause, the planner's estimate (from counted-tree range counts) and the
-- number of intermediate rows before and after.
explainEdn :: Db -> Text -> [Input] -> IO ([[Value]], [Text])
explainEdn db txt inputs = do
  q <- parseText txt
  ref <- newIORef []
  cells <- runWith (Just ref) db q inputs
  rows <- forM cells $ mapM $ \c -> case c of
    CVal v  -> pure v
    CPull _ -> qerr "explainEdn does not support pull in :find"
  steps <- reverse <$> readIORef ref
  pure (rows, steps)

-- ---------------------------------------------------------------------------

renderTerm :: Term -> Text
renderTerm t = case t of TVar v -> v; TConst v -> renderValue v; TBlank -> "_"

renderClause :: Clause -> Text
renderClause c = case c of
  CPattern a b d -> "[" <> T.unwords (map renderTerm [a, b, d]) <> "]"
  CPred f ts     -> "[(" <> T.unwords (f : map renderTerm ts) <> ")]"
  CFn f ts o     -> "[(" <> T.unwords (f : map renderTerm ts) <> ") " <> o <> "]"
  CNot cs        -> "(not " <> T.unwords (map renderClause cs) <> ")"
  CNotJoin js cs -> "(not-join [" <> T.unwords js <> "] " <> T.unwords (map renderClause cs) <> ")"
  COr js bs      -> "(or-join [" <> T.unwords js <> "] " <> T.unwords (map branch bs) <> ")"
  CRule n ts     -> "(" <> T.unwords (n : map renderTerm ts) <> ")"
  where branch b = case b of [x] -> renderClause x; _ -> "(and " <> T.unwords (map renderClause b) <> ")"
