{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

module CG_SAT where

import Rule hiding ( Not, Negate )
import qualified Rule as R
import CghsUtils ( isLex, normalisePosition, normaliseTagsetRel )
import SAT ( Solver(..) )
import SAT.Named


import Data.Foldable ( fold )
import Data.IntMap ( IntMap, (!), assocs, elems )
import qualified Data.IntMap as IM
import Data.IntSet ( IntSet )
import qualified Data.IntSet as IS 
import Data.List ( findIndices, intercalate, nub )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( catMaybes, fromMaybe, isNothing )

import Control.Monad ( foldM, liftM2, when )
import Control.Monad.Except ( MonadError, ExceptT, runExceptT
                            , throwError, catchError )
import Control.Monad.RWS ( RWST, MonadState, MonadReader, MonadWriter
                         , runRWST, asks, get, gets, put, tell )
import Control.Monad.IO.Class ( MonadIO, liftIO )

import Control.Exception ( Exception ) 


--------------------------------------------------------------------------------

newtype RWSE a = RWSE { runRWSE :: ExceptT CGException (RWST Env Log Config IO) a }
  deriving ( Functor, Applicative, Monad, MonadIO, MonadWriter Log
           , MonadState Config, MonadReader Env, MonadError CGException )

--runExceptT :: ExceptT e m a -> m (Either e a)       -- RWST Env Log Config IO (Either CGException a)

--runRWST :: RWST r w s m a -> r -> s -> m (a, s, w)  -- IO ( )

rwse :: Env -> Config 
     -> RWSE a
     -> IO ( Either CGException a, Config, Log )
rwse env conf (RWSE m) = runRWST (runExceptT m) env conf


{-  tagMap 
           ‾‾`v

         vblex |-> [321,         322,         323      ...]
                     |            |            |
                    vblex sg p1  vblex sg p2  vblex sg ...  
                ___,^
           rdMap 
-}
data Env = Env { tagMap :: Map Tag IntSet 
               , rdMap :: IntMap Reading 
               , lems :: OrList Tag
               , wfs :: OrList Tag
               , solver :: Solver }

withNewSolver :: Solver -> Env -> Env
withNewSolver s env = env { solver = s }

data Config = Config { sentence :: Sentence 
                     , senlength :: Int } deriving (Show)

type Log = [String]

data CGException = TagsetNotFound String | OutOfScope Int String | NoReadingsLeft 
                 | UnknownError String deriving ( Show )

instance Exception CGException


data Sentence = S { readings :: IntMap Cohort
                  , lemmas :: IntMap (Map Tag Lit)
                  , wordforms :: IntMap (Map Tag Lit)  -- One per cohort may be true
                  } deriving (Show,Eq)
type Cohort = IntMap Lit

emptyConfig = Config (S IM.empty IM.empty IM.empty) 0


--------------------------------------------------------------------------------

type Lex = (OrList Tag, OrList Tag)  -- first attempt: Lex is outside Match, because C shouldn't affect.. how about NOT?

type SeqList a = [a] -- List of things that follow each other in sequence

data Pattern = Pat (OrList ( OrList (SeqList Int) -- OrList length >1 if the ctx is template
                           , SeqList Match
                           , SeqList Lex ) )
             | PatAlways 
             | Negate Pattern -- Negate goes beyond set negation and C
             deriving (Show,Eq,Ord)

data Match = Mix IntSet -- ^ Cohort contains tags in IntSet, and 
                        -- for condition, it may contain tags not in IntSet.
                        -- OBS. for target, it *must* contain tags not in IntSet.
           | Cau IntSet -- ^ Cohort contains only tags in IntSet.
           | Not IntSet -- ^ Cohort contains no tags in IntSet.
           | NotCau IntSet -- ^ Cohort contains (not only) tags in IntSet:
                            -- either of Not and Mix are valid.
           | Bar (Int,Match) Match -- ^ Cohort matches the second Match, and requires that 
                                   -- there is no match to the first Match, up to the Int.
           | AllTags -- Cohort may contain any tag
           deriving (Show,Eq,Ord)

nullMatch :: Match -> Bool
nullMatch m = case m of
  AllTags -> False
  Mix is -> IS.null is
  Cau is -> IS.null is
  Not is -> IS.null is
  NotCau is -> IS.null is
  Bar _ m -> nullMatch m

--------------------------------------------------------------------------------
-- Interaction with SAT-solver, non-pure functions


-- | Using the readings and the solver in environment, create a sentence.
-- (should I also put it in the config???)
mkSentence :: Int -> RWSE Sentence
mkSentence width = do
  s <- asks solver
  rds  <- asks rdMap
  lemmas <- asks lems
  wforms <- asks wfs
  rdSent <- liftIO $ IM.fromList `fmap` sequence
    [ (,) n `fmap` sequence (IM.mapWithKey (mkLit s n) rds)
        | n <- [1..width] ] 

  lmSent <- liftIO $ IM.fromList `fmap` sequence
    [ do innerLits <- mapM (mkLitLex s n) lemmas'
         let innerTagMap = M.fromList (zip lemmas' innerLits)
         return (n,innerTagMap)

      | n <- [1..width] 
      , let lemmas' = getOrList lemmas ] 
--  let lmSent = IM.fromList lmList

  --wfSent <- liftIO $ IM.fromList `fmap` sequence
  --  [ do innerLits <- map (mkLitLex s n) wforms
  --       let innerTagMap = M.fromList (zip wforms innerLits)
  --       return (n,innerTagMap)
  --      | n <- [1..width] ] 

  return $ S rdSent lmSent IM.empty --wfSent
 where
  mkLitLex :: Solver -> Int -> Tag -> IO Lit
  mkLitLex s n t = newLit s (showReading (And [t]) n n)

  mkLit :: Solver -> Int -> Int -> Reading -> IO Lit
  mkLit s n m rd = newLit s (showReading rd n m)

  showReading :: Reading -> Int -> Int -> String
  showReading (And ts) wdi rdi = "w" ++ show wdi ++ concatMap (\t -> '<':show t++">") ts


solveAndPrint :: Bool -> Solver -> [Lit] -> Sentence -> IO ()
solveAndPrint verbose s ass (S sent _ _) = do
  b <- solve s ass
  if b then do
          when verbose $ print ass
          vals <- sequence 
                   [ sequence [ modelValue s lit | lit <- IM.elems word ] 
                      | (sind,word) <- IM.assocs sent ]
          let trueAnas =
               [ "\"w" ++ show sind ++ "\"\n"
                  ++ unlines [ "\t"++show ana | (ana, True) <- zip (IM.elems word) vs ]
                 | ((sind,word), vs) <- zip (IM.assocs sent) vals ]
          mapM_ putStrLn trueAnas
          putStrLn "----"
      else do
        putStrLn $ "solveAndPrint: Conflict with assumptions " ++ show ass
--------------------------------------------------------------------------------
-- 

-- | Apply the rule to all applicable cohorts in the sentence
apply :: Rule -> RWSE ()
apply rule = do 
  s <- asks solver
  w <- gets senlength

  ----------- The seitan of the function ------------
  let applyToCohort sen i = do

      --Trigger throwErrors NoReadingsLeft if the rule tries to remove all;
      -- as per VISL CG-3 behaviour, remove nothing.
       (someTrgIsTrue, someOtherIsTrue, allCondsHold, trgCoh, trgRds) 
         <- trigger rule i `catchError` \e -> case e of 
              NoReadingsLeft -> return (true,true,true,IM.empty,IM.empty)
              OutOfScope _ _ -> return (true,true,true,IM.empty,IM.empty)
              TagsetNotFound s -> do liftIO $ putStrLn ("Warning: tagset " ++ s ++ " not found")
                                     return (true,true,true,IM.empty,IM.empty)
              _              -> throwError e

       if IM.null trgCoh -- if one of the expected errors was caught,

        then return sen -- just return the sentence unchanged

        else do onlyTrgLeft <- liftIO $ andl' s [ someTrgIsTrue, neg someOtherIsTrue ]
                cannotApply <- liftIO $ orl' s [ neg allCondsHold, onlyTrgLeft ]
 
                newTrgLits <- liftIO $ sequence               
                      [ (,) j `fmap`        --wN<a>' is true if both of the following:
                         andl s newTrgName [ oldTrgLit     --wN<a> was also true, and
                                           , cannotApply ] --rule cannot apply 
                         | (j,oldTrgLit) <- assocs trgRds
                         , let newTrgName = show oldTrgLit ++ "'" ]

                let newcoh = foldl changeReading trgCoh newTrgLits
                let newsen = changeCohort sen i newcoh
                return newsen

  ----------------------------------------------------
   
  sen <- gets sentence
  newSen <- foldM applyToCohort sen [1..w]
  put (Config newSen w)

 where
  changeReading :: Cohort -> (Int,Lit) -> Cohort
  changeReading coh (i,newrd) = IM.adjust (const newrd) i coh

  changeCohort :: Sentence -> Int -> Cohort -> Sentence
  changeCohort (S sen x y) i newcoh = S (IM.adjust (const newcoh) i sen) x y


trigger :: Rule -> Int -> RWSE (Lit,Lit,Lit,Cohort,IntMap Lit)
trigger rule origin = do
  (Config sen len) <- get
  tm <- asks tagMap
  s <- asks solver
  conds <- condLits rule origin
  trgCoh <- case IM.lookup origin (readings sen) of
              Nothing -> do tell [ "trigger: target position " ++ show origin ++ 
                                   " out of scope, sentence length " ++ show len ]
                            throwError (OutOfScope origin "trigger")
              Just ch -> return ch
  trgIS <- case normaliseTagsetAbs (target rule) tm of
             Left AllTags -> do tell [ "trigger: rule " ++ show rule ++ 
                                       " tries to remove or select all readings" ]
                                throwError NoReadingsLeft
             Right intset -> return intset
  let (trg,oth) = partitionTarget (oper rule) trgIS trgCoh

  mht <- liftIO $ orl' s (IM.elems trg) -- 1) Cohort has ≥1 target lits
  mho <- liftIO $ orl' s (IM.elems oth) -- 2) Cohort has ≥1 other lits                              
  ach <- liftIO $ andl' s (getAndList conds) -- 3) All conditions hold
  return (mht, mho, ach, trgCoh, trg)


--------------------------------------------------------------------------------
-- Create new literals from contextual tests of a rule

condLits :: Rule -- ^ Rule, whose conditions to turn into literals
         -> Int  -- ^ Absolute position of the target in the sentence
         -> RWSE (AndList Lit) -- If all condition(s) are in scope, return literal for each condition.
                 -- Otherwise, ctx2Pattern or pattern2Lit returns OutOfScope.
condLits rule origin = do
  slen <- gets senlength
  pats <- ctx2Pattern slen origin `mapM` context rule -- :: AndList (Maybe Pattern)
  mapM pattern2Lit pats :: RWSE (AndList Lit)


--------------------------------------------------------------------------------
-- Transform the contextual test(s) of one rule into a Pattern.

ctx2Pattern :: Int  -- ^ Sentence length
            -> Int  -- ^ Absolute position of the target in the sentence
            -> Context -- ^ Context to be transformed
            -> RWSE Pattern
ctx2Pattern senlen origin ctx = case ctx of
  Always -> return PatAlways

  c@(Ctx posn polr tgst)
    -> do (ps,m,l) <- singleCtx2Pat c
          return $ Pat (Or [(ps,m,l)])
          
  Link ctxs 
    -> do (ps,ms,ls) <- unzip3 `fmap` mapM singleCtx2Pat (getAndList ctxs)
          return $ Pat (Or [(fold ps, fold ms, fold ls)])
    -- This wouldn't support linked templates, 
    -- but I don't think any sane CG engine supports them either.

  Template ctxs
    -> do pats <- ctx2Pattern senlen origin `mapM` ctxs --  :: OrList Pattern
          return undefined --(fold pats)

  R.Negate ctx 
    -> Negate `fmap` ctx2Pattern senlen origin ctx

 where 
  singleCtx2Pat (Ctx posn polr tgst) = 
    do let allPositions = normalisePosition posn senlen origin :: OrList Int -- 1* -> e.g. [2,3,4]
       if null (getOrList allPositions) 
        then do --tell ["ctx2Pattern: position " ++ show posn ++ " not in scope"]
                throwError $ OutOfScope origin "ctx2Pattern" -- Pattern fails because condition(s) are not in scope. 
                -- This is to be expected, because we try to apply every rule to every cohort.
        else do 
          tagset <- normaliseTagsetAbs tgst `fmap` asks tagMap --normaliseTagsetAbs ignores lexical tags
          let lxt@(lemmas,wforms) = lemsAndWFs tgst :: (OrList Tag,OrList Tag)
          mop <- getMatch origin posn polr
          let match = either id mop tagset
          if nullMatch match && null (getOrList lemmas) && null (getOrList wforms)
            then do tell ["singleCtx2Pat: tagset " ++ show tgst ++ show lxt ++ " not found, rule cannot apply"]
                    throwError $ TagsetNotFound (show tgst) -- Pattern fails because tagset is not found in any readings, ie. it won't match anything.
                                             -- This is unexpected, and indicates a bug in the grammar, TODO alert user!!!!
            else return (fmap (:[]) allPositions, [match], [lxt] ) 

getMatch :: Int -> Position -> Polarity -> RWSE (IntSet -> Match)
getMatch origin pos pol = case (pos,pol) of
  (Pos (Barrier ts) c n, _) -> do
    tsMatch <- getMatch origin (Pos AtLeast c n) pol
    barTags <- normaliseTagsetAbs ts `fmap` asks tagMap
    let barMatch = either id Mix barTags
    let barInd = origin + n 
    return $ Bar (barInd,barMatch) . tsMatch

  (Pos (CBarrier ts) c n, _) -> do 
    tsMatch <- getMatch origin (Pos AtLeast c n) pol
    barTags <- normaliseTagsetAbs ts `fmap` asks tagMap
    let barMatch = either id Cau barTags
    let barInd = origin + n 
    return $ Bar (barInd,barMatch) . tsMatch

  (Pos _ NC _, Yes)   -> return Mix
  (Pos _ NC _, R.Not) -> return Not
  (Pos _ C _,  Yes)   -> return Cau
  (Pos _ C _,  R.Not) -> return NotCau

--------------------------------------------------------------------------------
-- Transform the pattern into a literal. Fails if any other step before has failed.

pattern2Lit :: Pattern 
            -> RWSE Lit
pattern2Lit pat = do 
  s <- asks solver
  sen <- gets sentence
  case pat of
    PatAlways -> return true
    Negate p  -> neg `fmap` pattern2Lit p
    Pat pats  -> do let ms_is_ls = concat [ nub $ concat
                                  [ zip3 mats ls inds | inds <- getOrList indss ]
                                   | (indss,mats,ls) <- getOrList pats ]

                    lits <- mapM (\(x,y,z) -> match2CondLit x y z) ms_is_ls
                    liftIO $ orl' s lits


match2CondLit :: Match -> Lex -> Int -> RWSE Lit
match2CondLit (Bar (bi,bm) mat) lextags ind = do
  s <- asks solver
  sen <- gets sentence
  matchLit <- match2CondLit mat lextags ind

  let barInds | bi <= ind = [bi..ind]
              | otherwise = [ind..bi]


  let negBM = case bm of
                Mix is -> Not is
                Cau is -> NotCau is
                _      -> error "match2CondLit: invalid condition for BARRIER"

  -- cohorts between the condition and target do NOT contain the specified readings
  -- TODO: add lexical tags here too
  barLits <- mapM (match2CondLit negBM (Or[],Or[]) {-TODO-} ) barInds

  liftIO $ andl' s (matchLit:barLits)

match2CondLit mat (Or lemmas, Or wforms) ind = do 
  s <- asks solver
  (Config sen len) <- get


  (coh,lemMap,wfMap) <- case lookupSent ind sen of 
           Nothing -> do tell [ "match2CondLit: position " ++ show ind ++ 
                                " out of scope, sentence length " ++ show len ]
                         throwError (OutOfScope ind "match2CondLit")                 
           Just (c,l,w) -> return (c,l,w)
  someLem <- liftIO $ maybe (return true) (orl' s) 
                            (sequence $ fmap (`M.lookup` lemMap) lemmas)
  someWF <- liftIO $ maybe (return true) (orl' s) 
                           (sequence $ fmap (`M.lookup` wfMap) wforms)
  liftIO $ case mat of
    -- if Mix is for condition, then it doesn't matter whether some tag not
    -- in the tagset is True. Only for *targets* there must be something else.
    Mix is -> do let (inmap,_) = partitionCohort is coh
                 someReading <- orl' s (elems inmap)
                 andl' s [someReading, someLem, someWF]

              -- Any difference whether to compute neg $ orl' or andl $ map neg?                 
    Cau is -> do let (inmap,outmap) = partitionCohort is coh
                 someReading <- orl' s (elems inmap)
                 noOtherReading <- andl' s (neg `fmap` elems outmap)
                 andl' s [someReading, noOtherReading, someLem, someWF]

    Not is -> do let (inmap,outmap) = partitionCohort is coh
                 noReading <- andl' s (neg `fmap` elems inmap)
                 someOtherReading <- orl' s (elems outmap) 
                 andl' s [ noReading, someOtherReading, someLem, someWF ]

  -- `NOT 1C foo' means: "either there's no foo, or the foo is not unique."
  -- Either way, having at least one true non-foo lit fulfils the condition,
  -- because the cohort may not be empty.
    NotCau is -> do let (_,outmap) = partitionCohort is coh
                    orl' s (elems outmap) 

    AllTags   -> return true

    _ -> error "match2CondLit: :("

--------------------------------------------------------------------------------
--TODO move somewhere else/merge into something?

defaultRules :: Solver -> Sentence -> IO ()
defaultRules s sentence = 
   sequence_ [ do addClause s lits          --Every word must have >=1 reading
                --  constraints s mp [] form  --Constraints based on lexicon --TODO AmbiguityClasses.hs
               | coh <- IM.elems (readings sentence)
               , let lits = IM.elems coh 
               , let mp i = fromMaybe (error $ "constraints: " ++ show i) (IM.lookup i coh) ] 

--------------------------------------------------------------------------------
-- From here on, only pure helper functions

mkEnv :: Solver -> [Reading] -> [Tag] -> [Tag] -> Env
mkEnv s rds ls ws = Env (mkTagMap rds) (mkRdMap rds) (Or ls) (Or ws) s 

{- rdMap --  1 |-> vblex sg p3
             2 |-> noun sg mf
               ...
          9023 |-> "aurrera" adv -}
mkRdMap :: [Reading] -> IntMap Reading
mkRdMap = IM.fromDistinctAscList . zip [1..]


{- tagMap    --  vblex |-> IS(1,30,31,32,..,490)
                 sg    |-> IS(1,2,3,120,1800)
                 mf    |-> IS(2,20,210)
                 adv   |-> IS() -}
mkTagMap :: [Reading] -> Map Tag IntSet 
mkTagMap rds = M.fromList $
  ts `for` \t -> let getInds = IS.fromList . map (1+) . findIndices (elem t)
                 in (t, getInds rdLists) 
 where 
  for = flip fmap 
  rdLists = map getAndList rds
  ts = nub $ concat rdLists
  -- maybe write this more readably later? --getInds :: Tag -> [Reading] -> IntSet

lookupSent :: Int -> Sentence -> Maybe (IntMap Lit, Map Tag Lit, Map Tag Lit)
lookupSent ind (S rds lemmas wforms) = if IM.null c then Nothing else Just (c,l,w)
 where 
   c = fromMaybe IM.empty (IM.lookup ind rds)
   [l,w] = fromMaybe [M.empty,M.empty] (sequence (IM.lookup ind `fmap` [lemmas,wforms]))

partitionCohort :: IntSet -> Cohort -> (IntMap Lit,IntMap Lit)
partitionCohort is coh = let onlykeys = IM.fromSet (const true) is --Intersection is based on keys
                             inmap = IM.intersection coh onlykeys 
                             outmap = IM.difference coh onlykeys
                         in (inmap,outmap)


partitionTarget :: Oper -> IntSet -> Cohort -> (IntMap Lit,IntMap Lit)
partitionTarget op is coh = case op of
  SELECT -> (outmap,inmap)
  REMOVE -> (inmap,outmap)
  _   -> (inmap,outmap) --TODO other operations
 where
  (inmap,outmap) = partitionCohort is coh

--------------------------------------------------------------------------------


-- | Takes a tagset, with OrLists of underspecified readings,  and returns corresponding IntSets of fully specified readings.
-- Compare to normaliseRel in cghs/Rule: it only does the set operations relative to the underspecified readings, not with absolute IntSets.
-- That's why we cannot handle Diffs in normaliseRel, but only here.
normaliseTagsetAbs :: TagSet -> Map Tag IntSet -> Either Match IntSet
normaliseTagsetAbs tagset tagmap = case tagset of
  All -> Left AllTags
  Set s -> Right (lu s tagmap)
  Union t t' -> liftM2 IS.union (norm t) (norm t')
  Inters t t' -> liftM2 IS.intersection (norm t) (norm t')

{- Intended behaviour:
     adv adV (ada xx) `diff` ada == adv adV
     adv adV ada `diff` (ada xx) == adv adV (ada ((*) -xx)) 
     OBS. this all breaks when we split morph/synt. tags from lexical -}
  Diff t t' 
    -> do is  <- norm t
          is' <- norm t'
          if IS.null is -- only lexical tags on the left side
              then Left $ Not is' --TODO include Lex in what this returns too
              else Right $ IS.difference is is'


{- say t=[n,adj] and t'=[m,f,mf]
   is  = specified readings that match t: (n foo), (n bar), (n mf), ... , (adj foo), (adj bar), (adj f), ..
   is' = specified readings that match t': (n m), (n f), (vblex sg p3 mf), (adj f), ..
   intersection of the specified readings returns those that match 
   sequence [[n,adj],[m,f,mf]]: (n m), (n f), ..., (adj f), (adj mf) -}
  Cart t t' -> liftM2 IS.intersection (norm t) (norm t')

 where
  norm = (`normaliseTagsetAbs` tagmap)

  lu :: OrList Reading -> Map Tag IntSet -> IntSet
  lu s m = IS.unions [ fold1 IS.intersection $ 
                         catMaybes [ M.lookup t m | t <- getAndList ts
                                                  , not $ isLex t ] -- don't include lexical tags
                        | ts <- getOrList s ]

  fold1 f [] = IS.empty
  fold1 f xs = foldl1 f xs

lemsAndWFs :: TagSet -> (OrList Tag,OrList Tag)
lemsAndWFs tagset = case normaliseTagsetRel tagset of
  Set s -> let onlytags = concatMap getAndList (getOrList s)
           in ( Or $ filter isLem onlytags, Or $ filter isWF onlytags )
  _     -> (Or [],Or []) -- TODO
 where 
  isLem (Lem _) = True
  isLem _       = False

  isWF (WF _) = True
  isWF _      = False