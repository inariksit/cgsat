{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

module CG_SAT where

import Rule hiding ( Not, Negate )
import qualified Rule as R
import Utils
import SAT ( Solver(..) )
import SAT.Named


import Data.Foldable ( fold )
import Data.IntMap ( IntMap, (!), elems, assocs )
import qualified Data.IntMap as IM
import Data.IntSet ( IntSet )
import qualified Data.IntSet as IS 
import Data.List ( intercalate, findIndices, nub )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( catMaybes, fromMaybe, isNothing )

import Control.Monad ( foldM, liftM2, mapAndUnzipM, zipWithM )
import Control.Monad.Trans
import Control.Monad.IO.Class ( MonadIO(..), liftIO )
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.State ( StateT(..) )
import Control.Monad.Trans.Reader ( ReaderT(..) )

--------------------------------------------------------------------------------

newtype RSIO a = RSIO { runRSIO :: ReaderT Env (StateT Conf IO) a }
  deriving (Functor, Applicative, Monad, MonadState Conf, MonadReader Env, MonadIO)



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
               , solver :: Solver } 

data Conf = Conf { sentence :: Sentence 
                 , senlength :: Int }

type Sentence = IntMap Cohort
type Cohort = IntMap Lit

emptyConf = Conf IM.empty 0

type SeqList a = [a] -- List of things that follow each other in sequence

data Pattern = Pat (OrList ( OrList (SeqList Int) -- OrList length >1 if the ctx is template
                           , SeqList Match ) )
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


--------------------------------------------------------------------------------
-- Interaction with SAT-solver, non-pure functions


-- | Using the readings and the solver in environment, create a sentence.
-- (should I also put it in the env???)
mkSentence :: Int -> RSIO Sentence
mkSentence width = do
  s <- asks solver
  rds <- asks rdMap
  liftIO $ IM.fromList `fmap` sequence 
    [ (,) n `fmap` sequence (IM.mapWithKey (mkLit s n) rds)
        | n <- [1..width] ] 
 where
  mkLit :: Solver -> Int -> Int -> Reading -> IO Lit
  mkLit s n m rd = newLit s (showReading rd n m)

  showReading :: Reading -> Int -> Int -> String
  showReading (And ts)  wdi rdi = "w" ++ show wdi ++ concatMap (\t -> '<':show t++">") ts


--------------------------------------------------------------------------------
-- 

-- | Apply the rule to all applicable cohorts in the sentence
apply :: Rule -> RSIO Sentence
apply rule = do 
  s <- asks solver
  w <- gets senlength

  targetTagSet <- normaliseTagsetAbs (target rule) `fmap` asks tagMap

  maybeCondLitsPerTrg <- sequence 
       [ (,) i `fmap` condLits rule i | i <- [1..w] ] -- may be out of scope

  let condLitsPerTrg = 
       [ (i,lits) | (i,Just lits) <- maybeCondLitsPerTrg ] -- keep only those in scope



  case targetTagSet of
    Nothing -> do sent <- gets sentence
                  return sent --Tried to remove all; as per VISL CG-3 behaviour, remove nothing.

    Just is -> do
      let applyToCohort sent (i,condLits) = do
              let coh = sent ! i
              let (inmap,outmap) = partitionCohort is coh
              let (trg,oth) = case oper rule of
                               SELECT -> (outmap,inmap)
                               REMOVE -> (inmap,outmap)
                               _   -> (inmap,outmap) --TODO other operations
 
              ----------- The seitan of the function ------------
              allCondsHold  <- andl' s (getAndList condLits)
              -- =<< sequence 
              --                            [ orl' s $ getOrList litsPerCond 
              --                              | litsPerCond <- getAndList condLits ]
              someTrgIsTrue <- orl' s (elems trg)
              noOtherIsTrue <- andl' s (neg `fmap` elems oth)
              onlyTrgLeft   <- andl' s [ someTrgIsTrue, noOtherIsTrue ]
              cannotApply   <- orl' s [ neg allCondsHold, onlyTrgLeft ]
 
              newTrgLits <- sequence               
                  [ (,) i `fmap`        --wN<a>' is true if both of the following:
                     andl s newTrgName [ oldTrgLit     --wN<a> was also true, and
                                       , cannotApply ] --rule cannot apply 
                     | (i,oldTrgLit) <- assocs trg
                     , let newTrgName = show oldTrgLit ++ "'" ]
              ----------------------------------------------------

              let newcoh = foldl changeReading coh newTrgLits
              let newsent = changeCohort sent i newcoh
              return newsent

      --let flatCondLits = flattenCondLits condLitsPerTrg :: AndList (Int, AndList (OrList Lit))
      sent <- gets sentence
      newSent <- liftIO $ foldM applyToCohort sent condLitsPerTrg
      put (Conf newSent w)
      return newSent

 where
  changeReading :: Cohort -> (Int,Lit) -> Cohort
  changeReading coh (i,newrd) = IM.adjust (const newrd) i coh

  changeCohort :: Sentence -> Int-> Cohort -> Sentence
  changeCohort sent i newcoh = IM.adjust (const newcoh) i sent
--------------------------------------------------------------------------------
-- Create new literals from contextual tests of a rule

condLits :: Rule -- ^ Rule, whose conditions to turn into literals
         -> Int  -- ^ Absolute position of the target in the sentence
         -> RSIO (Maybe -- If some condition(s) are out of scope, return Nothing.
                (AndList Lit)) -- Otherwise return literal for each condition.
condLits rule origin = do
  slen <- gets senlength
  --liftIO $ print (origin, slen) --DEBUG
  pats <- ctx2Pattern slen origin `mapM` context rule -- :: AndList (Maybe Pattern)
  if any isNothing (getAndList pats)
   then do liftIO $ putStrLn $ "condLits: pattern fails in index " ++ show origin ++ " with rule " ++ show rule
           return Nothing -- Some of the conditions is out of scope, doesn't apply
   else do
    lits <- mapM pattern2Lit (fmap (\(Just x) -> x) pats) :: RSIO (AndList Lit)
    return $ Just lits


--------------------------------------------------------------------------------
-- Transform the contextual test(s) of one rule into a Pattern.

ctx2Pattern :: Int  -- ^ Sentence length
            -> Int  -- ^ Absolute position of the target in the sentence
            -> Context -- ^ Context to be transformed
            -> RSIO (Maybe Pattern)
ctx2Pattern senlen origin ctx = case ctx of
  Always -> return $ Just PatAlways
  c@(Ctx posn polr tgst)
    -> do psm <- singleCtx2Pat c
          case psm of
            Nothing -> return Nothing
            Just (ps,m) -> return $ Just (Pat $ Or [(ps,m)])
          
  Link ctxs 
    -> do psms <- mapM singleCtx2Pat (getAndList ctxs)
          case any isNothing psms of
            True -> return Nothing
            False -> do let (ps,ms) = unzip (catMaybes psms)
                        liftIO $ print ("ctx2Pattern",take 10 ps, take 10 ms) -- DEBUG
                        return $ Just (Pat $ Or [(fold ps, fold ms)])
    -- This wouldn't support linked templates, 
    -- but I don't think any sane CG engine supports them either.

  Template ctxs
    -> do pats <- ctx2Pattern senlen origin `mapM` ctxs --  :: OrList Pattern
          return undefined --(fold pats)

  R.Negate ctx 
    -> do pat <- ctx2Pattern senlen origin ctx
          return $ case pat of
            Nothing -> Nothing
            Just x  -> Just (Negate x)

 where 
  singleCtx2Pat (Ctx posn polr tgst) = 
    do tagset <- normaliseTagsetAbs tgst `fmap` asks tagMap
       let allPositions = normalisePosition posn senlen origin :: OrList Int -- 1* -> e.g. [2,3,4]
       case null (getOrList allPositions) of
        True  -> do liftIO $ putStrLn "singleCtx2Pat: condition out of scope"
                    return Nothing -- Pattern fails because condition(s) are not in scope. 
                                -- This is to be expected, because we try to apply every rule to every cohort.
        False -> case tagset of
                  Nothing -> return $ Just ( fmap (:[]) allPositions, [AllTags] ) -- if normaliseTagsetAbs returns Nothing, it was (*).
                  Just is -> case IS.null is of
                              True  -> do liftIO $ putStrLn $ "singleCtx2Pat: tagset " ++ show tgst ++ " not found, rule cannot apply"
                                          return Nothing -- Pattern fails because tagset is not found in any readings, ie. it won't match anything.
                                                      -- This is unexpected, and indicates a bug in the grammar, TODO alert user!!!!
                              False -> do matchOp <- getMatch origin posn polr
                                          let match = matchOp is
                                          return $ Just (fmap (:[]) allPositions, [match] ) 



getMatch :: Int -> Position -> Polarity -> RSIO (IntSet -> Match)
getMatch origin pos pol = case (pos,pol) of
  (Pos (Barrier ts) c n, _) -> do
    tsMatch <- getMatch origin (Pos AtLeast c n) pol
    barTags <- normaliseTagsetAbs ts `fmap` asks tagMap
    let barMatch = maybe AllTags Mix barTags
    let barInd = origin + n 
    return $ Bar (barInd,barMatch) . tsMatch

  (Pos (CBarrier ts) c n, _) -> do 
    tsMatch <- getMatch origin (Pos AtLeast c n) pol
    barTags <- normaliseTagsetAbs ts `fmap` asks tagMap
    let barMatch = maybe AllTags Cau barTags
    let barInd = origin + n 
    return $ Bar (barInd,barMatch) . tsMatch

  (Pos _ NC _, Yes)   -> return Mix
  (Pos _ NC _, R.Not) -> return Not
  (Pos _ C _,  Yes)   -> return Cau
  (Pos _ C _,  R.Not) -> return NotCau

--------------------------------------------------------------------------------
-- Transform the pattern into a disjunction of literals

pattern2Lit :: Pattern -- ^ Conditions to turn into literals
            -> RSIO Lit -- ^ All possible ways to satisfy the conditions
pattern2Lit pat = do 
  s <- asks solver
  sent <- gets sentence
  case pat of
    PatAlways -> return true
    Negate p  -> neg `fmap` pattern2Lit p
    Pat pats  -> do let ms_is = concat [ nub $ concat
                                  [ zip mats inds | inds <- getOrList indss ]
                                   | (indss,mats) <- getOrList pats ]

                    lits <- mapM (uncurry match2CondLit) ms_is
                    liftIO $ orl' s lits


match2CondLit :: Match -> Int -> RSIO Lit
match2CondLit (Bar (bi,bm) mat) ind = do
  s <- asks solver
  sent <- gets sentence
  matchLit <- match2CondLit mat ind

  let barInds | bi <= ind = [bi..ind]
              | otherwise = [ind..bi]


  let negBM = case bm of
                Mix is -> Not is
                Cau is -> NotCau is
                _      -> error "match2CondLit: invalid condition for BARRIER"

  -- cohorts between the condition and target do NOT contain the specified readings
  barLits <- mapM (match2CondLit negBM) barInds

  liftIO $ andl' s (matchLit:barLits)

match2CondLit mat ind = do 
  s <- asks solver
  sent <- gets sentence
  let coh = fromMaybe (error "match2CondLit: condition out of scope") (IM.lookup ind sent)
  liftIO $ case mat of
    -- if Mix is for condition, then it doesn't matter whether some tag not
    -- in the tagset is True. Only for *targets* there must be something else.
    Mix is -> do let (inmap,_) = partitionCohort is coh
                 orl' s (elems inmap)

              -- Any difference whether to compute neg $ orl' or andl $ map neg?                 
    Cau is -> do let (inmap,outmap) = partitionCohort is coh
                 andl' s =<< sequence [ orl' s (elems inmap)
                                      , andl' s (neg `fmap` elems outmap) ]

    Not is -> do let (inmap,outmap) = partitionCohort is coh
                 andl' s =<< sequence [ andl' s (neg `fmap` elems inmap)
                                      , orl' s (elems outmap) ]

  -- `NOT 1C foo' means: "either there's no foo, or the foo is not unique."
  -- Either way, having at least one true non-foo lit fulfils the condition,
  -- because the cohort may not be empty.
    NotCau is -> do let (_,outmap) = partitionCohort is coh
                    orl' s (elems outmap) 

    AllTags   -> return true

    _ -> error "match2CondLit: :("


partitionCohort :: IntSet -> Cohort -> (IntMap Lit,IntMap Lit)
partitionCohort is coh = let onlykeys = IM.fromSet (const true) is --Intersection is based on keys
                             inmap = IM.intersection coh onlykeys 
                             outmap = IM.difference coh onlykeys
                         in (inmap,outmap)



--------------------------------------------------------------------------------
--TODO move somewhere else/merge into something?
defaultRules :: Solver -> Sentence -> IO ()
defaultRules s sentence = 
   sequence_ [ do addClause s lits          --Every word must have >=1 reading
                --  constraints s mp [] form  --Constraints based on lexicon --TODO AmbiguityClasses.hs
               | coh <- IM.elems sentence 
               , let lits = IM.elems coh 
               , let mp i = fromMaybe (error $ "constraints: " ++ show i) (IM.lookup i coh) ] 

--------------------------------------------------------------------------------
-- From here on, only pure helper functions

mkEnv :: Solver -> [Reading] -> [Tag] -> Env
mkEnv s rds tags = Env (mkTagMap tags rds) (mkRdMap rds) s 

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
mkTagMap :: [Tag] -> [Reading] -> Map Tag IntSet 
mkTagMap ts rds = M.fromList $
  ts `for` \t -> let getInds = IS.fromList . map (1+) . findIndices (elem t)
                 in (t, getInds rdLists) 
 where 
  for = flip fmap 
  rdLists = map getAndList rds
  -- maybe write this more readably later? --getInds :: Tag -> [Reading] -> IntSet

--------------------------------------------------------------------------------


-- | Takes a tagset, with OrLists of underspecified readings,  and returns corresponding IntSets of fully specified readings.
-- Compare to normaliseRel in cghs/Rule: it only does the set operations relative to the underspecified readings, not with absolute IntSets.
-- That's why we cannot handle Diffs in normaliseRel, but only here.
normaliseTagsetAbs :: TagSet -> Map Tag IntSet -> Maybe IntSet
normaliseTagsetAbs tagset tagmap = case tagset of
  All -> Nothing
  Set s -> Just (lu s tagmap)
  Union t t' -> liftM2 IS.union (norm t) (norm t')
  Inters t t' -> liftM2 IS.intersection (norm t) (norm t')

{- Intended behaviour:
     adv adV (ada "very") `diff` ada == adv adV
     adv adV ada `diff` (ada "very") == adv adV (ada ((*) -"very")) -}
  Diff t t' -> liftM2 IS.difference (norm t) (norm t')

{- say t=[n,adj] and t'=[m,f,mf]
   is  = specified readings that match t: (n foo), (n bar), (n mf), ... , (adj foo), (adj bar), (adj f), ..
   is' = specified readings that match t': (n m), (n f), (vblex sg p3 mf), (adj f), ..
   intersection of the specified readings returns those that match 
   sequence [[n,adj],[m,f,mf]]: (n m), (n f), ..., (adj f), (adj mf) -}
  Cart t t' -> liftM2 IS.intersection (norm t) (norm t')

 where
  norm = (`normaliseTagsetAbs` tagmap)

  lu :: OrList Reading -> M.Map Tag IntSet -> IntSet
  lu s m = IS.unions [ fold1 IS.intersection $ 
                         catMaybes [ M.lookup t m | t <- getAndList ts ]
                        | ts <- getOrList s ]

  fold1 f [] = IS.empty
  fold1 f xs = foldl1 f xs