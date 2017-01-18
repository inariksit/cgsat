{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

module CG_SAT where

import Rule hiding ( Not, Negate )
import qualified Rule as R
import Utils
import SAT ( Solver(..) )
import SAT.Named


import Data.Foldable ( fold )
import Data.IntMap ( IntMap, (!), elems )
import qualified Data.IntMap as IM
import Data.IntSet ( IntSet )
import qualified Data.IntSet as IS 
import Data.List ( intercalate, findIndices )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( catMaybes, fromMaybe )

import Control.Monad ( liftM2, mapAndUnzipM, zipWithM )
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

data Pattern = Pat { positions :: OrList (SeqList Int)
                   , contexts :: SeqList Match } 
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


updateSentence :: RSIO ()
updateSentence = do 
  oldSent <- get
  put $ doStuff oldSent

 where
  doStuff x = undefined

  changeReading :: Cohort -> (Int,Lit) -> Cohort
  changeReading coh (i,newrd) = IM.adjust (const newrd) i coh

  changeCohort :: Sentence -> Int-> Cohort -> Sentence
  changeCohort sent i newcoh = IM.adjust (const newcoh) i sent

--------------------------------------------------------------------------------
-- 

condLits :: Rule -- ^ Rule, whose conditions to turn into literals
         -> Int  -- ^ Absolute position of the target in the sentence
         -> RSIO (Maybe -- Each condition must hold; but each of them may
              (AndList (OrList (OrList Lit)))) -- be fulfilled in various ways.          
condLits rule origin = do
  slen <- gets senlength
  --liftIO $ print (origin, slen) --DEBUG
  pats <- ctx2Pattern slen origin `mapM` context rule -- :: AndList (OrList Pattern)
  if any (all patNull) pats -- any OrList whose *all* positions are empty
   then return Nothing -- Some of the conditions is out of scope, doesn't apply
   else do
    lits <- mapM (mapM (pattern2Lits origin)) pats :: RSIO (AndList (OrList (OrList Lit)))
    return $ Just lits

 where
  patNull = null . positions

--------------------------------------------------------------------------------
-- Transform the contextual test(s) of one rule into a Pattern.

ctx2Pattern :: Int  -- ^ Sentence length
            -> Int  -- ^ Absolute position of the target in the sentence
            -> Context -- ^ Context to be transformed
            -> RSIO (OrList Pattern) -- ^ List of patterns. Length >1 only if the context is template.
ctx2Pattern senlen origin ctx = case ctx of
  Always -> return (Or [PatAlways])
  c@(Ctx posn polr tgst)
    -> do (ps,m) <- singleCtx2Pat c
          return (Or [Pat ps m])
          
  Link ctxs 
    -> do (ps,cs) <- mapAndUnzipM singleCtx2Pat (getAndList ctxs)
          return (Or [Pat (fold ps) (fold cs)])
    -- To support linked templates (which don't make sense), do like below. 
    -- pats <- And `fmap` (ctx2Pattern senlen origin `mapM` getAndList ctxs) 
    -- :t pats :: AndList (OrList Pattern)

  Template ctxs
    -> do pats <- ctx2Pattern senlen origin `mapM` ctxs --  :: OrList Pattern
          return (fold pats)

  R.Negate ctx 
    -> do pat <- ctx2Pattern senlen origin ctx -- :: OrList Pattern
          return (fmap Negate pat)

 where 
  singleCtx2Pat (Ctx posn polr tgst) = 
    do tagset <- normaliseTagsetAbs tgst `fmap` asks tagMap
       matchOp <- getMatch origin posn polr
       let match = maybe (error "ctx2Pattern: invalid tagset") matchOp tagset
       let allPositions = normalisePosition posn senlen origin :: OrList Int -- 1* -> e.g. [2,3,4]
       --liftIO $ putStrLn ("allPositions " ++ show allPositions) --DEBUG
       return (fmap (:[]) allPositions, [match] ) 



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

pattern2Lits :: Int -- ^ Position of the target cohort in the sentence
             -> Pattern -- ^ Conditions to turn into literals
             -> RSIO (OrList Lit) -- ^ All possible ways to satisfy the conditions
pattern2Lits origin pat = do 
  s <- asks solver
  sent <- gets sentence
  Or `fmap` sequence
        -- OBS. potential for sharing: if cohorts are e.g. [1,2],[1,3],[1,4],
        -- we can keep result of 1 somewhere and not compute it all over many times.
       [ do lits <- zipWithM match2CondLit matches cohorts
            if length cohorts == length matches
              then liftIO $ andl' s lits
              else error "pattern2Lits: contextual test(s) out of scope"
          | inds <- getOrList $ positions pat  -- inds :: [Int]
          , let cohorts = [ (i,c) | (i,c) <- IM.assocs sent
                                  , i `elem` inds ]
          , let matches = contexts pat
       ] 

-- OBS. Try adding some short-term cache; add something in the state,
-- and before computing the literal, see if it's there
match2CondLit :: Match -> (Int,Cohort) -> RSIO Lit
match2CondLit (Bar (bi,bm) mat) (i,coh) = do
  s <- asks solver
  sent <- gets sentence
  matchLit <- match2CondLit mat (i,coh)

  let barInds | bi <= i   = [bi..i]
              | otherwise = [i..bi]

  let barCohorts = [ (i,c) | (i,c) <- IM.assocs sent
                           , i `elem` barInds ]

  let negBM = case bm of
                Mix is -> Not is
                Cau is -> NotCau is
                _      -> error "match2CondLit: invalid condition for BARRIER"

  -- cohorts between the condition and target do NOT contain the specified readings
  barLits <- mapM (match2CondLit negBM) barCohorts 

  liftIO $ andl' s (matchLit:barLits)

match2CondLit mat (i,coh) = do 
  s <- asks solver
  liftIO $ case mat of
    -- if Mix is for condition, then it doesn't matter whether some tag not
    -- in the tagset is True. Only for *targets* there must be something else.
    Mix is -> do let (inmap,_) = partitionIM is coh
                 orl' s (elems inmap)

              -- Any difference whether to compute neg $ orl' or andl $ map neg?                 
    Cau is -> do let (inmap,outmap) = partitionIM is coh
                 andl' s =<< sequence [ orl' s (elems inmap)
                                      , neg `fmap` orl' s (elems outmap) ]

    Not is -> do let (inmap,outmap) = partitionIM is coh
                 andl' s =<< sequence [ andl' s (neg `fmap` elems inmap)
                                      , orl' s (elems outmap) ]

  -- `NOT 1C foo' means: "either there's no foo, or the foo is not unique."
  -- Either way, having at least one true non-foo lit fulfils the condition,
  -- because the cohort may not be empty.
    NotCau is -> do let (_,outmap) = partitionIM is coh
                    orl' s (elems outmap) 

    AllTags   -> return true

    _ -> error "match2CondLit: :("

 where
  partitionIM :: IntSet -> IntMap Lit -> (IntMap Lit,IntMap Lit)
  partitionIM is im = let onlykeys = IM.fromSet (const true) is --Intersection is based on keys
                          inmap = IM.intersection im onlykeys 
                          outmap = IM.difference im onlykeys
                      in (inmap,outmap)



--------------------------------------------------------------------------------
  -- For future use, we may want to do some fine-grained inspection,
  -- ie. which parts of the conditions may block some rule.
  -- For now, we just flatten them. But here's the structure of the 3 nested lists.
  {- For future reference:
    condLitsPerTrg :: [AndList (OrList (OrList Lit))] 
    condLitsPerCond :: AndList (OrList (OrList Lit)) 
      -- each individual condition, e.g. IF (-1 foo) (1 bar) (2* baz LINK 1 quux)
    condTempl :: OrList (OrList Lit)
      -- if the individual condition is a template, e.g. IF ( (-1 foo) OR (1 bar) )
    allIndsLinkedCond :: OrList Lit
      -- if the condition is scanning, all the indices that match.
      -- if it's linked, the lits come from ANDing several indices.
      -- e.g. IF (-1* foo LINK 1 bar) --> lit@[2,3] OR lit@[3,4] OR lit@[4,5].
  -}
flattenCondLits :: [(Int, AndList (OrList (OrList Lit)))] -> [(Int,[[Lit]])] --TODO appropriate AndList and OrList
flattenCondLits litsPerTrg = 
  [ (,) iTrg $ concat 
    [ [ getOrList allIndsLinkedCond  -- 3) finally, just the individual (possibly linked) conditions, e.g. [2,3]
       | allIndsLinkedCond <- getOrList condTempl -- 2) second level is linked (or single) condition in different indices:
      ]                               -- IF (1* foo LINK 1 bar) -> 
      | condTempl <- getAndList litsPerCond  -- 1) the first level of OrList is template: 
    ]                                        -- e.g. 
    | (iTrg,litsPerCond) <- litsPerTrg 
  ]


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
  lu s m = IS.unions [ foldl1 IS.intersection $ 
                        catMaybes [ M.lookup t m | t <- getAndList ts ]
                        | ts <- getOrList s ]
