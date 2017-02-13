{-# LANGUAGE FlexibleContexts #-}

module CGSAT.Context where

-- Everything related to contextual tests.

import CGSAT.Base
import CGSAT.Tagset
import CGHS hiding ( Not,Negate )
import qualified CGHS

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M

import Data.Foldable ( fold )
import Data.List ( nub )

--------------------------------------------------------------------------------

type SeqList a = [a] -- List of things that follow each other in sequence

data Pattern = Pat (OrList  -- OrList length >1 if the ctx is template
                           ( OrList (SeqList Int) -- SeqLists are same length
                           , SeqList (OrList Match) -- OrList because of the split:
                           )
                   )            
             | PatAlways 
             | Negate Pattern -- Negate goes beyond set negation and C
             deriving (Eq)


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
    -> do (ps,m) <- singleCtx2Pat c
          return $ Pat (Or [ (ps,m) ] )
          
  Link ctxs 
    -> do (ps,ms) <- mapAndUnzipM singleCtx2Pat (getAndList ctxs)
          return $ Pat (Or [(fold ps, fold ms)])
    -- This wouldn't support linked templates, 
    -- but I don't think any sane CG engine supports them either.

  Template ctxs
    -> do pats <- ctx2Pattern senlen origin `mapM` ctxs --  :: OrList Pattern
          return undefined --(fold pats)

  CGHS.Negate ctx 
    -> Negate `fmap` ctx2Pattern senlen origin ctx

 where 
  singleCtx2Pat (Ctx posn polr tgst) = 
    do let allPositions = normalisePosition posn senlen origin :: OrList Int -- 1* -> e.g. [2,3,4]
       if null (getOrList allPositions) 
        then do --tell ["ctx2Pattern: position " ++ show posn ++ " not in scope"]
                throwError $ OutOfScope origin "ctx2Pattern" -- Pattern fails because condition(s) are not in scope. 
                -- This is to be expected, because we try to apply every rule to every cohort.
        else do 
          splitRds <- normaliseTagsetAbs tgst `fmap` ask  -- :: RWSE (OrList SplitReading)

          let matchtype = getMatchType posn polr -- TODO: Barrier case doesn't work

          let matches = either (\x -> Or [x]) (fmap (M matchtype)) splitRds
          if all nullMatch matches
            then do tell ["singleCtx2Pat: tagset " ++ show tgst ++" not found, rule cannot apply"]
                    throwError $ TagsetNotFound (show tgst) -- Pattern fails because tagset is not found in any readings, ie. it won't match anything.
                                             -- This is unexpected, and indicates a bug in the grammar, TODO alert user!!!!
            else return (fmap (:[]) allPositions, [matches] ) 


--getMatch :: Int -> Position -> Polarity -> RWSE Match
--getMatch origin pos pol = case (pos,pol) of
--  (Pos (Barrier ts) c n, _) -> do
--    tsMatch <- getMatch origin (Pos AtLeast c n) pol
--    barTags <- normaliseTagsetAbs ts `fmap` asks tagMap
--    let barMatch = either id Mix barTags
--    let barInd = origin + n 
--    return $ Bar (barInd,barMatch) . tsMatch

--  (Pos (CBarrier ts) c n, _) -> do 
--    tsMatch <- getMatch origin (Pos AtLeast c n) pol
--    barTags <- normaliseTagsetAbs ts `fmap` asks tagMap
--    let barMatch = either id Cau barTags
--    let barInd = origin + n 
--    return $ Bar (barInd,barMatch) . tsMatch

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
    Pat pats  -> do let ms_is = concat [ nub $ concat
                                  [ zip mats inds | inds <- getOrList indss ]
                                   | (indss,mats) <- getOrList pats ]

                    lits <- mapM (uncurry matches2CondLit) ms_is
                    liftIO $ orl' s lits

matches2CondLit :: OrList Match -> Int -> RWSE Lit
matches2CondLit mats ind = do
  s <- asks solver
  lits <- sequence [ match2CondLit mat ind
                        | mat <- getOrList mats ]
  liftIO $ orl' s lits

match2CondLit :: Match -> Int -> RWSE Lit
match2CondLits AllTags ind = return true --TODO: check if index is out of scope, then return False
match2CondLit (Bar (bi,bm) mat) ind = undefined

--match2CondLit (M mtype wfs lems mrds) ind = do
match2CondLit (M mtype splitrd) ind = do

  s <- asks solver
  (Config len sen) <- get

  coh <- case IM.lookup ind sen of 
           Nothing -> do tell [ "match2CondLit: position " ++ show ind ++ 
                                " out of scope, sentence length " ++ show len ]
                         throwError (OutOfScope ind "match2CondLit")                 
           Just co -> return co

  (SCoh inWFs inLems inRds, SCoh outWFs outLems outRds) <- partitionCohort coh splitrd


  liftIO $ case mtype of
    Mix -> do mixWF <- mix s inWFs
              mixLemma <- mix s inLems
              mixReading <- mix s inRds
              andl' s [mixWF, mixLemma, mixReading]

    Cau -> do cauWF <- cau s inWFs outWFs
              cauLemma <- cau s inLems outLems
              cauReading <- cau s inRds outRds
              andl' s [cauWF, cauLemma, cauReading]
    Not -> do notWF <- cau s outWFs inWFs
              notLemma <- cau s outLems inLems
              notReading <- cau s outRds inRds
              andl' s [notWF, notLemma, notReading]
    NotCau -> do ncWF <- mix s outWFs
                 ncLemma <- mix s outLems
                 ncReading <-  mix s outRds
                 andl' s [ncWF, ncLemma, ncReading]

 where
  mix s y = case y of  -- If partitionCohort encountered an empty slot in a SplitReading,
             Nothing -> return true -- it doesn't care: default rule will be enough for determining the readings in this particular slot.
             Just mp -> orl' s (M.elems mp)

  cau s y n = case (y,n) of  
     (Nothing, Nothing) -> return true
     (Just ym, Just nm) -> andl' s =<< sequence
                               [ orl' s (M.elems ym)
                               , neg `fmap` orl' s (M.elems nm) ]

{-
match2CondLit (Bar (bi,bm) mat) ind = do
  s <- asks solver
  sen <- gets sentence
  matchLit <- match2CondLit mat  ind

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
-}