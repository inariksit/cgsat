module CGSAT (

    apply, trigger, defaultRules

  , solveAndPrint

  , RWSE, rwse, evalRWSE
  , CGException(..)
  , Config, mkConfig
  , Env, envRules

  ) where

import CGHS.Rule hiding ( Not, Negate )
import qualified CGHS.Rule as R
import CGHS 
--( isLex, normalisePosition, normaliseTagsetRel )

import CGSAT.Base
import CGSAT.Context
import CGSAT.Tagset


import qualified Data.IntMap as IM
import qualified Data.IntSet as IS 
import qualified Data.Map as M

import Data.List ( findIndices, intercalate, nub )
import Data.Maybe ( catMaybes, fromMaybe, isNothing )


--------------------------------------------------------------------------------



-- The type Reading that CGHS/Rule exports is actually an underspecified reading.
-- For our symbolic sentences, each reading has a word form, lemma and other tags.
-- Maybe make some newtypes to enforce that wform is WF _ and lemma is Lem _?
data SymbolicReading = SR { wform :: Tag 
                          , lemma :: Tag 
                          , reading :: Reading } deriving (Eq)

instance Show SymbolicReading where
  show (SR w l r) = show w ++ show l ++ show r



--------------------------------------------------------------------------------
-- Interaction with SAT-solver, non-pure functions



solveAndPrint :: Bool -> Solver -> [Lit] -> Sentence -> IO ()
solveAndPrint verbose s ass sent = do
  b <- solve s ass
  if b then do
          when verbose $ print ass
          vals <- sequence 
                   [ sequence [ modelValue s lit | lit <- [true] ] -- TODO! IM.elems word ] 
                      | (sind,wfs_lems_rds) <- IM.assocs sent ]
          --let trueAnas =
          --     [ "\"w" ++ show sind ++ "\"\n"
          --        ++ unlines [ "\t"++show ana | (ana, True) <- zip (IM.elems word) vs ]
          --       | ((sind,word), vs) <- zip (IM.assocs sent) vals ]
          --mapM_ putStrLn trueAnas
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
              NoReadingsLeft -> return (true,true,true,emptyCohort,IM.empty)
              OutOfScope _ _ -> return (true,true,true,emptyCohort,IM.empty)
              TagsetNotFound s -> do liftIO $ putStrLn ("Warning: tagset " ++ s ++ " not found")
                                     return (true,true,true,emptyCohort,IM.empty)
              _              -> throwError e

                   --TODO
       if IM.null (readings trgCoh) -- if one of the expected errors was caught,

        then return sen -- just return the sentence unchanged

        else do onlyTrgLeft <- liftIO $ andl' s [ someTrgIsTrue, neg someOtherIsTrue ]
                cannotApply <- liftIO $ orl' s [ neg allCondsHold, onlyTrgLeft ]
 
                newTrgLits <- liftIO $ sequence               
                      [ (,) j `fmap`        --wN<a>' is true if both of the following:
                         andl s newTrgName [ oldTrgLit     --wN<a> was also true, and
                                           , cannotApply ] --rule cannot apply 
                         | (j,oldTrgLit) <- IM.assocs trgRds
                         , let newTrgName = show oldTrgLit ++ "'" ]

                --let newcoh = foldl changeReading trgCoh newTrgLits
                --let newsen = changeCohort sen i newcoh
                --return newsen
                return sen --TODO

  ----------------------------------------------------
  sen <- gets sentence --TODO
  put (Config w sen) --TODO
 -- sen <- gets sentence
 -- newSen <- foldM applyToCohort sen [1..w]
 -- put (Config w newSen)

 --where
 -- changeReading :: Cohort -> (Int,Lit) -> Cohort
 -- changeReading coh (i,newrd) = IM.adjust (const newrd) i coh

 -- changeCohort :: Sentence -> Int -> Cohort -> Sentence
 -- changeCohort sen i newcoh = IM.adjust (const newcoh) i sen


trigger :: Rule -> Int -> RWSE (Lit,Lit,Lit,Cohort,IntMap Lit)
trigger rule origin = do
  (Config len sen) <- get
  tm <- asks tagMap
  s <- asks solver
  conds <- condLits rule origin
  trgCoh <- case IM.lookup origin sen of
              Nothing -> do tell [ "trigger: target position " ++ show origin ++ 
                                   " out of scope, sentence length " ++ show len ]
                            throwError (OutOfScope origin "trigger")
              Just ch -> return ch
  trgIS <- case normaliseTagsetAbs (target rule) tm of
             Left AllTags -> do tell [ "trigger: rule " ++ show rule ++ 
                                       " tries to remove or select all readings" ]
                                throwError NoReadingsLeft
             Right intset -> return intset --TODO: made normaliseTagsetAbs return always Left
                                                    --TODO
  let (trg,oth) = partitionTarget (oper rule) trgIS (readings trgCoh)

  mht <- liftIO $ orl' s (IM.elems trg) -- 1) Cohort has ≥1 target lits
  mho <- liftIO $ orl' s (IM.elems oth) -- 2) Cohort has ≥1 other lits                              
  ach <- liftIO $ andl' s (getAndList conds) -- 3) All conditions hold
  return (mht, mho, ach, trgCoh, trg)


--TODO update to new system
defaultRules :: Solver -> Sentence -> IO ()
defaultRules s sentence = 
   sequence_ [ do addClause s lits          --Every word must have >=1 reading
                --  constraints s mp [] form  --Constraints based on lexicon --TODO AmbiguityClasses.hs
               | (Coh _ _ coh) <- IM.elems sentence
               , let lits = IM.elems coh 
               , let mp i = fromMaybe (error $ "constraints: " ++ show i) (IM.lookup i coh) ] 

--------------------------------------------------------------------------------
-- From here on, only pure helper functions



partitionTarget :: Oper -> IntSet -> IntMap Lit -> (IntMap Lit,IntMap Lit)
partitionTarget op is coh = case op of
  SELECT -> (outmap,inmap)
  REMOVE -> (inmap,outmap)
  _   -> (inmap,outmap) --TODO other operations
 where
  (inmap,outmap) = IM.partitionWithKey (\ k _ -> IS.member k is) coh

