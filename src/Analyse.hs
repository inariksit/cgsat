module Analyse (
    testRules
  , testRule
  , width
  , Conflict(..)
  , isConflict
  , ruleTriggers
  ) where


import CGSAT
import CGSAT.Base


import CGHS

import Data.Foldable ( fold )
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Either ( lefts, rights )
import Data.List ( nub )
import Data.Maybe ( catMaybes, fromMaybe )


--------------------------------------------------------------------------------

data Conflict = NoConf | Internal | Interaction -- [Rule]
  deriving (Show,Eq)

isConflict :: Conflict -> Bool
isConflict NoConf = False
isConflict _      = True

----------------------------------------------------------------------------
-- Functions that apply only for analysis, not disambiguation

testRules :: Bool -> [Rule] -> RWSE [Conflict]
testRules v = mapM (testRule v)


testRule :: Bool -> Rule -> RWSE Conflict
testRule v rule = do
  when v $ liftIO $ putStrLn ("\n=============\n" ++ show rule)
  len <- gets senlength
  confs <- mapM (evalE . ruleTriggers v rule) [1..len]
              -- 1) Test if the rule may apply, and return result of that
  apply rule  -- 2) Apply the rule regardless


  let legitConfs = rights confs
  when v $ liftIO $ print confs
  if NoConf `elem` legitConfs
    then return NoConf
    else if Interaction `elem` legitConfs 
      then return Interaction
      else return Internal



ruleTriggers :: Bool -> Rule -> Int -> RWSE Conflict
ruleTriggers verbose rule i = do
  s <- asks solver
  (Config len sen) <- get
  reqOrErr <- Just `fmap` trigger rule i 
                `catchError` \e -> case e of
                   TagsetNotFound _ -> return Nothing -- Treat as an internal conflict
                   _ -> throwError e -- Other thing went wrong: treat as an error
  case reqOrErr of
    Nothing -> return Internal -- If tagset is not found, treat it as an internal conflict
    Just (allCondsHold, trgCohs_otherLits) -> do
      let (trgCohs, otherLits) = unzip trgCohs_otherLits
      mustHaveTarget <- liftIO $ getTargetLit s trgCohs
      mustHaveOther <- liftIO $ orl' s otherLits
      b <- liftIO $
        if verbose then solveAndPrint True s [mustHaveTarget, mustHaveOther, allCondsHold] sen
                 else solve s [allCondsHold,mustHaveTarget,mustHaveOther]
      if b then return NoConf
       else do s' <- liftIO newSolver
               c <- local (withNewSolver s') $ do
                      newConfig len
                      tempSen <- gets sentence
                      liftIO $ defaultRules s' tempSen
                      (condsHold,trg_oth) <- trigger rule i
                      let (trg,oth) = unzip trg_oth 
                      mustHaveTrg <- liftIO $ getTargetLit s' trg
                      mustHaveOth <- liftIO $ orl' s' oth
                      b <- liftIO $ if verbose then do
                                           putStrLn "\nTrying to solve with a fresh sentence:"
                                           solveAndPrint True s' [condsHold,mustHaveTrg,mustHaveOth] tempSen
                                     else solve s' [condsHold,mustHaveTrg,mustHaveOth]
                      return $ if b then Interaction else Internal
               liftIO $ deleteSolver s'
               put (Config len sen)
               return c

 where
  getTargetLit :: Solver -> [SplitCohort] -> IO Lit
  getTargetLit s trgCohs =  andl' s =<< sequence
    [ do wfLit  <- orl' s (maybe [true] M.elems mw)
         lemLit <- orl' s (maybe [true] M.elems ml)
         rdLit  <- orl' s (maybe [true] M.elems mr)
         andl' s [wfLit, lemLit, rdLit]
     | (SCoh mw ml mr) <- trgCohs ]


