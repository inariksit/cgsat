module Analyse (
    testRules
  , width
  , Conflict(..)
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
import Debug.Trace ( trace )
import System.Environment ( getArgs )


--------------------------------------------------------------------------------

data Conflict = NoConf | Internal | Interaction -- [Rule]
  deriving (Show,Eq)


----------------------------------------------------------------------------
-- Functions that apply only for analysis, not disambiguation

testRules :: Bool -> [Rule] -> RWSE [Conflict]
testRules v = mapM (testRule v)


testRule :: Bool -> Rule -> RWSE Conflict
testRule v rule = do
  liftIO (print rule)
  c@(Config len sen) <- get
  e@(Env w l r s) <- ask
  confs <-    -- I suspect there's a nicer way to handle this
     mapM (\i -> RWSE $ lift $ runExceptT $ runRWSE $ ruleTriggers v rule i
                  :: RWSE (Either CGException Conflict) )
                [1..len] -- 1) Test if the rule may apply, and return result of that

  apply rule             -- 2) Apply the rule regardless


  let legitConfs = rights confs
  liftIO $ print ("legit conflicts: ", legitConfs)
  if Interaction `elem` legitConfs 
    then return Interaction

    else if Internal `elem` legitConfs
      then return Internal
          else if null legitConfs
                then liftIO $ print confs >> return Internal
      else return NoConf


ruleTriggers :: Bool -> Rule -> Int -> RWSE Conflict
ruleTriggers verbose rule i = do
  s <- asks solver
  (Config len sen) <- get
  --liftIO $ defaultRules s sen
  (allCondsHold, trgCohs_otherLits) <- trigger rule i
  let (trgCohs, otherLits) = unzip trgCohs_otherLits

  mustHaveTarget <- liftIO $ getTargetLit s trgCohs

  --liftIO $ print ("mustHaveTarget: ", mustHaveTarget)
  --liftIO $ print ("trgCohs: ", trgCohs)
  mustHaveOther <- liftIO $ orl' s otherLits
  b <- liftIO $
        if verbose then solveAndPrint True s [mustHaveTarget, mustHaveOther, allCondsHold] sen
          else solve s [allCondsHold,mustHaveTarget,mustHaveOther]
  if b then return NoConf
   else 
     do s' <- liftIO newSolver
        c <- local (withNewSolver s') $ do tempConf <- mkConfig len
                                           put tempConf
                                           tempSen <- gets sentence
                                           liftIO $ defaultRules s' tempSen
                                           (condsHold,trg_oth) <- trigger rule i
                                           let (trg,oth) = unzip trg_oth 
                                           mustHaveTrg <- liftIO $ getTargetLit s' trg
                                           mustHaveOth <- liftIO $ orl' s' oth
                                           b <- liftIO $
                                                 if verbose then solveAndPrint True s' [condsHold,mustHaveTrg,mustHaveOth] tempSen
                                                   else solve s' [condsHold,mustHaveTrg,mustHaveOth]
                                           return $ if b then Interaction
                                                        else Internal
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


