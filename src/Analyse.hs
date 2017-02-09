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
  c@(Config len sen) <- get
  e@(Env w l r s) <- ask
  confs <-    -- I suspect there's a nicer way to handle this
     mapM (\i -> RWSE $ lift $ runExceptT $ runRWSE $ ruleTriggers v rule i
                  :: RWSE (Either CGException Conflict) )
                [1..len] -- 1) Test if the rule may apply, and return result of that

  apply rule             -- 2) Apply the rule regardless


  let legitConfs = rights confs
  liftIO $ print legitConfs
  if Interaction `elem` legitConfs 
    then liftIO $ print rule >> return Interaction

    else if Internal `elem` legitConfs || null legitConfs
      then liftIO $ print rule >> return Internal
      else return NoConf


ruleTriggers :: Bool -> Rule -> Int -> RWSE Conflict
ruleTriggers verbose rule i = do
  s <- asks solver
  (Config len sen) <- get
  (allCondsHold, trgCohs_otherLits) <- trigger rule i
  let (trgCohs, otherLits) = unzip trgCohs_otherLits
  mustHaveTrg <- liftIO $ orl' s (concatMap litsFromCohort trgCohs) --TODO
  mustHaveOther <- liftIO $ orl' s otherLits --TODO
  b <- liftIO $ solve s [allCondsHold]
  if b then do when verbose $
                 liftIO $ solveAndPrint True s [mustHaveTrg, mustHaveOther, allCondsHold] sen
               return NoConf
   else 
     do s' <- liftIO newSolver
        c <- local (withNewSolver s') $ do --tempSen <- mkSentence len
                                           --put (Config len tempSen)
                                           tempConf <- mkConfig len
                                           put tempConf
                                           tempSen <- gets sentence
                                           liftIO $ defaultRules s' tempSen
                                           (condsHold,trg_oth) <- trigger rule i
                                           let (trg,oth) = unzip trg_oth --TODO
                                           mustHaveTrg <- liftIO $ orl' s' (concatMap litsFromCohort trg)
                                           mustHaveOth <- liftIO $ orl' s' oth
                                           b <- liftIO $ solve s' [condsHold,mustHaveTrg,mustHaveOth]
                                           if b then do when verbose $ 
                                                          liftIO $ solveAndPrint True s' [condsHold,mustHaveTrg,mustHaveOth] tempSen
                                                        return Interaction
                                           else do when verbose $ do
                                                     liftIO $ print rule
                                                     liftIO $ solveAndPrint True s' [condsHold,mustHaveTrg,mustHaveOth] tempSen
                                                   return Internal
        liftIO $ deleteSolver s'
        put (Config len sen)
        return c

