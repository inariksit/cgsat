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
import Data.Either ( lefts, rights )
import Data.List ( nub, elemIndex )
import Data.Maybe ( catMaybes, fromMaybe )
import Debug.Trace ( trace )
import System.Environment ( getArgs )


--------------------------------------------------------------------------------

data Conflict = NoConf | Internal | Interaction -- [Rule]
  deriving (Show,Eq)


----------------------------------------------------------------------------
-- Functions that apply only for analysis, not disambiguation

testRules :: [Rule] -> RWSE [Conflict]
testRules = mapM testRule


testRule :: Rule -> RWSE Conflict
testRule rule = do
  c@(Config len sen) <- get
  e@(Env w l r s) <- ask
  confs <-    -- I suspect there's a nicer way to handle this
     mapM (\i -> RWSE $ lift $ runExceptT $ runRWSE $ ruleTriggers False rule i
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
  (allCondsHold, trgAndOthers) <- trigger rule i
  mustHaveTrg <- liftIO $ orl' s [true] -- TODO: excract lits from TargetCohort
  mustHaveOther <- liftIO $ orl' s (map snd trgAndOthers)
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
                                           --(x,y,z,_,_) <- trigger rule i
                                           let (x,y,z) = (true,true,true)
                                           b <- liftIO $ solve s' [x,y,z]
                                           if b then do when verbose $ 
                                                          liftIO $ solveAndPrint True s' [x,y,z] tempSen
                                                        return Interaction
                                           else do when verbose $ do
                                                     liftIO $ print rule
                                                     liftIO $ solveAndPrint True s' [x,y,z] tempSen
                                                   return Internal
        liftIO $ deleteSolver s'
        put (Config len sen)
        return c


width :: Rule -> (Int,Int)
width rule = (length [minw..maxw], maybe 9999 (1+) (elemIndex 0 [minw..maxw]))
 where                                   
  ctxScopes = fmap scopes (context rule) :: AndList (OrList Int) -- And [Or [1], Or [1,2,3], Or [-2,-1]]
  flatScopes = fold (getAndList ctxScopes) :: OrList Int -- Or [1,1,2,3,-2,-1]
  (minw,maxw) = (0 `min` minimum flatScopes, 0 `max` maximum flatScopes)
