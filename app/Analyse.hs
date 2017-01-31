module Main where


import CG_SAT
import Rule
import CghsUtils
import CgsatUtils

import SAT ( Solver(..), newSolver, deleteSolver )
import SAT.Named

import Data.Foldable ( fold )
import qualified Data.IntMap as IM
import Data.Either ( lefts, rights )
import Data.List ( nub, elemIndex )
import Data.Maybe ( catMaybes, fromMaybe )
import Debug.Trace ( trace )
import System.Environment ( getArgs )

--maybe remove these (and things that need them) to CG_SAT?
import Control.Monad ( forM, when )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Except ( runExceptT, catchError, throwError, lift )
import Control.Monad.RWS ( runRWST, get, gets, put, ask, asks, local )

--------------------------------------------------------------------------------

data Conflict = NoConf | Internal | Interaction -- [Rule]
  deriving (Show,Eq)

--------------------------------------------------------------------------------

main :: IO ()
main = do 
  args <- getArgs
  s <- newSolver

  case args of 
   (lang:r) -> do 

     (env,rules) <- liftIO $ envRules (lang,r) s
     let largestWidth = maximum $ map (fst . width) rules
     (Right initSent,_,_) <- rwse env emptyConfig $ mkSentence largestWidth
     let config = Config initSent largestWidth

     (_,_,log_) <- rwse env config $ testRules (take 50 rules)

     mapM_ putStrLn log_

     putStrLn "---------"



   _ -> print "give me a 3-letter code for a language" 

----------------------------------------------------------------------------
-- Functions that apply only for analysis, not disambiguation

testRules :: [Rule] -> RWSE [Conflict]
testRules = mapM testRule


testRule :: Rule -> RWSE Conflict
testRule rule = do
  c@(Config sen len) <- get
  e@(Env tm _rm _ _ s) <- ask
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
  (mustHaveTrg, mustHaveOther, allCondsHold,_,_) <- trigger rule i
  s <- asks solver
  (Config sen len) <- get
  b <- liftIO $ solve s [mustHaveTrg, mustHaveOther, allCondsHold]
  if b then do when verbose $
                 liftIO $ solveAndPrint True s [mustHaveTrg, mustHaveOther, allCondsHold] sen
               return NoConf
   else 
     do s' <- liftIO newSolver
        c <- local (withNewSolver s') $ do tempSen <- mkSentence len
                                           put (Config tempSen len)
                                           (x,y,z,_,_) <- trigger rule i
                                           b <- liftIO $ solve s' [x,y,z]
                                           if b then do when verbose $ 
                                                          liftIO $ solveAndPrint True s' [x,y,z] tempSen
                                                        return Interaction
                                           else do when verbose $ do
                                                     liftIO $ print rule
                                                     liftIO $ solveAndPrint True s' [x,y,z] tempSen
                                                   return Internal
        liftIO $ deleteSolver s'
        put (Config sen len)
        return c


width :: Rule -> (Int,Int)
width rule = (length [minw..maxw], maybe 9999 (1+) (elemIndex 0 [minw..maxw]))
 where                                   
  ctxScopes = fmap scopes (context rule) :: AndList (OrList Int) -- And [Or [1], Or [1,2,3], Or [-2,-1]]
  flatScopes = fold (getAndList ctxScopes) :: OrList Int -- Or [1,1,2,3,-2,-1]
  (minw,maxw) = (0 `min` minimum flatScopes, 0 `max` maximum flatScopes)
