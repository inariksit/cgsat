module Main where


import CG_SAT
import Rule
import Parse ( parse )
import Utils

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

    let verbose = ("v" `elem` r || "d" `elem` r, "d" `elem` r)
    let subr = if "withsub" `elem` r then ".withsub" else ".nosub"
    let rdsfromgrammar = True --"undersp" `elem` r || "rdsfromgrammar" `elem` r
    let parseReading = if "withsub" `elem` r 
                        then Utils.parseReadingApeSubr
                        else Utils.parseReadingApe
 
    let dirname = "data/" ++ lang ++ "/" 
    let grfile  = dirname ++ lang ++ ".rlx"
    let tagfile = dirname ++ lang ++ ".tags"
    let lemfile = dirname ++ lang ++ ".lem"
    let wffile  = dirname ++ lang ++ ".wf"
    let rdsfile = dirname ++ lang ++ ".readings" --  ++ subr
    --let acfile  = dirname ++ lang ++ ".ambiguity-classes"
    --let frmfile = dirname ++ lang ++ ".formula"
    
    ----------------------------------------------------------------------------

    tagsInLex <- (map Utils.readTag . filter (not.null) . words) 
                   `fmap` readFile tagfile
    lemInLex <- (map readTag . filter (not.null) . words) `fmap` readFile lemfile
    wfInLex  <-  (map readTag . filter (not.null) . words) `fmap` readFile wffile

    readingsInLex <- (map parseReading . words) --Apertium format
                       `fmap` readFile rdsfile 
    (tsets,ruless) <- parse `fmap` readFile grfile
    let rules = filter (sel_or_rm . oper) (concat ruless)
    let readingsInGr = if rdsfromgrammar --OBS. will mess up ambiguity class constraints
                        then concatMap Utils.tagSet2Readings tsets --TODO filter all lexical items out
                        else []

--    print $ (length tagsInLex, take 50 tagsInLex)
--    print $ (length lemInLex, take 50 lemInLex)
    putStrLn "---------"

    print $ (length readingsInLex, take 50 readingsInLex)

    print $ (length readingsInGr, take 50 readingsInGr)
    putStrLn "---------"

    --mapM_ print (take 15 rules)
    putStrLn $ show (length rules) ++ " rules"

    putStrLn "---------"

    let env = mkEnv s (readingsInLex++readingsInGr) tagsInLex lemInLex wfInLex

    let largestWidth = maximum $ map (fst . width) rules
    (Right initSent,_,_) <- rwse env emptyConfig $ mkSentence largestWidth
    let config = Config initSent largestWidth

    (_,_,log_) <- rwse env config $ testRules (take 50 rules)

    mapM_ putStrLn log_

    putStrLn "---------"



   _ -> print "give me a 3-letter code for a language" 

sel_or_rm :: Oper -> Bool
sel_or_rm SELECT = True
sel_or_rm REMOVE = True
sel_or_rm _ = False

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

