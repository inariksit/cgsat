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
import Control.Monad ( forM )
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
   (lang:r)-> do 

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
    let rdsfile = dirname ++ lang ++ ".readings" --  ++ subr
    --let acfile  = dirname ++ lang ++ ".ambiguity-classes"
    --let frmfile = dirname ++ lang ++ ".formula"
    
    ----------------------------------------------------------------------------

    tagsInLex <- (map Utils.readTag . filter (not.null) . words) 
                   `fmap` readFile tagfile
    lemInLex <- (map readTag . filter (not.null) . words) `fmap` readFile lemfile
    readingsInLex <- (map parseReading . words) --Apertium format
                       `fmap` readFile rdsfile 
    (tsets,ruless) <- parse `fmap` readFile grfile
    let rules = concat ruless
    let readingsInGr = if rdsfromgrammar --OBS. will mess up ambiguity class constraints
                        then concatMap Utils.tagSet2Readings tsets
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

    let env = mkEnv s (readingsInLex++readingsInGr) (tagsInLex++lemInLex)

    let largestWidth = maximum $ map (fst . width) rules
    (Right initSent,_,_) <- rwse env emptyConfig $ mkSentence largestWidth
    let config = Config initSent largestWidth

    (_,_,log_) <- rwse env config $ testRules rules

    mapM_ putStrLn log_

    putStrLn "---------"



   _ -> print "give me a 3-letter code for a language" 

splits :: (Eq a) => [a] -> [(a,[a])]
splits xs = xs `for` \x -> let Just ind = elemIndex x xs
                           in  (x, take ind xs)
 
for = flip fmap

----------------------------------------------------------------------------
-- Functions that apply only for analysis, not disambiguation

testRules :: [Rule] -> RWSE [Conflict]
testRules = mapM testRule


testRule :: Rule -> RWSE Conflict
testRule rule = do
  c@(Config sen len) <- get
  e@(Env tm _rm s) <- ask
  confs <-    -- I suspect there's a nicer way to handle this
     mapM (\i -> RWSE $ lift $ runExceptT $ runRWSE $ ruleTriggers rule i
                  :: RWSE (Either CGException Conflict) )
                [1..len] -- 1) Test if the rule may apply, and return result of that

  apply rule             -- 2) Apply the rule regardless


  let legitConfs = rights confs
  liftIO $ print legitConfs
  if Interaction `elem` legitConfs 
    then return Interaction
    else if Internal `elem` legitConfs || null legitConfs
      then return Internal
      else return NoConf


ruleTriggers :: Rule -> Int -> RWSE Conflict
ruleTriggers rule i = do
  (mustHaveTrg, mustHaveOther, allCondsHold,_,_) <- trigger rule i
  s <- asks solver
  (Config sen len) <- get
  b <- liftIO $ solve s [mustHaveTrg, mustHaveOther, allCondsHold]
  if b then return NoConf
   else 
     do s' <- liftIO newSolver
        c <- local (withNewSolver s') $ do sent' <- mkSentence len
                                           put (Config sent' len)
                                           apply rule
                                           (x,y,z,_,_) <- trigger rule i
                                           b <- liftIO $ solve s' [x,y,z]
                                           if b then return Internal
                                           else return (Interaction)
        liftIO $ deleteSolver s'
        return c


testRule' :: Maybe Int -> Rule -> [Rule] -> RWSE (Conflict, Maybe Int)
testRule' prevWid rule prevRules = do 
  let (w,trgCohInd) = width rule
  let sameWidth = case prevWid of
                    Nothing -> False
                    Just wid -> wid >= w 
  initSent <- if sameWidth then ps "reused the same sentence!" >> gets sentence
                            else mkSentence w

  initSent <- gets sentence
  s <- asks solver
  tm <- asks tagMap 

  liftIO $ defaultRules s initSent
  put (Config initSent w)

  liftIO $ print (rule,"width of rule: " ++ show w, "target ind: " ++ show trgCohInd)

  ps "--------"

  if sameWidth
    then do ps "applied only last rule!" 
            if null prevRules
              then ps "empty prevRules, alert!" >> return ()
              else apply (last prevRules)
    else mapM_ apply prevRules

  conf <- ruleTriggers rule trgCohInd
  return (conf, Just w)






 where
  ps = liftIO . putStrLn


width :: Rule -> (Int,Int)
width rule = (length [minw..maxw], maybe 9999 (1+) (elemIndex 0 [minw..maxw]))
 where                                   
  ctxScopes = fmap scopes (context rule) :: AndList (OrList Int) -- And [Or [1], Or [1,2,3], Or [-2,-1]]
  flatScopes = fold (getAndList ctxScopes) :: OrList Int -- Or [1,1,2,3,-2,-1]
  (minw,maxw) = (0 `min` minimum flatScopes, 0 `max` maximum flatScopes)

