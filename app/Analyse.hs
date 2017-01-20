module Main where


import CG_SAT
import Rule
import Parse ( parse )
import Utils

import SAT ( Solver(..), newSolver, deleteSolver )
import SAT.Named

import Data.Foldable ( fold )
import qualified Data.IntMap as IM
import Data.List ( nub, elemIndex )
import Data.Maybe ( catMaybes, fromMaybe )
import Debug.Trace ( trace )
import System.Environment ( getArgs )

--maybe remove these (and things that need them) to CG_SAT?
import Control.Monad.Trans ( liftIO )
import Control.Monad.State.Class ( put, gets )
import Control.Monad.Reader.Class ( asks )
import Control.Monad.Trans.State ( evalStateT )
import Control.Monad.Trans.Reader ( runReaderT )

--------------------------------------------------------------------------------

data Conflict = TODO

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

    print $ (length tagsInLex, take 50 tagsInLex)
    print $ (length lemInLex, take 50 lemInLex)
    putStrLn "---------"

    print $ (length readingsInLex, take 50 readingsInLex)

    print $ (length readingsInGr, take 50 readingsInGr)
    putStrLn "---------"

    mapM_ print (take 15 rules)

    putStrLn "---------"

    let env = mkEnv s (readingsInLex++readingsInGr) (tagsInLex++lemInLex)
--    evalStateT (runReaderT ( runRSIO $ testRule (rules !! 13) (take 12 rules) ) 
    evalStateT (runReaderT ( runRSIO $ testRule (last rules) rules ) 
                           env) 
               emptyConf
    putStrLn "---------"



   _ -> print "give me a 3-letter code for a language" 


----------------------------------------------------------------------------
-- Functions that apply only for analysis, not disambiguation


testRule :: Rule -> [Rule] -> RSIO Conflict
testRule rule prevRules = do 
  let (w,trgCohInd) = width rule
  initSent <- mkSentence w
  s <- asks solver
  tm <- asks tagMap 

  liftIO $ defaultRules s initSent
  put (Conf initSent w)

  liftIO $ print rule
  liftIO $ print w

  ps "--------"

  mapM_ apply prevRules

  newSent <- gets sentence
  --liftIO $ print newSent

 --TODO: less copypaste, merge some of this with CG_SAT.apply
  let shouldTriggerLast sent conds = do
        let trgCoh = fromMaybe (error "shouldTriggerLast: no trg index found") 
                               (IM.lookup trgCohInd sent)
        let trgIS = fromMaybe (error "shouldTriggerLast: no trg tagset found")
                              (normaliseTagsetAbs (target rule) tm)
        let (inmap,outmap) = partitionCohort trgIS trgCoh
        let (trg,oth) = case oper rule of
                               SELECT -> (outmap,inmap)
                               REMOVE -> (inmap,outmap)
                               _   -> (inmap,outmap) --TODO other operations

        mht <- orl' s (IM.elems trg) -- 1) must have ≥1 target lits
        mho <- orl' s (IM.elems oth) -- 2) must have ≥1 other lits                              
        ach <- andl' s (getAndList conds) -- 3) all conditions must hold
        return (mht, mho, ach)

  condlits <- fromMaybe (error "testRule: conditions not applicable") 
                 `fmap` condLits rule trgCohInd

  (mustHaveTrg, mustHaveOther, allCondsHold) <- liftIO $ shouldTriggerLast newSent condlits

  b <- liftIO $ solve s [mustHaveTrg, mustHaveOther, allCondsHold]
  liftIO $ print b

  return TODO


 where
  ps = liftIO . putStrLn


width :: Rule -> (Int,Int)
width rule = (length [minw..maxw], maybe 9999 (1+) (elemIndex 0 [minw..maxw]))
 where                                   
  ctxScopes = fmap scopes (context rule) :: AndList (OrList Int) -- And [Or [1], Or [1,2,3], Or [-2,-1]]
  flatScopes = fold (getAndList ctxScopes) :: OrList Int -- Or [1,1,2,3,-2,-1]
  (minw,maxw) = (0 `min` minimum flatScopes, 0 `max` maximum flatScopes)

