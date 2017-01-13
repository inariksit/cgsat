module Main where


import CG_SAT
import Rule
import Parse ( parse )
import Utils

import SAT ( Solver(..), newSolver, deleteSolver )
import SAT.Named

import Data.Foldable ( fold )
import Debug.Trace ( trace )
import System.Environment ( getArgs )

--maybe remove these (and things that need them) to CG_SAT?
import Control.Monad.Trans ( liftIO )
import Control.Monad.State.Class ( put )
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
    let subr = if "nosub" `elem` r then ".nosub" else ".withsub"
    let rdsfromgrammar = "undersp" `elem` r || "rdsfromgrammar" `elem` r
 
    let dirname = "data/" ++ lang ++ "/" 
    let grfile  = dirname ++ lang ++ ".rlx"
    let tagfile = dirname ++ lang ++ ".tags"
    let rdsfile = dirname ++ lang ++ ".readings" ++ subr
    --let acfile  = dirname ++ lang ++ ".ambiguity-classes"
    --let frmfile = dirname ++ lang ++ ".formula"
    
    ----------------------------------------------------------------------------

    tagsInLex <- (map toTag . filter (not.null) . words) 
                   `fmap` readFile tagfile
    readingsInLex <- (map parseReading . words) `fmap` readFile rdsfile
    rules <- (concat . snd . parse) `fmap` readFile grfile
    --let readingsInGr = if rdsfromgrammar --OBS. will mess up ambiguity class constraints
    --                    then nub $ concatMap toTags' tsets
    --                    else []

    print tagsInLex
    print readingsInLex
    mapM_ print (take 5 rules)

    putStrLn "---------"

    let env = mkEnv s readingsInLex tagsInLex
    evalStateT (runReaderT (runRSIO (testRule (head rules) [])) 
                           env) 
               emptySent
    putStrLn "---------"



   _ -> print "give me a 3-letter code for a language" 


----------------------------------------------------------------------------
-- Functions that apply only for analysis, not disambiguation


testRule :: Rule -> [Rule] -> RSIO Conflict -- ReaderT Env (StateT Sentence IO) Conflict
testRule rule prevRules = do 
  liftIO $ print rule
  liftIO $ print (width rule)
  sent <- mkSentence (width rule)
  put sent
  tm <- asks tagMap 
  liftIO $ print sent
  liftIO $ print tm 
  return TODO


width :: Rule -> Int
width rule = length [minw..maxw]
 where                                   
  ctxScopes = fmap scopes (context rule) :: AndList (OrList Int) -- And [Or [1], Or [1,2,3], Or [-2,-1]]
  flatScopes = fold (getAndList ctxScopes) :: OrList Int -- Or [1,1,2,3,-2,-1]
  (minw,maxw) = (0 `min` minimum flatScopes, 0 `max` maximum flatScopes)

----------------------------------------------------------------------------
-- Helper functions, TODO move these to someplace more general

-- This is for Apertium format, TODO find out what the basque grammar uses
parseReading :: String -> Reading
parseReading str = And $ maintags ++ concat subtags
 where
  (mainr:subrs) = split (=='+') str
  maintags = map toTag $ filter (not.null) $ split isValid mainr
  subrs_ns = map FromStart [1..] `zip` map (split isValid) subrs :: [(Subpos,[String])]
  subtags = map (\(n, strs) -> map (Subreading n . toTag) strs) subrs_ns
  isValid = (=='<') 


toTag ">>>" = BOS
toTag "<<<" = EOS
toTag []    = error "empty tag"
toTag str | last str == '>' = Tag (init str)
          | last str == '$' = WF (init str)
          | otherwise       = Lem str

split :: (a -> Bool) -> [a] -> [[a]]
split p [] = []
split p xs = takeWhile (not . p) xs : split p (drop 1 (dropWhile (not . p) xs))