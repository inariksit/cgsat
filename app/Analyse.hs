module Main where

import Rule
import Parse ( parse )
import CG_SAT

import SAT ( Solver(..), newSolver, deleteSolver )
import SAT.Named

import System.Environment ( getArgs )

--------------------------------------------------------------------------------

data Conflict = TODO

--------------------------------------------------------------------------------

main :: IO ()
main = do 
  args <- getArgs
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
    print (take 5 rules)

    putStrLn "---------"
    testRule (rules !! 10) (take 10 rules)
    putStrLn "---------"



   _ -> print "give me a 3-letter code for a language" 


----------------------------------------------------------------------------
-- Functions that apply only for analysis, not disambiguation


testRule :: Rule -> [Rule] -> IO Conflict --CGMonad Conflict
testRule rule prevRules = do print rule
                             print rulewidths
                             return TODO
 where rulewidths = fmap width (context rule)


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