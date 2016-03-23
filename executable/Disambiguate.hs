module Main where

import CG_parse ( readRules, readReadings, readData, parseData )
import CG_SAT
import CG_base -- ( Sentence, showSentence, Rule )
import SAT.Named
import SAT ( Solver(..), newSolver, deleteSolver )

import Control.Monad
import qualified Data.IntSet as IS
import qualified Data.Set as S
import System.Environment


main :: IO ()
main = do
  args <- getArgs
  case args of
    --["v", f1]  -> do rules <- concat `fmap` readRules f1
    --                 loop False (disambiguate False False rules)
    --["v", "d", f1]  -> do rules <- concat `fmap` readRules f1
    --                      loop True (disambiguate True True rules)
    (lang:txt:r) -> do
      let dirname = "data/" ++ lang ++ "/" 

      let grfile  = dirname ++ lang ++ ".rlx"
      --let tagfile = dirname ++ lang ++ ".tags"
      let rdsfile = dirname ++ lang ++ ".readings" 

      rules <- readRules grfile
      text <- readData txt
      allrds <- readReadings rdsfile 

                  --let verbose = "v" `elem` o
                  --    debug = "d" `elem` o
                  --    disam = if "noord" `elem` o 
                  --              then disambiguateUnordered verbose debug
                  --              else disambiguate verbose debug
                  --    disec = if "sec" `elem` o
                  --              then disamSection disam rules
                  --              else disam (concat rules)



      mapM_ (disambiguate allrds (concat rules)) text
    --("test":_) -> CG_SAT.test
    _          -> putStrLn "usage: ./Main (<rules> <data> | test) [v]"
  

loop :: Bool -> (Sentence -> IO Sentence) -> IO ()
loop debug f = do
    s <- parseData `fmap` getLine    
    disam <- mapM f s
    when (not debug) $ mapM_ (putStrLn . showSentence) disam --if debug, it will show already, no need to print it again
    loop debug f


--------------------------------------------------------------------------------

disambiguate :: [Reading] -> [Rule] -> Sentence -> IO Sentence
disambiguate allrds' rules sentence = do
  -- Don't bother disambiguating if not ambiguous
  if (all (not.isAmbig) sentence) then return sentence
   else do


  -- Pre-processing the nice Rule datatype to Rule'.
  -- Overkill here, but makes a difference with symbolic sentences.
    let allrds = concat sentence
    let alltags = S.toList . S.fromList . concat $ allrds
    let tagmap = mkTagMap alltags allrds
    let allinds = IS.fromList [1..length allrds]
    let rules' = map (ruleToRule' tagmap allinds) rules



    s <- newSolver
    satSentence <- mkSentence s allrds sentence 
    print satSentence
    return sentence



  




--------------------------------------------------------------------------------

