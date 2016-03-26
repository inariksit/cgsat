module Main where

import CG_parse ( readRules, readReadings, readData, parseData )
import CG_SAT
import CG_base -- ( Sentence, showSentence, Rule )
import SAT.Named
import SAT ( Solver(..), newSolver, deleteSolver )

import Control.Monad
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
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
    (dir:gr:txt:r) -> do
      let dirname = "data/" ++ dir ++ "/" 
      let grfile  = dirname ++ gr ++ ".rlx"
      let rdsfile = dirname ++ dir ++ ".readings"

      rules <- readRules grfile
      text <- readData $ dirname ++ txt
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
    initialSentence <- mkSentence' s allrds sentence
    finalSentence <- foldM (apply s) initialSentence rules'

    solveAndPrint True s [] initialSentence sentence
    defaultRules s finalSentence
    solveAndPrint True s [] finalSentence sentence

--    moreFinalSentence <- foldM (apply s) finalSentence rules'
--    mostFinalSentence <- foldM (apply s) moreFinalSentence rules'

    return sentence



 where
  defaultRules s sentence = 
   sequence_ [ do addClause s lits          --Every word must have >=1 reading
                  print lits
               | word <- IM.elems sentence 
               , let lits = IM.elems word ] 



solveAndPrint :: Bool -> Solver -> [Lit] -> Sentence' -> Sentence -> IO ()
solveAndPrint debug s ass satsent origsent = do

  putStrLn $ "Original sentence:\n" ++ showSentence origsent
  putStrLn "----"
  
  --let allLits = concat [ [ lit | lit <- IM.elems cohort' ] 
  --                       | cohort' <- IM.elems satsent ]
  --countAllLits <- count s allLits
  --b <- solveMaximize s ass countAllLits
  b <- solve s ass
  if b then do
          vals <- sequence 
                   [ sequence [ modelValue s lit | lit <- IM.elems word ] 
                      | word <- IM.elems satsent ]
          let trueRds = if debug 
               then
                [ unlines [ showReading rd  ++ "\n\t" ++ show rd'
                            | (rd, rd', True) <- zip3 cohort (IM.elems cohort') vs ]
                  | (cohort, cohort', vs) <- zip3 origsent (IM.elems satsent) vals ]
               else
                [ showCohort [ rd | (rd, True) <- zip cohort vs ]
                  | (cohort, vs) <- zip origsent vals ]

          mapM_ putStrLn trueRds
          putStrLn "----"
      else do
        putStrLn $ "solveAndPrintSentence: Conflict with assumptions " ++ show ass
--------------------------------------------------------------------------------

