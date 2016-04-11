module Main where

import CG_parse ( readRules, readReadings, readData, parseData )
import CG_SAT
import CG_base -- ( Sentence, showSentence, Rule )
import SAT.Named
import SAT ( Solver(..), newSolver, deleteSolver )
import SAT.Unary ( Unary )
import CG_eval

import Control.Monad
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Data.Set as S
import System.Environment

spa20k = "20k.tagged.ambiguous"
spa20kgold = "20k.tagged"


main :: IO ()
main = do
  args <- getArgs
  case args of
    --["v", f1]  -> do rules <- concat `fmap` readRules f1
    --                 loop False (disambiguate False False rules)
    --["v", "d", f1]  -> do rules <- concat `fmap` readRules f1
    --                      loop True (disambiguate True True rules)
    (dir:gr:txt:r) -> do
      let parallel = "par" `elem` r
      let test = "test" `elem` r

      let dirname = "data/" ++ dir ++ "/" 
      let grfile  = dirname ++ gr ++ ".rlx"
      let rdsfile = dirname ++ dir ++ ".readings"
      let txtfile = dirname ++ if test then spa20k else txt


      let disam = if parallel then disambiguateParallel else disambiguateParallel

      rules <- map reverse `fmap` readRules grfile
      text <- readData txtfile
      allrds <- readReadings rdsfile 

                  --let verbose = "v" `elem` o
                  --    debug = "d" `elem` o
                  --    disam = if "noord" `elem` o 
                  --              then disambiguateUnordered verbose debug
                  --              else disambiguate verbose debug
                  --    disec = if "sec" `elem` o
                  --              then disamSection disam rules
                  --              else disam (concat rules)


--      print rules
      resSAT <- mapM (disam allrds (concat rules)) text
      if test 
       then do gold <- readData $ dirname ++ spa20kgold
               resVISL <- vislcg3 grfile txtfile True
               putStrLn "SAT-CG in comparison to gold standard"
               let verbose = length text < 1 --change if you want different output
               prAll "SAT" resSAT gold text verbose
               putStrLn "\nVISLCG3 in comparison to gold standard"
               prAll "VISL" resVISL gold text verbose
               putStrLn ""
       else do mapM_ (putStrLn . showSentence) resSAT


    _          -> putStrLn "usage: ./Main (<rules> <data> | test) [v]"
  

loop :: Bool -> (Sentence -> IO Sentence) -> IO ()
loop debug f = do
    s <- parseData `fmap` getLine    
    disam <- mapM f s
    when (not debug) $ mapM_ (putStrLn . showSentence) disam --if debug, it will show already, no need to print it again
    loop debug f


--------------------------------------------------------------------------------

--so far just terrible copypaste, will fix later
disambiguateParallel :: [Reading] -> [Rule] -> Sentence -> IO Sentence
disambiguateParallel  allrds' rules sentence = do
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
    satSentence <- mkSentence' s allrds sentence
    defaultRules s satSentence

    clausesRaw <- mapM (applyParallel s satSentence) rules'
    (helperLits, clauses)
      <- unzip `fmap` sequence 
          [ do b <- newLit s "" 
               addClause s (neg b:cl) --- ?
               return (b, neg b:cl)
            | cls <- clausesRaw
            , cl <- cls ]

    --b <- do k <- count s helperLits :: IO Unary
    --        solveMaximize s [] k
    b <- solve s []
    if b then do
      newSentence <- toSentence s satSentence sentence
      --solveAndPrint True s [] satSentence sentence
      --putStrLn $ showSentence newSentence
      return newSentence
    else do
      putStrLn "No solution :("
      return sentence




------------------------------------------------------------

--This one starts off as all variables True--ie. no search/decision,
--just manipulate Boolean expressions. Should behave like VISL CG-3.
disambiguateNotActuallySAT :: [Reading] -> [Rule] -> Sentence -> IO Sentence
disambiguateNotActuallySAT allrds' rules sentence = do
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
    allTrue s initialSentence

    finalSentence <- foldM (apply s) initialSentence rules'
    defaultRules s finalSentence
    --moreFinalSentence <- foldM (apply s) finalSentence rules'
    --mostFinalSentence <- foldM (apply s) moreFinalSentence rules'

    newSentence <- toSentence s finalSentence sentence

    return newSentence
 where
  allTrue :: Solver -> Sentence' -> IO ()
  allTrue s sentence = 
    sequence_ [ do mapM (\x -> addClause s [x]) lits 
                | word <- IM.elems sentence 
                , let lits = IM.elems word ] 

---------------------------------------------------------------

defaultRules :: Solver -> Sentence' -> IO ()
defaultRules s sentence = 
   sequence_ [ do addClause s lits --Every word must have >=1 reading
                  --print lits
               | word <- IM.elems sentence 
               , let lits = IM.elems word ] 

toSentence :: Solver -> Sentence' -> Sentence -> IO Sentence
toSentence s satsent origsent = do
  --b <- solve s ass
  --if b then do
  vals <- sequence 
               [ sequence [ modelValue s lit | lit <- IM.elems word ] 
                 | word <- IM.elems satsent ]
  return $ [ [ rd | (rd, True) <- zip cohort vs ]
                  | (cohort, vs) <- zip origsent vals ]
  --else return origsent


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
          let trueRds = if False --debug 
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

