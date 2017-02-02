module Main where

import CGSAT ( rwse, evalRWSE, mkConfig, envRules, dummyTest )
import Analyse ( width, testRules )
import SAT ( newSolver )

import System.Environment ( getArgs )


--------------------------------------------------------------------------------

main :: IO ()
main = do 
  args <- getArgs
  s <- newSolver

  case args of 
   ("dummy":_) -> do
     (env,_) <- envRules ("eus",[]) s
     evalRWSE env dummyTest

   (lang:r) -> do 
     (env,rules) <- envRules (lang,r) s
     let largestWidth = maximum $ map (fst . width) rules
     config <- evalRWSE env (mkConfig largestWidth)

     (_,_,log_) <- rwse env config $ testRules (take 50 rules)

     mapM_ putStrLn log_

     putStrLn "---------"

     -- Only does analysis so far.
     -- When Disambiguate is ready, add command line option to choose.


   _ -> print "give me a 3-letter code for a language" 


