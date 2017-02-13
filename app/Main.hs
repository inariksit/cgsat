module Main where

import CGSAT ( rwse, evalRWSE, mkConfig, emptyConfig, envRules, dummyGenerate, apply, width )
import Analyse ( testRules )
import Order ( order, howmanyReadings )
import SAT ( newSolver )

import System.Environment ( getArgs )


--------------------------------------------------------------------------------

main :: IO ()
main = do 
  args <- getArgs
  s <- newSolver

  case args of 

   (lang:task:r) -> do 
     (env,rules) <- envRules (lang,r) s
     let largestWidth = maximum $ map (fst . width) rules
     config <- evalRWSE env (mkConfig largestWidth)     
     let verbose = "v" `elem` r
--     let verbose = True

     case task of
      "analyse" -> do 
          (_,_,log_) <- rwse env config $ testRules verbose (take 2 rules)

          mapM_ putStrLn log_

          putStrLn "---------"

      "models" -> do
          (a,_,_) <- rwse env config $ howmanyReadings (take 20 rules)
          print a

      "order" -> do          
          let fewRules = take 10 rules
          (a,_,_) <- rwse env emptyConfig $ order fewRules
          let rls = either (const []) id a
          putStrLn "Original order: "
          mapM_ print fewRules
          putStrLn "New order: "
          mapM_ print rls

      "dummy" -> do
          evalRWSE env (dummyGenerate rules)

      _ -> putStrLn "I don't do that yet"

   _ -> print "give me a 3-letter code for a language" 


