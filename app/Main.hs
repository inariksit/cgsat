module Main where

import CGSAT ( rwse, evalRWSE, mkConfig, emptyConfig, envRules, dummyGenerate, apply, width )
import Analyse ( testRules )
import Order ( order, howmanyReadings, checkByTarget, feedingOrder )
import SAT ( newSolver )

import System.Environment ( getArgs )
import Control.Monad ( forM_ )
import Data.List ( delete, elemIndex )


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
          let rls = take 20 rules
          (_,_,log_) <- rwse env config $ testRules verbose rls

          mapM_ putStrLn log_

          putStrLn "---------"

      "models" -> do
          let rls = take 4 rules
          mapM_ (\x -> rwse env config $ howmanyReadings [x]) rls
          putStrLn "end"
          --(b,_,_) <- rwse env config $ howmanyReadings (reverse rls)
          --print b

      "order" -> do          
          let fewRules = take 10 rules
          (a,_,_) <- rwse env emptyConfig $ order fewRules
          let rls = either (const []) id a
          putStrLn "Original order: "
          mapM_ print fewRules
          putStrLn "New order: "
          mapM_ print rls

      "feed" -> do
          let splitrls = splits $ take 3 rules
          forM_ splitrls $ \(r,rs) ->
            do (a,_,_) <- rwse env emptyConfig $ feedingOrder r rs
               let result = either (const []) id a
               putStrLn ("Rule: " ++ show r)
               putStrLn "Rules that feed to it:"
               let len = length result
               print len
               if len < 50
                then mapM_ print result
                else return ()

      "check" -> do
          let (from:to:_) = map read r :: [Int]
          _ <- rwse env config $ checkByTarget rules from to
          putStrLn "done"

      "dummy" -> do
          evalRWSE env (dummyGenerate rules)

      _ -> putStrLn "I don't do that yet"

   _ -> print "give me a 3-letter code for a language" 


splits :: (Eq a) => [a] -> [(a,[a])]
splits xs = xs `for` \x -> let Just ind = elemIndex x xs
                              in  (x, delete x xs)
                              --in  (x, take ind xs)

for = flip fmap