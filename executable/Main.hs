module Main where

import CG_parse (parseData, parseRules)
import CG_SAT
import System.Environment


main :: IO ()
main = do
  args <- getArgs
  case args of
    [f1, f2]   -> do rules <- readFile f1 >>= parseRules
                     data' <- readFile f2 >>= parseData 
                     mapM_ (disambiguate False rules) data'
    ["-v", f1, f2]   -> do rules <- readFile f1 >>= parseRules
                           data' <- readFile f2 >>= parseData 
                           mapM_ (disambiguate True rules) data'

    ("test":_) -> CG_SAT.test
    _          -> putStrLn "usage: ./Main (<rules> <data> | test)"
