module Main where

import CG_parse (readRules, readData)
import CG_SAT
import Control.Monad
import System.Environment


main :: IO ()
main = do
  args <- getArgs
  case args of
    [f1, f2]   -> do rules <- readRules f1
                     data' <- readData f2 
                     mapM_ (disambiguate False rules) data'
    [f1, f2, "v"]   -> do rules <- readRules f1
                          data' <- readData f2 
                          mapM_ (disambiguate True rules) data'

    ("test":_) -> CG_SAT.test
    _          -> putStrLn "usage: ./Main (<rules> <data> | test) [v]"
