module Main where

import CG_parse (readRules, readData, parseData)
import CG_SAT
import CG_base ( Sentence, showSentence )
import Control.Monad
import System.Environment


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["v", f1]  -> do rules <- readRules f1
                     loop (disambiguate False False rules)
    [f1, f2]   -> do rules <- readRules f1
                     data' <- readData f2 
                     mapM_ (disambiguate False False rules) data'
    [f1, f2, "v"]   -> do rules <- readRules f1
                          data' <- readData f2 
                          mapM_ (disambiguate True True rules) data'
    ("test":_) -> CG_SAT.test
    _          -> putStrLn "usage: ./Main (<rules> <data> | test) [v]"



loop :: (Sentence -> IO Sentence) -> IO ()
loop f = do
    s <- parseData `fmap` getLine
    disam <- mapM f s
    mapM_ (putStrLn . showSentence) disam
    loop f
