module Main where

import CG_parse ( readRules, readData, parseData )
import CG_SAT
import qualified CG_simple as Simple
import CG_base ( Sentence, showSentence, Rule )
import Control.Monad
import System.Environment


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["v", f1]  -> do rules <- concat `fmap` readRules f1
                     loop False (disambiguate False False rules)
    ["v", "d", f1]  -> do rules <- concat `fmap` readRules f1
                          loop True (disambiguate True True rules)
    (r:d:o) -> do rules <- readRules r
                  text <- readData d
                  let verbose = "v" `elem` o
                      debug = "d" `elem` o
                      disam = if "noord" `elem` o 
                                then disambiguateUnordered verbose debug
                                else disambiguate verbose debug
                      disec = if "sec" `elem` o
                                then disamSection disam rules
                                else disam (concat rules)
                      dirul = if "s" `elem` o || "simple" `elem` o
                              --  then Simple.disamRule (concat rules)
                                then Simple.disamSecRule rules
                                else disec
                  mapM_ dirul text
    ("test":_) -> CG_SAT.test
    _          -> putStrLn "usage: ./Main (<rules> <data> | test) [v]"
  

loop :: Bool -> (Sentence -> IO Sentence) -> IO ()
loop debug f = do
    s <- parseData `fmap` getLine    
    disam <- mapM f s
    when (not debug) $ mapM_ (putStrLn . showSentence) disam --if debug, it will show already, no need to print it again
    loop debug f
