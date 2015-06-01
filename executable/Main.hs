module Main where

import CG_parse (readRules, readData, parseData)
import CG_SAT
import CG_base ( Sentence, showSentence, Rule )
import Control.Monad
import System.Environment


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["v", f1]  -> do rules <- concat `fmap` readRules f1
                     loop False (disambiguateWithOrder False False rules)
    ["v", "d", f1]  -> do rules <- concat `fmap` readRules f1
                          loop True (disambiguateWithOrder True True rules)
    (r:d:o) -> do rules <- readRules r
                  text <- readData d
                  let is2 = "2" `elem` o
                      verbose = "v" `elem` o
                      debug = "d" `elem` o
                      disam = if "noord" `elem` o 
                                then disambiguate verbose debug
                                else disambiguateWithOrder verbose debug
                      disec = if "nosec" `elem` o
                                then disam (concat rules)
                                else disamSection disam rules
                  mapM_ disec text
    ("test":_) -> CG_SAT.test
    _          -> putStrLn "usage: ./Main (<rules> <data> | test) [v]"
  

loop :: Bool -> (Sentence -> IO Sentence) -> IO ()
loop debug f = do
    s <- parseData `fmap` getLine    
    disam <- mapM f s
    when (not debug) $ mapM_ (putStrLn . showSentence) disam --if debug, it will show already, no need to print it again
    loop debug f
