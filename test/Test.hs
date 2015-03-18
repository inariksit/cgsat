module Main where

import CG_base
import CG_parse
import CG_SAT
import Control.Monad
import Data.List
import Data.Maybe
import System.Environment
import System.IO
import System.Process

main :: IO ()
main = do
  args <- getArgs
  case args of
    [f1, f2] -> do rules <- readRules f1
                   data' <- readData f2
                   result <- mapM (disambiguate False rules) data'
                   goldst <- gold f1 f2
                   let diff = [ (showSent r, showSent g) 
                                 | (r,g) <- zip result goldst, r/=g ]
                   mapM_ pr diff
                   print (length result, length goldst)
                   putStr "Sentences that differ from vislcg3: "
                   print $ length diff
                   --print [diffByWord r g | (r,g) <- zip result goldst, r/=g]
    _        -> do putStrLn "usage: ./test <rules> <data>"
  where showSent   = map showAnalysis
        pr (rs,gs) = do putStrLn "\nResult by CG-SAT"
                        mapM_ putStrLn rs
                        putStrLn "\nGold standard"
                        mapM_ putStrLn gs

diffByWord :: Sentence -> Sentence -> [(Analysis,Analysis)]
diffByWord s1 s2 = [ (a1, a2) | (a1, a2) <- zip s1 s2, a1/=a2 ] 

gold :: FilePath -> FilePath -> IO [Sentence]
gold rls dt = do
  (_, Just out1, _, _) <-
      createProcess (proc "cat" [dt]){std_out=CreatePipe}
  (_, Just out2, _, _) <- 
      createProcess (proc "cg-conv" ["-a"]){std_in=UseHandle out1
                                          , std_out=CreatePipe}
  (_, Just out3, _, _) <- 
      createProcess (proc "vislcg3" ["-2", "-g", rls]){std_in=UseHandle out2
                                                     , std_out=CreatePipe}
  (_, Just out4, _, _) <- 
      createProcess (proc "cg-conv" ["-A"]){std_in=UseHandle out3
                                          , std_out=CreatePipe}

  result <- hGetContents' out4
  mapM_ hClose [out1,out2,out3,out4]
  return $ map (filter (not.null)) $ parseData result


-- Strict hGetContents
hGetContents' :: Handle -> IO String
hGetContents' hdl = do e <- hIsEOF hdl
                       if e then return []
                            else do c <- hGetChar hdl
                                    cs <- hGetContents' hdl
                                    return (c:cs)