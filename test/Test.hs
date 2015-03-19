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
    [r,d]      -> go r d False False
    [r,d,"-2"] -> go r d False True
    [r,d,"-v"] -> go r d True False
    [r,d,"-v", "-2"] -> go r d True True
    _        -> do putStrLn "usage: ./test <rules> <data>"
  where go r d v is2 = do rules <- readRules r
                          data' <- readData d
                          result <- mapM (disambiguate False rules) data'
                          goldst <- gold r d is2
                          let diff = [ (diffByWord r g orig)
                                       | (r,g,orig) <- zip3 result goldst data', r/=g ]
                          when v $ (mapM_ . mapM_) prDiff diff
                          print (length result, length goldst)
                          putStr "Sentences that differ from vislcg3: "
                          print $ length diff
        prDiff (a1,a2,s) = do putStrLn "Original sentence:"
                              putStrLn (showSentence s)
                              putStrLn "\nDisambiguation by CG-SAT"
                              putStrLn $ showAnalysis a1
                              putStrLn "\nDisambiguation by vislcg3"
                              putStrLn $ showAnalysis a2
                              putStrLn "---------------\n"

diffByWord :: Sentence -> Sentence -> Sentence -> [(Analysis,Analysis,Sentence)]
diffByWord s1 s2 orig = [ (a1, a2, orig) | (a1, a2) <- zip s1 s2, a1/=a2 ] 

gold :: FilePath -> FilePath -> Bool -> IO [Sentence]
gold rls dt isCG2 = do
  let cg2 = if isCG2 then "-2" else ""
  (_, Just out1, _, _) <-
      createProcess (proc "cat" [dt]){std_out=CreatePipe}
  (_, Just out2, _, _) <- 
      createProcess (proc "cg-conv" ["-a"]){std_in=UseHandle out1
                                          , std_out=CreatePipe}
  (_, Just out3, _, _) <- 
      createProcess (proc "vislcg3" [cg2, "-g", rls]){std_in=UseHandle out2
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