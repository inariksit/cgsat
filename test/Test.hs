module Main where

import CG_base
import CG_parse (parseData, parseRules)
import CG_SAT
import Control.Monad
import Data.Maybe
import System.Environment
import System.IO
import System.Process

main :: IO ()
main = do
  args <- getArgs
  case args of
    [f1, f2] -> do rules <- readFile f1 >>= parseRules
                   data' <- readFile f2 >>= parseData 
                   result <- liftM (concat . reverse) $ 
                             mapM (disambiguate False rules) data'
                   goldst <- gold f1 f2
                   let foo = zipWith (==) result goldst
                   mapM_ putStrLn goldst
                   mapM_ putStrLn result
                   print $ length goldst
                   print $ length result
                   print foo
                   putStrLn "the end"
    _        -> do putStrLn "usage: ./test <rules> <data>"


gold :: FilePath -> FilePath -> IO [String]
gold rls dt = do
  (_, Just out1, _, _) <-
      createProcess (proc "cat" [dt]){std_out=CreatePipe}
  (_, Just out2, _, _) <- 
      createProcess (proc "cg-conv" ["-a"]){std_in=UseHandle out1
                                          , std_out=CreatePipe}
  (_, Just out3, _, _) <- 
      createProcess (proc "vislcg3" ["-g", rls]){std_in=UseHandle out2
                                               , std_out=CreatePipe}
  result <- hGetContents' out3
  mapM_ hClose [out1,out2,out3]
  return $ reverse $ splitByWords result


splitByWords :: String -> [String]
splitByWords str = go (lines str) []
  where go []     ws = ws
        go (l:ls) ws = let anas = takeWhile (startsWith '\t') ls
                           newLs = dropWhile (startsWith '\t') ls
                       in go newLs ((unlines (l:anas)):ws)

startsWith :: Char -> String -> Bool
startsWith y (x:xs) = x==y
startsWith y []     = False

-- Strict hGetContents
hGetContents' :: Handle -> IO String
hGetContents' hdl = do e <- hIsEOF hdl
                       if e then return []
                            else do c <- hGetChar hdl
                                    cs <- hGetContents' hdl
                                    return (c:cs)