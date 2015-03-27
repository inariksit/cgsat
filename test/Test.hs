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

ambiguous = "data/ambiguous/es.tagged.ambiguous"
apertium = "data/apertium-spa.spa.rlx"
flipped = "data/spa_cg3_flip.rlx"
nonflipped = "data/spa_onlyinari.rlx"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["gold","flip"] -> gold flipped ambiguous
    ["gold","mini"] -> gold "/tmp/jugarcg" ambiguous
    ["gold","orig"] -> gold apertium ambiguous
    ["gold"] -> gold nonflipped ambiguous
    [r,d,"-gold"] -> gold r d
    [r,d]      -> go r d False
    [r,d,"-2"] -> go r d True
    _           -> putStrLn "usage: ./test <rules> <data>"
  where gold rls dat = do rules <- readRules rls
                          text <- readData dat
                          resSAT <- mapM (disambiguate False rules) text
                          resVISL <- vislcg3 rls dat False  -- :: [Sentence]
                          gold <- readData "data/gold/es.tagged"
                          putStrLn "SAT-CG in comparison to gold standard"
                          prAll resSAT gold text
                          putStrLn "\nVISLCG3 in comparison to gold standard"
                          prAll resVISL gold text

        go rl dt is2 = do rules <- readRules rl
                          text <- readData dt
                          resSAT <- mapM (disambiguate False rules) text -- :: [Sentence]
                          resVISL <- vislcg3 rl dt is2  -- :: [Sentence]
                          prAll resSAT resVISL text
                          
        prAll s v tx = do let diffBS = [ (orig, diff)
                                          | (sat,visl,orig) <- zip3 s v tx
                                          , let diff = diffBySent sat visl
                                          , (not.null) diff ]
                              moreD = length $ filter (not.null) $ zipWith moreDisamb s v
                              lessD = length $ filter (not.null) $ zipWith lessDisamb s v
                              diffD = length $ filter (not.null) $ zipWith noIntersect s v
                              restD = length $ filter (not.null) $ zipWith diffBySent s v
                                                      
                              
                          when (length diffBS < 100) $ mapM_ prDiff diffBS

                          let origwords = fromIntegral $ length $ concat tx
                              origsents = fromIntegral $ length s
                              diffsents = fromIntegral $ length diffBS
                              diffwords = fromIntegral $ length $ concatMap snd diffBS
                          putStr "(Sentences,words) in the original: "
                          print (origsents,origwords)
                          putStr "Different (sentences,words) from original: "
                          print (diffsents,diffwords)
                          putStr "% same sentences: "
                          print $ 100 * ((origsents - diffsents) / origsents)
                          putStr "% same words: "
                          print $ 100 * ((origwords - diffwords) / origwords)
                          putStr "Disambiguates (more,less,disjoint,rest,all): "
                          print (moreD, lessD, diffD,restD,diffwords)
                          putStrLn ""

        prDiff :: (Sentence, [(Analysis,Analysis)]) -> IO ()
        prDiff (s,as) = do putStrLn "---------------\n"
                           putStrLn "Original sentence:"
                           putStrLn $ showSentence s
                           mapM_ prAnas as
        prAnas (a1,a2) = do putStrLn "\nDisambiguation by satcg"
                            putStrLn $ showAnalysis a1
                            putStrLn "\nDisambiguation by vislcg3"
                            putStrLn $ showAnalysis a2


diffBySent :: Sentence -> Sentence -> [(Analysis,Analysis)]
diffBySent s1 s2 = [ (a1, a2) | (a1, a2) <- zip s1 s2
                              , sort a1 /= sort a2 ]

moreDisamb :: Sentence -> Sentence -> [(Analysis, Analysis)]
moreDisamb s1 s2 =
  [ (a1, a2) | (a1, a2) <- zip s1 s2
             , length a1 < length a2
             , (not.null) $ intersect a1 a2 ]

lessDisamb :: Sentence -> Sentence -> [(Analysis, Analysis)]
lessDisamb s1 s2 =
  [ (a1, a2) | (a1, a2) <- zip s1 s2
             , length a1 > length a2
             , (not.null) $ intersect a1 a2 ]

diffDisamb :: Sentence -> Sentence -> [(Analysis, Analysis)]
diffDisamb s1 s2 = [ (a1, a2) | (a1, a2) <- zip s1 s2
                              , sort a1 /= sort a2 ]

noIntersect :: Sentence -> Sentence -> [(Analysis, Analysis)]
noIntersect s1 s2 = [ (a1, a2) | (a1, a2) <- zip s1 s2
                           , null $ intersect a1 a2 ]

vislcg3 :: FilePath -> FilePath -> Bool -> IO [Sentence]
vislcg3 rls dt isCG2 = do
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