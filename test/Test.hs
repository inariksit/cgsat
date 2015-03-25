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
    [r,d,"-v","-2"] -> go r d True True
    [r,d,"-2","-v"] -> go r d True True
    [r,d,"-v2"] -> go r d True True
    [r,d,"-2v"] -> go r d True True
    _           -> putStrLn "usage: ./test <rules> <data>"
  where go r d v is2 = do rules <- readRules r
                          text <- readData d
                          resSAT <- mapM (disambiguate False rules) text -- :: [Sentence]
                          resVISL <- vislcg3 r d is2  -- :: [Sentence]
                          let diffBS = [ (orig, diff)
                                          | (s,v,orig) <- zip3 resSAT resVISL text
                                          , let diff = diffBySent s v
                                          , (not.null) diff ]
                              moreD = [ satDisMore
                                          | (s,v) <- zip resSAT resVISL
                                          , let satDisMore = moreDisamb s v
                                          , (not.null) satDisMore ]
                              
                          when v $ do
                            mapM_ prDiff diffBS

                          let origwords = fromIntegral $ length $ concat text
                              origsents = fromIntegral $ length resSAT
                              diffsents = fromIntegral $ length diffBS
                              diffwords = fromIntegral $ length $ concatMap snd diffBS
                          putStr "(Sentences,words) in the original: "
                          print (origsents,origwords)
                          putStr "Different (sentences,words) from original: "
                          print (diffsents,diffwords)
                          putStr "% same sentences with vislcg3: "
                          print $ 100 * ((origsents - diffsents) / origsents)
                          putStr "% same words with vislcg3: "
                          print $ 100 * ((origwords - diffwords) / origwords)
                          putStr "(SAT disambiguates more/SAT disambiguates differently): "
                          print (length moreD, diffwords)
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
  [ (a1, a2) | (a1, a2) <- diffBySent s1 s2
             , length a1 < length a2
             , (not.null) $ intersect a1 a2 ]


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