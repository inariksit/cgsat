module CG_eval ( prAll, vislcg3 ) 
where

import CG_base
import CG_parse
import CG_SAT
import Control.Monad
import Data.List
import Data.Maybe
import System.Environment
import System.IO
import System.Process
--import System.Random
import Text.Printf

spa20k = "data/spa/20k.tagged.ambiguous"
spa20kgold = "data/spa/20k.tagged"
spaFull = "data/spa/apertium-spa.spa.rlx"
spaSmall = "data/spa/spa_smallset.rlx"
spaPre = "data/spa/spa_pre.rlx"


nldgr   = "data/nld/nld.rlx"
nldtext = "data/nld/nld_story.txt"
nldgold = "data/nld/nld_story.gold"




snd4 (_,b,_,_) = b
trd4 (_,_,c,_) = c
fth4 (_,_,_,d) = d

prAll :: String -> [Sentence] -> [Sentence] -> [Sentence] -> Bool -> IO [Float]
prAll nm cand gold tx verbose = do

  let sentsSameLen = [ cgo | cgo@(c, g, orig) <- zip3 cand gold tx
                           , length c == length g ]
      -- take only sentences that have same number of cohorts
   
      precRec = [ (orig, prec, rec, univ) 
                    | (test, goldst, orig) <- sentsSameLen
                    , let prec = precision test goldst
                    , let rec = recall test goldst 
                    --TODO: something strange in the univ measurement
                    {- Precision 100.00, recall 100.00 
                        General diff 78.46 
                        F-score 100.00 -}
                    , let univ = [ 1.0 / anas 
                                    | (tw, gw) <- zip test goldst
                                    , (not.null) (tw `intersect` gw)
                                    , let anas = fromIntegral $ length tw ]
                ]

      diffwordsPrec = concatMap snd4 precRec :: [(Cohort,Cohort)]
      diffwordsRec = concatMap trd4 precRec :: [(Cohort,Cohort)]

      universaldiff = sum $ concatMap fth4 precRec

  when verbose $ mapM_ prDiff precRec

  let origwlen = length $ concatMap (\(a,_,_) -> a) sentsSameLen
      origslen = length sentsSameLen
      diffwlenPrec = length diffwordsPrec
      diffwlenRec = length diffwordsRec

      moreD = length $ filter (\(t,g) -> length t < length g) diffwordsPrec
      lessD = length $ filter (\(t,g) -> length t > length g) diffwordsPrec
      diffD = length $ filter (\(t,g) -> null $ intersect t g) diffwordsPrec

      prec = 100 * (fromIntegral origwlen - fromIntegral diffwlenPrec) / fromIntegral origwlen
      rec  = 100 * (fromIntegral origwlen - fromIntegral diffwlenRec) / fromIntegral origwlen
      univ = 100 * (universaldiff / fromIntegral origwlen)
      fscore = 2 * ( (prec*rec) / (prec+rec) )

  putStrLn $ "Original: " ++ show origslen ++ " sentences, " 
                          ++ show origwlen ++ " words"
  putStrLn $ "Different words from original: " ++ show diffwlenPrec
  printf "Precision %.2f, " (prec :: Float)
  printf "recall %.2f \n" (rec :: Float)
  
  putStr nm
  printf " General diff %.2f \n" (univ :: Float)

  printf " F-score %.2f \n" (fscore :: Float)
  
  putStr "Disambiguates (more,less,disjoint,all): "
  print (moreD, lessD, diffD, diffwlenPrec)
  putStrLn ""
  return [prec,rec,univ]

  where prDiff (s,as,_,_) = do putStrLn "---------------\n"
                               putStrLn "Original sentence:"
                               putStrLn $ showSentence s
                               mapM_ prAnas as
        prAnas (a1,a2) = do putStrLn $ "\nDisambiguation by candidate " ++ nm 
                            putStrLn $ showCohort a1
                            putStrLn "\nGold" --Disambiguation by VISL CG-3" 
                            putStrLn $ showCohort a2

--We zip the sentences, so it doesn't matter that
--SAT-CG sentences are missing sentence boundary!
precision :: Sentence -> Sentence -> [(Cohort,Cohort)]
precision cand gold = [ (r1, r2) | (r1, r2) <- zip cand gold
                                 , sort r1 /= sort r2 ]

recall :: Sentence -> Sentence -> [(Cohort,Cohort)]
recall cand gold = [ (r1, r2) | (r1, r2) <- zip cand gold
                              , null $ r1 `intersect` r2 ]

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

--wordCount :: [Sentence] -> Int
wordCount s = length $ map (filter (\x -> null $ [BOS,EOS] `intersect` x)) $ concat s


--------------------------------------------------------------------------------



pr (score,rs) = do putStrLn $ (show score) ++ ":"
                   mapM_ print rs

