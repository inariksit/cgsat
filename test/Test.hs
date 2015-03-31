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

ambiguous = "data/es.tagged.ambiguous"
apertium = "data/apertium-spa.spa.rlx"
small = "data/spa_smallset.rlx"
golddata = "data/es.tagged"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["opticomp",n]  -> optiComp small ambiguous (read n)
    ["gold","orig"] -> gold apertium ambiguous
    ["gold","opti"] -> opti small ambiguous
    ["gold","obs"]  -> optiBySz small ambiguous
    ["gold"]        -> gold small ambiguous --using small as default grammar

    [r,"gold"]      -> gold r ambiguous --specify grammar

    (r:d:o) -> do rules <- readRules r
                  text <- readData d
                  resSAT <- mapM (disambiguate False rules) text -- :: [Sentence]
                  let is2 = "2" `elem` o
                      verbose = "v" `elem` o
                  resVISL <- vislcg3 r d is2  -- :: [Sentence] 
                  prAll resSAT resVISL text verbose
    _          -> putStrLn "usage: ./test <rules> <data> (or something else, check the source code test/Test.hs >__>)"

gold rls dat = do rules <- readRules rls
                  text <- readData dat
                  resSAT <- mapM (disambiguate False rules) text
                  resVISL <- vislcg3 rls dat True
                  gold <- readData golddata
                  putStrLn "SAT-CG in comparison to gold standard"
                  let verbose = length text < 100 --change if you want different output
                  prAll resSAT gold text verbose
                  putStrLn "\nVISLCG3 in comparison to gold standard"
                  prAll resVISL gold text verbose

optiBySz rl dt = do r <- readRules rl
                    t <- readData dt
                    g <- readData golddata
                    let seqs = groupBy (\x y -> length x == length y) (subsequences r)
                    res <- sequence [ loop rset t g [] | rset <- seqs ]
                    putStrLn "Best rule set for each size:"
                    mapM_ (mapM_ pr . (sortBy (\x y -> fst x `compare` fst y))) res

opti rls dat = do r <- readRules rls
                  t <- readData dat
                  g <- readData golddata
                  res <- loop (subsequences r) t g []
                  putStrLn "optimal rule sequence:"
                  mapM_ pr (take 3 (sortBy (\x y -> fst x `compare` fst y) res))

optiComp rls dat n = do r <- readRules rls
                        t <- readData dat
                        g <- readData golddata
                        let nrules = filter (\x -> length x==n) $ subsequences r
                        res <- loop nrules t g []
                        
                        putStrLn "optimal rule sequence:"
                        mapM_ pr (reverse (sortBy (\x y -> fst x `compare` fst y) res))

pr (score,rs) = do putStrLn $ (show score) ++ ":"
                   mapM_ print rs

loop :: [[Rule]] -> [Sentence] -> [Sentence] -> [(Float, [Rule])] -> IO [(Float, [Rule])]
loop []     t g scores = return scores
loop (r:rs) t g scores = do
  res <- mapM (disambiguate False r) t
  let diff =  [dif | (sat,gold) <- zip res g
                   , let dif = diffBySent sat gold
                   , (not.null) dif ]
      orig = fromIntegral $ length (concat t)
      difbw = fromIntegral $ length (concat diff)
      perc = 100 * ((orig-difbw) / orig) :: Float
  when ((length scores) `mod` 100 == 0) $ print (length scores) 
  loop rs t g ((perc,r):scores)

  
prAll s v tx verbose = do
  let diffsents = [ (orig, diff) | (test, goldst, orig) <- zip3 s v tx
                                 , let diff = diffBySent test goldst
                                 , (not.null) diff ]
      diffwords = concatMap snd diffsents :: [(Analysis,Analysis)]
      
  when verbose $ mapM_ prDiff diffsents

  let origwlen = length $ concat tx
      origslen = length s
      diffslen = length diffsents
      diffwlen = length diffwords

      moreD = length $ filter (\(tst,gld) -> length tst < length gld)  diffwords
      lessD = length $ filter (\(tst,gld) -> length tst > length gld) diffwords
      diffD = length $ filter (\(tst,gld) -> null $ intersect tst gld) diffwords
  putStr "(Sentences,words) in the original: "
  print (origslen,origwlen)
  putStr "Different (sentences,words) from original: "
  print (diffslen,diffwlen)
  putStr "% same sentences: "
  print $ 100 * ((fromIntegral origslen - fromIntegral diffslen) / fromIntegral origslen)
  putStr "% same words: "
  print $ 100 * ((fromIntegral origwlen - fromIntegral diffwlen) / fromIntegral origwlen)
  putStr "Disambiguates (more,less,disjoint,all): "
  print (moreD, lessD, diffD, diffwlen)
  putStrLn ""

  where prDiff (s,as) = do putStrLn "---------------\n"
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