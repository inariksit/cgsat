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
import System.Random
import System.Random.Shuffle
import Text.Printf

ambiguous = "data/es.tagged.ambiguous"
apertium = "data/apertium-spa.spa.rlx"
small = "data/spa_smallset.rlx"
esgold = "data/es.tagged"
espre = "data/spa_pre.rlx"
rusgr = undefined
rustext = undefined
engr = "data/eng_cg2.rlx"
entext = "data/en.tagged.ambiguous"
engold = "data/en.tagged"
-- entext = "data/vietnam.tagged.ambiguous"
-- engold = "data/vietnam.tagged"
enpre = "data/en_pre.rlx"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["shuffle"]     -> shuffle_ small ambiguous esgold espre
    [r,"shuffle"]   -> shuffle_ r ambiguous esgold espre
    ["enshuffle"]   -> shuffle_ engr entext engold enpre
    ["reverse"]     -> rev small ambiguous esgold espre
    [r,"reverse"]   -> rev r ambiguous esgold espre
    ["enreverse"]   -> rev engr entext engold enpre
    ["opticomp",n]  -> optiComp small ambiguous (read n)
    ["gold","orig"] -> gold apertium ambiguous esgold
    ["gold","opti"] -> opti small ambiguous 
    ["gold","obs"]  -> optiBySz small ambiguous
    ["gold"]        -> gold small ambiguous esgold --using small as default grammar
    ["goldrus"]     -> gold rusgr rustext undefined
    ["engold"]     -> gold engr entext engold
    [r,"gold"]      -> gold r ambiguous esgold --specify grammar

    (r:d:o) -> do rules <- readRules r
                  text <- readData d
                  let is2 = "2" `elem` o
                      verbose = "v" `elem` o
                      debug = "d" `elem` o
                      disam = if "no" `elem` o 
                                then disambiguate verbose debug
                                else disambiguateWithOrder verbose debug
                  resSAT <- mapM (disamSection disam rules) text -- :: [Sentence]
                  resVISL <- vislcg3 r d is2  -- :: [Sentence] 
                  prAll "" resSAT resVISL text verbose
                  putStrLn ""
    _          -> putStrLn "usage: ./test <rules> <data> (or something else, check the source code test/Test.hs >__>)"

gold rl dt g = do rules <- readRules rl
                  text <- readData dt
                  gold <- readData g

                  resSAT <- mapM (disamSection (disambiguateWithOrder False False) rules) text
                  resVISL <- vislcg3 rl dt True
                  putStrLn "SAT-CG in comparison to gold standard"
                  let verbose = length text < 100 --change if you want different output
                  prAll "SAT" resSAT gold text verbose
                  putStrLn "\nVISLCG3 in comparison to gold standard"
                  prAll "VISL" resVISL gold text verbose
                  putStrLn ""



snd4 (_,b,_,_) = b
trd4 (_,_,c,_) = c
fth4 (_,_,_,d) = d

prAll :: String -> [Sentence] -> [Sentence] -> [Sentence] -> Bool -> IO [Float]
prAll str s v tx verbose = do

  let sentsSameLen = [ (s', v', orig) | (s', v', orig) <- zip3 s v tx
                                      , length s' == length v' ]
      -- take only sentences that have same number of analyses
   
      precRec = [ (orig, prec, rec, univ) 
                    | (test, goldst, orig) <- sentsSameLen
                    , let prec = precision test goldst
                    , let rec = recall test goldst 
                    , let univ = [ 1.0 / anas 
                                    | (tw, gw) <- zip test goldst
                                    , (not.null) (tw `intersect` gw)
                                    , let anas = fromIntegral $ length tw ]
                ]

      diffwordsPrec = concatMap snd4 precRec :: [(Analysis,Analysis)]
      diffwordsRec = concatMap trd4 precRec :: [(Analysis,Analysis)]

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
  
  putStr str
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
        prAnas (a1,a2) = do putStrLn "\nDisambiguation by satcg"
                            putStrLn $ showAnalysis a1
                            putStrLn "\nDisambiguation by vislcg3" 
                            putStrLn $ showAnalysis a2


precision :: Sentence -> Sentence -> [(Analysis,Analysis)]
precision s1 s2 = [ (a1, a2) | (a1, a2) <- zip s1 s2
                              , sort a1 /= sort a2 ]

recall :: Sentence -> Sentence -> [(Analysis,Analysis)]
recall cand gold = [ (a1, a2) | (a1, a2) <- zip cand gold
                              , null $ a1 `intersect` a2 ]

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

--shuffle :: [Rule] -> IO ()
shuffle_ r d g pre = do
  rls <- concat `fmap` readRules r
  seed <- newStdGen
  sequence_ [ do mkRuleFile ps fname r pre
                 putStrLn (fname ++ "\n---------") 
                 gold fname d g
                 putStrLn "----------\n\n"
               | (n, ps) <- zip [0..] (take 100 $ shuffles rls seed)
               , let fname = "/tmp/permFull" ++ show n ]
  where shuffles xs seed = let (_,newSeed) = random seed :: (Int,StdGen) 
                           in shuffle' xs (length xs) seed : shuffles (reverse xs) newSeed

rev r d g pre = do
  rls <- concat `fmap` readRules r
  sequence_ [ do mkRuleFile ps fname r pre
                 putStrLn (fname ++ "\n---------") 
                 gold fname d g
                 putStrLn "----------\n\n"
               | (n, ps) <- zip [0..] [rls, reverse rls]
               , let fname = "/tmp/rev" ++ show n ]


mkRuleFile :: [Rule] -> FilePath -> FilePath -> FilePath -> IO ()
mkRuleFile rules fp orig pre = do
  lists <- readFile pre
  writeFile fp lists
  rules <- sequence [ putStr name >> grep name | rl <- rules
                       , let name = head $ words $ show rl ]
  appendFile fp (unwords rules)

  where
    grep name = readProcess "egrep" [("\\<"++name++"\\>"), orig] []

--------------------------------------------------------------------------------

optiBySz rl dt = do r <- concat `fmap` readRules rl
                    t <- readData dt
                    g <- readData esgold
                    let seqs = groupBy (\x y -> length x == length y) (subsequences r)
                    res <- sequence [ loop rset t g [] | rset <- seqs ]
                    putStrLn "Best rule set for each size:"
                    mapM_ (mapM_ pr . (sortBy (\x y -> fst x `compare` fst y))) res

opti rls dat = do r <- concat `fmap` readRules rls
                  t <- readData dat
                  g <- readData esgold
                  res <- loop (subsequences r) t g []
                  putStrLn "optimal rule sequence:"
                  mapM_ pr (take 3 (sortBy (\(x,_) (y,_) -> x `compare` y) res))

optiComp rls dat n = do r <- concat `fmap` readRules rls
                        t <- readData dat
                        g <- readData esgold
                        let nrules = filter (\x -> length x==n) $ subsequences r
                        res <- loop nrules t g []
                        
                        putStrLn "optimal rule sequence:"
                        mapM_ pr (reverse (sortBy (\x y -> fst x `compare` fst y) res))

pr (score,rs) = do putStrLn $ (show score) ++ ":"
                   mapM_ print rs

loop :: [[Rule]] -> [Sentence] -> [Sentence] -> [(Float, [Rule])] -> IO [(Float, [Rule])]
loop []     t g scores = return scores
loop (r:rs) t g scores = do
  res <- mapM (disambiguate False False r) t
  let diff =  [dif | (sat,gold) <- zip res g
                   , let dif = precision sat gold
                   , (not.null) dif ]
      orig = fromIntegral $ length (concat t)
      difbw = fromIntegral $ length (concat diff)
      perc = 100 * ((orig-difbw) / orig) :: Float
  when ((length scores) `mod` 100 == 0) $ print (length scores) 
  loop rs t g ((perc,r):scores)