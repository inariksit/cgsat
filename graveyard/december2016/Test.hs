module Main where

import CG_base
import CG_parse
import CG_SAT_old
import CG_eval ( prAll, vislcg3 )
import Control.Monad
import Data.List
import Data.Maybe
import System.Environment
import System.IO
import System.Process
import System.Random
import System.Random.Shuffle
import Text.Printf

spa20k = "data/spa/20k.tagged.ambiguous"
spa20kgold = "data/spa/20k.tagged"
spaFull = "data/spa/spa.rlx"
spaSmall = "data/spa/spa_smallset.rlx"
spaPre = "data/spa/spa_pre.rlx"


rusgr = undefined
rustext = undefined
engr = "data/eng_cg2.rlx"
entext = "data/en.tagged.ambiguous"
engold = "data/en.tagged"
-- entext = "data/vietnam.tagged.ambiguous"
-- engold = "data/vietnam.tagged"
enpre = "data/en_pre.rlx"

nldgr   = "data/nld/nld.rlx"
nldtext = "data/nld/nld_story.txt"
nldgold = "data/nld/nld_story.gold"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["subset",n]    -> subsetN (read n) spaFull spaPre
    ["shuffle"]     -> shuffle_ spaSmall spa20k spa20kgold spaPre
    [r,"shuffle"]   -> shuffle_ r spa20k spa20kgold spaPre
    ["enshuffle"]   -> shuffle_ engr entext engold enpre
    ["reverse"]     -> rev spaSmall spa20k spa20kgold spaPre
    [r,"reverse"]   -> rev r spa20k spa20kgold spaPre
    ["enreverse"]   -> rev engr entext engold enpre
    ["opticomp",n]  -> optiComp spaSmall spa20k (read n)
    ("gold":"orig":o) -> gold' ("max" `elem` o) spaFull spa20k spa20kgold
    ["gold","opti"] -> opti spaSmall spa20k 
    ["gold","obs"]  -> optiBySz spaSmall spa20k
    ["gold"]        -> gold spaSmall spa20k spa20kgold
    ["gold","max"]  -> goldPar spaSmall spa20k spa20kgold
    ["goldrus"]     -> gold rusgr rustext undefined
    ["engold"]      -> gold engr entext engold
    ["nldgold"]     -> gold nldgr nldtext nldgold
    (r:"gold":o)    -> gold' ("max" `elem` o) r spa20k spa20kgold --specify grammar

    (r:d:o) -> do rules <- readRules r
                  text <- readData d
                  let is2 = "2" `elem` o
                      verbose = "v" `elem` o
                      debug = "d" `elem` o
                      justRun = "run" `elem` o
                      disam = if "par" `elem` o 
                                then disambiguateUnordered verbose debug
                                else disambiguate verbose debug
                      disec = if "sec" `elem` o
                                then disamSection disam rules
                                else disam (concat rules)
                  resSAT <- mapM disec text -- :: [Sentence]
                  if justRun then return ()
                   else do
                    resVISL <- vislcg3 r d is2  -- :: [Sentence] 
                    prAll "" resSAT resVISL text verbose
                    putStrLn ""

    _          -> do putStrLn "usage: one of the following"
                     mapM_ putStrLn ["<rules> [gold | <data>] [par run sec]"]


goldPar rl dt g = gold' True rl dt g
gold rl dt g = gold' False rl dt g

gold' p rl dt g = do rules <- readRules rl
                     text <- readData dt
                     gold <- readData g
                     let disam = if p 
                                then disambiguateUnordered False False
                                else disambiguate False False
                     resSAT <- mapM (disamSection disam rules) text
                     resVISL <- vislcg3 rl dt True
                     putStrLn "SAT-CG in comparison to gold standard"
                     let verbose = length text < 10 --change if you want different output
                     prAll "SAT" resSAT gold text verbose
                     putStrLn "\nVISLCG3 in comparison to gold standard"
                     prAll "VISL" resVISL gold text verbose
                     putStrLn ""



snd4 (_,b,_,_) = b
trd4 (_,_,c,_) = c
fth4 (_,_,_,d) = d


--precision :: Sentence -> Sentence -> [(Reading,Reading)]
precision s1 s2 = [ (r1, r2) | (r1, r2) <- zip s1 s2
                              , sort r1 /= sort r2 ]

--recall :: Sentence -> Sentence -> [(Reading,Reading)]
recall cand gold = [ (r1, r2) | (r1, r2) <- zip cand gold
                              , null $ r1 `intersect` r2 ]


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

subsetN n rules pre = do
  rls <- concat `fmap` readRules rules
  let fname = "/tmp/subset" ++ show n
      ps = take n rls
  mkRuleFile (take n rls) fname rules pre


mkRuleFile :: [Rule] -> FilePath -> FilePath -> FilePath -> IO ()
mkRuleFile rules new orig pre = do
  lists <- readFile pre
  writeFile new lists
  rules <- sequence [ putStr name >> grep name | rl <- rules
                       , let name = head $ words $ show rl ]
  appendFile new (unwords rules)

  where
    grep name = readProcess "egrep" [("\\<"++name++"\\>"), orig] []

--------------------------------------------------------------------------------

optiBySz rl dt = do r <- concat `fmap` readRules rl
                    t <- readData dt
                    g <- readData spa20kgold
                    let seqs = groupBy (\x y -> length x == length y) (subsequences r)
                    res <- sequence [ loop rset t g [] | rset <- seqs ]
                    putStrLn "Best rule set for each size:"
                    mapM_ (mapM_ pr . (sortBy (\x y -> fst x `compare` fst y))) res

opti rls dat = do r <- concat `fmap` readRules rls
                  t <- readData dat
                  g <- readData spa20kgold
                  res <- loop (subsequences r) t g []
                  putStrLn "optimal rule sequence:"
                  mapM_ pr (take 3 (sortBy (\(x,_) (y,_) -> x `compare` y) res))

optiComp rls dat n = do r <- concat `fmap` readRules rls
                        t <- readData dat
                        g <- readData spa20kgold
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