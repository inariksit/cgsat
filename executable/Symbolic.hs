import CG_base
import CG_SAT 
import CG_parse
import Control.Monad ( when )
import Data.List
import Debug.Trace
import SAT
import System.Environment
import System.IO ( hFlush, stdout )
import System.IO.Unsafe

-- tagcfile = "data/spa_tagcombs.txt"
-- tagfile = "data/spa_tags.txt"

tagcfile = "data/nld_tagcombs.txt"
tagfile = "data/nld_tags.txt"



lookup' :: [[Tag]] -> [Tag] -> [Int] --list of indices where the wanted taglist is found
-- For empty tagsInCond, returns all indices in tagcombs.
-- This is wanted behaviour; when filling conditions with empty spaces, 
-- those spaces can have any tag.
-- TODO: Barrier case
lookup' tagcombs tagsInCond = --trace ("lookup': " ++ show tagsInCond) $
  findIndices (\tc -> all (\t -> t `elem` tc) tagsInCond) tagcombs


testrules = concat $ snd $ parseRules False "SELECT:r1 (aa) OR (acr) IF (-1C (mf)) ;\nREMOVE:r2 (aa) IF (-1C (acr)) ;"

main = do
  tc <- (map parse . words) `fmap` readFile tagcfile

  args <- getArgs
  case args of
   []    -> do putStrLn "test"
               ts <- (filter (not.null) . map parse . words) `fmap` readFile "data/spa_tags.txt"
  
               print ts
               let spl = splits testrules
               print spl
               (tsets, _) <- readRules' "data/spa_smallset.rlx"
               let tc = nub $ ts ++ concatMap toTags tsets
               print tc
               tokens <- mapM (uncurry (testRule True ts tc)) spl
               corpus <- concat `fmap` readData "data/spa_story.txt"
               mapM_ ((flip checkCorpus) corpus) tokens


   ("tiny":_)
         -> do let ts = map ((:[]) . Tag) ["det","v","n"] ++ [[Tag "adj", Tag "pred"], [Tag "adj", Tag "attr"]]
               --let rules = concat $ snd $ parseRules False "REMOVE:r1 (v) IF (-1C (det)) ; REMOVE:r2 (adj) ; REMOVE:r3 (n) IF (-1C (det));"
               let rules = concat $ snd $ parseRules False "REMOVE:r1 (v) IF (-1 (det)) (0 (n)) ; REMOVE:r2 (adj); REMOVE:r3 (n) IF (-1 (det));"
               let spl = splits $ reverse rules
               let tc = ts
               tokens <- mapM (uncurry (testRule True ts tc)) spl
               --mapM_ putStrLn (map (showSentence . dechunk) tokens)
               putStrLn "\n---------\n"

   ("nld":r)
         -> do let verbose = "v" `elem` r
               ts <- (filter (not.null) . map parse . words) `fmap` readFile "data/nld_tags.txt"
  
               (tsets, rls) <- readRules' "data/nld.rlx"
               let rules = concat rls
               mapM_ print rules
               let tc = nub $ concatMap toTags tsets
               let spl = splits rules
               mapM_ print spl
               tokens <- mapM (uncurry (testRule verbose ts tc)) spl
               corpus <- concat `fmap` readData "data/nld_story.txt"
               --checkCorpus tokens corpus
               putStrLn "\n---------\n"

  where 
   splits :: (Eq a) => [a] -> [(a,[a])]
   splits xs = xs >>= \x -> let Just ind = elemIndex x xs
                            in return (x, take ind xs)

testRule :: Bool -> [[Tag]] -> [[Tag]] -> Rule -> [Rule] -> IO [Token]
testRule verbose alltags tagcombs rule rules = do
  putStrLn "************* testRule ***************"
  putStrLn $ "Testing with " ++ show rule ++ " as the last rule"
  putStrLn "the rest of the rules: " >> mapM_ print rules

  s <- newSolver
  let ruleWidth = width rule alltags
  allLits <- sequence 
              [ sequence [ newLit s | _ <- tagcombs ] | _ <- ruleWidth ] :: IO [[Lit]]
  let ss = concat
             [ [ (((m,False), addWF m tags),lit) | (lit, tags) <- zip lits tagcombs ]
                   | (lits, m) <- zip allLits [1..length allLits]
             ]  :: [Token]
  when verbose $ do putStrLn "symbolic sentence:"
                    mapM_ print ss
  cls <- concat `fmap` 
              sequence [ f s wn sword | (wn, sword) <- zip allLits ruleWidth
                                      , let f = if any (isTarget.fst) sword
                                            then slOrRm
                                            else slCond ] 
                                                
  putStr $ "rule " ++ show rule
  putStrLn $ ": " ++ show (length cls) ++ " clauses"
  
  sequence_ [ do when verbose $ print cl
                 addClause s cl | cl <- cls ]

  b <- solve s []
  print b
  if b then do
    as <- sequence [ modelValue s x | x <- concat allLits ]
    let truetoks = [ t | (True, t) <- zip as ss ]
    putStrLn $ showSentence (dechunk truetoks)
   else do
    putStrLn "bad initial rule"
    conf <- conflict s
    putStr "conf: "
    print conf
  putStrLn "\n---------\n"

  applied <- nub `fmap` sequence [ analyseGrammar s ss rl
                                   | rl <- rules
                                   , length (width rl alltags) <= length ruleWidth ]


--  print applied
  sequence_ [ do -- b <- solve s cl
                 -- if not b 
                 --   then do
                 --         putStrLn $ "clause " ++ show cl ++ " cannot be applied"
                 --         conf <- conflict s
                 --         putStr "conf: "
                 --         print conf
                 --   else
                 addClause s cl

                 when verbose $ do
                   putStrLn $ show rl ++ ": " ++ show cl 
                   --printFancy $ show rl ++ ": " ++ show cl
                   -- when b $ do
                   --   as <- sequence [ modelValue s x | x <- concat allLits ]
                   --   --printFancy (map sh as)
                   --   let truetoks = [ t | (True, t) <- zip as ss ]
                   --   putStrLn $ showSentence (dechunk truetoks)
                   --   --printFancy $ show (dechunk truetoks)
                   -- putStrLn $ "Solution after prev clause: " ++ show b
                   --printFancy $ "Solution after prev clause: " ++ show b
                   
                 | (rl, cls) <- zip rules applied 
                 , cl <- cls ]
  b <- solve s []
  putStr "Solution after all clauses: "
  print b
  if b then do
    as <- sequence [ modelValue s x | x <- concat allLits ]
    let truetoks = [ t | (True, t) <- zip as ss ]
    putStrLn $ showSentence (dechunk truetoks)
    return truetoks
       else do
         print "bad D:" 
         conf <- conflict s
         putStr "conf: "
         print conf
         return []


 where 
  addWF m = (WF ("w" ++ show m) :)

  sh True = '1' 
  sh False = '0'

  -- conditions for one word; there can be many but the index is the same
   --TODO: here or somewhere else, make sure that (prn) \\ (pers) doesn't match (prn pers)
  slCond :: Solver -> [Lit] -> [(Info, [[Tag]])] -> IO [Clause]
  slCond s wn sword = trace ("slCond: " ++ show (map snd sword)) $
   do
    let n = length tagcombs - 1 
    newlits <- sequence --takes care of one condition with OR vs. many conditions with AND
                [ sequence [ newLit s | _ <- tagss ] | tagss <- map snd sword ] :: IO [[Lit]]
    putStrLn $ ("slCond.newlits: " ++ show newlits)
    let disjs = map (filter notNegUnit) $ 
                [ f n nl tags | (nlits, (info, tagss)) <- zip newlits sword
                              , (nl, tags) <- zip nlits tagss
                              , let f = if isCautious info
                                          then disjunctionC
                                          else if isPositive info
                                                 then disjunction
                                                 else negative ]
    return $ newlits ++ concat disjs 

    where
     disjunctionC :: Int -> Lit -> [Tag] -> [Clause]
     disjunctionC n nl ts = 
       let indY = lookup' tagcombs ts
           indN = [0..n] \\ indY
       in [ [ neg nl, neg$wn!!ind ] | ind <- indN ] ++
          [   neg nl:[  wn !! ind | ind <- indY ] ]

     negative n nl ts = trace "negative" $ 
       let indN = lookup' tagcombs ts
       in [ neg nl:[  neg $ wn !! ind | ind <- indN ] ]

     disjunction n nl ts = 
       let indY = lookup' tagcombs ts
       in [ neg nl:[  wn !! ind | ind <- indY ] ]
      
     notNegUnit [x] = pos x
     notNegUnit _   = True

  -- At index 0 there can be both targets and conditions
  slOrRm :: Solver -> [Lit] -> [(Info, [[Tag]])] -> IO [Clause]
  slOrRm s wn ind0s = trace ("slOrRm: " ++ (show $ concatMap snd ind0s)) $ do
    let n = length tagcombs - 1
    let (trgs,conds) = partition (isTarget.fst) ind0s
    newlits <- sequence [ newLit s | _ <- trgs ]
    --putStrLn $ ("slOrRm.newlits: " ++ show newlits)
    let trgCls = 
         nub $ concat [ disj n nl tags | (nl, tags) <- zip newlits (concatMap snd trgs) ]

    
    condCls <- if null conds
                then return [] 
                else slCond s wn conds
    return $ (newlits:trgCls) ++ condCls

    where
     disj :: Int -> Lit -> [Tag] -> [Clause]
     disj n nl ts = 
       let indY = lookup' tagcombs ts
           indN = [0..n] \\ indY
       in [ neg nl:[ wn !! ind | ind <- indN ] ] ++
          [ neg nl:[ wn !! ind | ind <- indY ] ]

--------------------------------------------------------------------------------
      
width :: Rule    -- ^ rule whose width to calculate 
      -> [[Tag]] -- ^ all possible tag combinations
      -> [[(Info,[[Tag]])]] -- ^ conditions and target
width rule alltags = case rule of
  (Select _ target Always) -> [[(defaultTrg, toTags target)]]
  (Remove _ target Always) -> [[(defaultTrg, toTags target)]]
  (Select _ target conds) -> doStuff target conds
  (Remove _ target conds) -> doStuff target conds
 where 
  doStuff t cs = 
    groupBy sameIndInfo $ nub $ sort $ fill $ 
      (defaultTrg, toTags t) `insert` map (toTuple alltags) (concat (toConds cs))


--------------------------------------------------------------------------------

data Info = I { index      :: Int 
              , isCautious :: Bool
              , isTarget   :: Bool
              , isPositive :: Bool } deriving (Eq,Ord,Show)

defaultTrg = I 0 False True True

mkCond :: Int -> Info
mkCond i = I i False False True



sameIndInfo :: (Info, a) -> (Info, a) -> Bool
sameIndInfo (I i _ _ _, a) (I i' _ _ _, a') = i== i'

--------------------------------------------------------------------------------

toTuple :: [[Tag]] -> Condition -> (Info,[[Tag]])
toTuple _ Always              = error "toTuple applied to Always: this should not happen"
toTuple _ (C pos (positive,tags)) = ( I { index      = ind
                                        , isCautious = c
                                        , isTarget   = False
                                        , isPositive = positive}
                                    , toTags tags )
 where 
  (ind,c) = case pos of
                 Exactly c i -> (i,c)
                 AtLeast c i -> (i,c)
                 Barrier i _ -> (i,False)



-- x:xs must be sorted
fill :: [(Info, [[Tag]])] -> [(Info, [[Tag]])]
fill [] = []
fill (x@((I ind c t ng),_):xs) = go (x:xs) ind []
 where
  go []                       _m res = reverse res
  go (x@((I ind c t ng),_):xs) m res | ind-m == 1 || ind-m == 0 = go xs ind (x:res)
                                     | otherwise       = go xs ind (x:(filled++res))
           where filled = [ (mkCond k, [[]]) | k <- [m+1..ind-1] ]



--------------------------------------------------------------------------------

-- chunk :: Sentence -> [(Info, [Tag])]
-- chunk sent = concat $ go sent 1
--    where go []    _m = []
--          go (x:xs) m = map ((,) (mkInfo m)) (addWF m x) : go xs (m+1)
--          addWF m = map (WF ("w" ++ show m) :)

dechunk' :: [(Info,[Tag])] -> Sentence
dechunk' ts = map (map snd) $ groupBy sameIndInfo ts

dechunk :: [Token] -> Sentence
dechunk ts = (map.map) getTags (groupBy sameInd ts)


fstEq (a,_) (b,_) = a==b

--------------------------------------------------------------------------------

parse :: String -> [Tag]
parse str = map toTag $ filter (not.null) $ split isValid str
 where 
  isValid c = c=='<' || c=='+'
  toTag ">>>" = BOS
  toTag "<<<" = EOS
  toTag []    = error "empty tag"
  toTag str = if last str=='>' then Tag (init str) else Lem str

split :: (a -> Bool) -> [a] -> [[a]]
split p [] = []
split p xs = takeWhile (not . p) xs : split p (drop 1 (dropWhile (not . p) xs))

printFancy :: String -> IO ()
printFancy s = 
  do putStr (s ++ back)
     hFlush stdout
     putStr (wipe ++ back)
 where
  n   = length s
  back = replicate n '\b'
  wipe = replicate n ' '


--------------------------------------------------------------------------------

--just a naive try to prune the corpus-free method by using a corpus.
--TODO rather restrict the number/combination of tags in the SAT formulas?

checkCorpus :: [Token] -> Sentence -> IO ()
checkCorpus symbsent corpus = do
  putStrLn "------------"
  putStrLn "checkCorpus:"
  let sent = dechunk symbsent
--  putStrLn $ showSentence sent
  let foo = filter (\s -> length s >= 2) (adjacent sent corpus)
  mapM_ pr $ map showSentence foo 
  where pr arg = putStrLn "---" >> putStrLn arg

elem' :: Analysis -> Analysis -> [Analysis]
elem' symbolic real = nub [ real | s <- symbolic, r<-real
                               , not $ null $ s `intersect` r ]

adjacent :: Sentence -> Sentence -> [Sentence]
adjacent (w1:w2:[]) corpus = 
 [ words1++words2 | (c1,c2) <- zip corpus (drop 1 corpus)
    , let words1 = elem' w1 c1
    , let words2 = elem' w2 c2
    , (not.null) words1
    , (not.null) words2
    , all (any mt1) [words1,words2] ] 

adjacent (w1:w2:ws) corpus = 
 [ words1 | (c1,c2) <- zip corpus (drop 1 corpus)
    , let words1 = elem' w1 c1
    , let words2 = elem' w2 c2
    , (not.null) words1
    , (not.null) words2
    , all (any mt1) [words1,words2] ]
 ++ adjacent (w2:ws) corpus

adjacent ws         corpus = []

mt1 :: [[Tag]] -> Bool
mt1 []     = False
mt1 (t:ts) = if length t > 1 then True else mt1 ts


--------------------------------------------------------------------------------
{--
checkIfApplies ::  Solver
                  -> [[Lit]]
                  -> Rule 
                  -> IO [Clause]
checkIfApplies s lits rule = do
  if length lits >= length sswidth 
    then do putStrLn "sufficiently long sentence"
            let inds = map (concatMap (map (lookup' [[]]) . snd)) sswidth
            print inds --e.g. [[[0,1]],[[2,3,4,5]],[[3,5],[2,4]]]
            --for lit in lits:
            --
            print sswidth
            return []
    else do putStrLn "conditions out of scope"
            return []
  where sswidth = width rule []



for rule in Pres:
  for w in symbolicSent:
     rl has no effect on w, because one of the following:

     1) conditions are out of scope (trivial, no clause)
     2) conditions in scope, but one doesn't hold
     3) tag has been removed in target
     4) all readings of  target have the desired tag (cannot remove)



general stuff:

we know length of symbolic sentence from the width of R
we know the target from R
conditions: if no C, then all tag combinations that have the tag in the condition
            if C, pick one of them (disjoint clauses?)
if empty space in conds, empty places must have >= 1 readings (but any will do)

--}