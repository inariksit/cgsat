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
  ts <- filter (not.null) `fmap` map parse `fmap` words `fmap` readFile tagfile :: IO [[Tag]]
  print ts
  tc <- map parse `fmap` words `fmap` readFile tagcfile

  args <- getArgs
  case args of
   []    -> do putStrLn "test"
               let spl = init $ splits testrules
               print spl
               (tsets, _) <- readRules' "data/spa_smallset.rlx"
               let tc = nub $ ts ++ concatMap toTags tsets
               print tc
               tokens <- mapM (uncurry (testRule True ts tc)) spl
               corpus <- concat `fmap` readData "data/spa_story.txt"
               mapM_ ((flip checkCorpus) corpus) tokens


   (r:o) -> do let verbose = "v" `elem` o

               (tsets, rls) <- readRules' r
               let rules = concat rls
               mapM_ print rules
               let tc = nub $ concatMap toTags tsets
               let spl = last $ take 9 $ splits (reverse rules)
               print spl
               tokens <- uncurry (testRule verbose ts tc) spl
               corpus <- concat `fmap` readData "data/nld_story.txt"
               checkCorpus tokens corpus
               putStrLn "\n---------\n"

  where splits list = list >>= \x -> return (x, delete x list)

testRule :: Bool -> [[Tag]] -> [[Tag]] -> Rule -> [Rule] -> IO [Token]
testRule verbose alltags tagcombs rule rules = do
  putStrLn $ "Testing with " ++ show rule ++ " as the last rule"
  s <- newSolver
  let ruleWidth = width rule alltags
  allLits <- sequence 
              [ sequence [ newLit s | _ <- tagcombs ] | _ <- ruleWidth ] :: IO [[Lit]]
  let ss = concat
             [ [ (((m,False), addWF m tags),lit) | (lit, tags) <- zip lits tagcombs ]
                   | (lits, m) <- zip allLits [1..length allLits]
             ]  :: [Token]
  mapM_ print ss
  cls <- concat `fmap` 
              sequence [ f s wn cond | (wn, cond) <- zip allLits ruleWidth
                                     , let f = if any (isTarget.fst) cond
                                            then slOrRm
                                            else slCond ] 
                                                
  putStr $ "rule " ++ show rule
  putStrLn $ ": " ++ show (length cls) ++ " clauses"
  
  sequence_ [ do -- when verbose $ print cl
                 addClause s cl | cl <- cls ]

  b <- solve s []
  print b
  if b then do
    as <- sequence [ modelValue s x | x <- concat allLits ]
    let truetoks = [ t | (True, t) <- zip as ss ]
    putStrLn $ showSentence (dechunk truetoks)
       else print "bad D:"
  putStrLn "\n---------\n"

  applied <- nub `fmap` sequence [ analyseGrammar s ss rl
                                   | rl <- rules
                                   , length (width rl alltags) <= length ruleWidth ]


--  print applied
  sequence_ [ do b <- solve s cl
                 if not b 
                   then putStrLn $ "clause " ++ show cl ++ " cannot be applied"
                   else 
                    do addClause s cl

                 when verbose $ do
                   putStrLn $ show rl ++ ": " ++ show cl 
                   --printFancy $ show rl ++ ": " ++ show cl
                   when b $ do
                     as <- sequence [ modelValue s x | x <- concat allLits ]
                     --printFancy (map sh as)
                     let truetoks = [ t | (True, t) <- zip as ss ]
                     putStrLn $ showSentence (dechunk truetoks)
                     --printFancy $ show (dechunk truetoks)
                   putStrLn $ "Solution after prev clause: " ++ show b
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
       else print "bad D:" >> return []
--  putStrLn "\n---------\n"

  -- stuff <- mapM (checkIfApplies s allLits) (tail randomrules)
  -- print stuff
  -- print tagcombs

 where 
  addWF m = (WF ("w" ++ show m) :)

  sh True = '1' 
  sh False = '0'

  -- conditions for one word; there can be many but the index is the same
  slCond :: Solver -> [Lit] -> [(Info, [[Tag]])] -> IO [Clause]
  slCond s wn conds = trace ("slCond: " ++ show (concatMap (\(_,ts) -> ts) conds)) $
   do
    let n = length tagcombs - 1 
        context = concatMap snd conds :: [[Tag]]
    newlits <- sequence [ newLit s | _ <- context ]
    let f = if all (isCautious.fst) conds then disjunctionC else disjunction --TODO
    let disjs = filter notNegUnit $ nub $ concat
              [ f n nl tags | (nl, tags) <- zip newlits context ]
    return $ newlits:disjs 

    where
     disjunctionC :: Int -> Lit -> [Tag] -> [Clause]
     disjunctionC n nl ts = 
       let indY = lookup' tagcombs ts
           indN = [0..n] \\ indY
       in [ [ neg nl, neg$wn!!ind ] | ind <- indN ] ++
          [   neg nl:[  wn !! ind | ind <- indY ] ]

     disjunction n nl ts = 
       let indY = lookup' tagcombs ts
       in [ neg nl:[  wn !! ind | ind <- indY ] ]
      
     notNegUnit [x] = pos x
     notNegUnit _   = True

  -- At index 0 there can be both targets and conditions
  slOrRm :: Solver -> [Lit] -> [(Info, [[Tag]])] -> IO [Clause]
  slOrRm s wn ind0s =  trace ("slOrRm: " ++ show (concatMap (\(_,ts) -> ts) ind0s)) $ do
    let n = length tagcombs - 1
    let (trgs,conds) = partition (isTarget.fst) ind0s
    newlits <- sequence [ newLit s | _ <- trgs ]
    let trgCls = 
         nub $ concat [ disj n nl tags | (nl, tags) <- zip newlits (concatMap snd trgs) ]

    condCls <- slCond s wn conds
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
              , isTarget   :: Bool } deriving (Eq,Ord,Show)

defaultTrg = I 0 False True

mkCond :: Int -> Info
mkCond i = I i False False

sameIndInfo :: (Info,a) -> (Info,a) -> Bool
sameIndInfo (I i _ _, a) (I i' _ _, b) = i== i'

--------------------------------------------------------------------------------

toTuple :: [[Tag]] -> Condition -> (Info,[[Tag]])
toTuple _ Always              = error "toTuple applied to Always: this should not happen"
toTuple _ (C pos (True,tags)) = (I n b False, toTags tags)
 where 
  (n,b) = case pos of
                 Exactly b i -> (i,b)
                 AtLeast b i -> (i,b)
                 Barrier i _ -> (i,False)
toTuple alltags (C pos (False,tags)) = trace ("toTuple neg: " ++ show complement ++ "\\ " ++ show tags) $ (I n b False, complement)
 where 
  (n,b) = case pos of
                 Exactly b i -> (i,False)
                 AtLeast b i -> (i,False)
                 Barrier i _ -> (i,False)
  complement = alltags \\ toTags tags


-- x:xs must be sorted
fill :: [(Info, [[Tag]])] -> [(Info, [[Tag]])]
fill [] = []
fill (x@((I n b t),_):xs) = go (x:xs) n []
 where
  go []              _m res = reverse res
  go (x@((I n b t),_):xs) m res | n-m == 1 || n-m == 0 = go xs n (x:res)
                              | otherwise            = go xs n (x:(filled++res))
           where filled = [ (I k False False, [[]]) | k <- [m+1..n-1] ]



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