import CG_base
import CG_SAT hiding (chunk)
import CG_parse
import Control.Lens (element, (.~), (&)) --for updating list index with bazooka
import Data.Function (on)
import Data.List
import Data.Maybe
import Debug.Trace
import SAT
import System.Environment
import System.IO.Unsafe


--TODO: get tags from the grammar to be tested
--globaltags = map Tag ["det", "n", "v", "pri", "prs", "imp", "p3", "predet", "prn", "adj", "pr"]
globaltags = map Tag ["det", "v", "p3", "imp", "prs"] -- , "predet", "prn"]

randomrules = concat $ parseRules False "REMOVE:r1 (v) IF (-1C (det)) ;\nREMOVE:r2 (prs) ;\n REMOVE:r3 (imp) IF (0 (p3)) ;\nSELECT:s4 (p3) IF (-1 det) (1 v) ;\nREMOVE:r5 (p3) IF (-2 det) (-1 v) (1 imp) (5 prs) ;"

goodrules = concat $ parseRules False "REMOVE:r1 (v) IF (-1C (det)) ;\nREMOVE:r2 (v) ;"
badrules = concat $ parseRules False "REMOVE:r1 (v) ;\nREMOVE:r2 (v) IF (-1C (det)) ;"

main = do
  args <- getArgs
  case args of
   []    -> do s <- newSolver
               (lits1, cls1) <- doFirst (head randomrules) s
               sequence_ [ addClause s cl | cl <- cls1 ]
               --cs <- sequence [ modelValue s x | x <- concat lits1 ]
               -- putStrLn $ "after " ++ show (head randomrules) ++ ": " ++ show' cs
               (finalSent, finalCl) <- loop s lits1 (tail randomrules)
               print finalSent
               print finalCl

   (r:o) -> undefined
   where
    loop s ls (r1:r2:rs) = 
         do (lits1, cls1) <- doNext r1 ls s
            sequence_ [ addClause s cl | cl <- cls1 ]
            cs1 <- sequence [ modelValue s x | x <- concat lits1 ]
            putStrLn $ "after " ++ show r1 ++ ": " ++ show' cs1
            b <- solve s []
            if b 
              then do
                (lits2, cls2) <- doNext r2 lits1 s
                cs2 <- sequence [ modelValue s x | x <- concat lits2 ]
                putStrLn $ "after " ++ show r2 ++ ": " ++ show' cs2
                putStrLn ""
                loop s lits2 rs
              else return (lits1,cls1)
                              
                              
    loop s ls _ = return (ls,[])
      
type Clause = [Lit]

show' :: [Bool] -> String
show' [] = ""
show' (b:bs) = (if b then "1" else "0") ++ show' bs

doFirst :: Rule -> Solver -> IO ([[Lit]], [Clause])
doFirst rule s = case rule of
  (Remove _ target cond) -> print rule >> makeFirstSentence s rmTarget (toTags target) (toConds cond)
  (Select _ target cond) -> print rule >> makeFirstSentence s slTarget (toTags target) (toConds cond)

makeFirstSentence :: Solver
                      -> ([Lit]->[Tag]->[Clause])
                      -> [[Tag]] 
                      -> [[Condition]] 
                      -> IO ([[Lit]], [Clause])
makeFirstSentence s rmOrSl target conds = do
  lits <- sequence 
           [ sequence [ newLit s | _ <- globaltags ] | _ <- condsByInd ]
  -- e.g. [[v0,v1,v2,v3], [v4,v5,v6,v7]]
  -- where v0=word1_isDet , v1=word1_isN ...
  --       v4=word2_isDet , v5=word2_isN ...
  -- print lits
  -- print ti
  --print condsByInd

  let condcls = zipWith slConds lits condsByInd
  --print condcls

  let targetcls = condcls!!ti ++ rmOrSl (lits!!ti) (concat target)

  -- print targetcls
  let allcls = concat $ condcls & element ti .~ targetcls
  print (lits, allcls)
  putStrLn ""
  return (lits, allcls)
  where 
   condsAsTuples = sort $ fill $ (0,[]) `insert` map toTuple (concat conds) --for now, only AND in conds
   condsByInd = groupBy fstEq condsAsTuples
   ti = 999 `fromMaybe` findIndex (elem (0,[])) condsByInd

doNext :: Rule -> [[Lit]] -> Solver -> IO ([[Lit]], [Clause])
doNext rule lits s = case rule of
  (Remove _ target cond) -> 
    do putStr "\nNext rule: "
       print rule
       checkNextRule s lits (toTags target) (toConds cond)
  (Select _ target cond) -> 
    do putStr "\nNext rule: "
       print rule 
       checkNextRule s lits (toTags target) (toConds cond)

checkNextRule :: Solver
                  -> [[Lit]]
                  -> [[Tag]]
                  -> [[Condition]]
                  -> IO ([[Lit]], [Clause])
checkNextRule s litss target conds = do
  putStr "conditions and target: "
  print condsByInd
  putStr "only conditions :" 
  print condsByIndWoT

  --try to match the new conditions produced by new rule to existing literals
  let listPairs = nub $ 
        map (zip litss) [ drop n condsByInd | n <- [0..(length litss)-1] ] ++
        map ((flip zip) condsByInd)   [ drop n litss | n <- [0..(length condsByInd)-1] ] 
        :: [[([Lit], [(Integer, [Tag])])]]

  --mapM_ print listPairs

  let potentialCondCls = [ [ (lits, conds) | (lits, conds) <- pairs
                                           , unsafePerformIO $ isOK lits conds ] 
                            | pairs <- listPairs ]
  --mapM_ print potentialCondCls
  let maxcomb = head $ reverse $ sortBy (compare `on` length) potentialCondCls
  putStr "max combination: "
  print maxcomb
  let condsInMaxcomb = map snd maxcomb
      condsOutside = condsByInd \\ condsInMaxcomb
      litsInMaxcomb = map fst maxcomb
      litsOutside = litss \\ litsInMaxcomb
  if (not.null) litsOutside 
     then putStrLn "free lits" 
     else putStrLn "no free lits, can't unify"
  putStr "conditions outside literals: "
  print condsOutside
  newlits <- sequence 
              [ sequence [ newLit s | _ <- globaltags ] | _ <- condsOutside ]
  let newclauses = zipWith slConds newlits condsOutside
  return (litss++newlits, concat newclauses) --because the old clauses are already inside SAT solver

  where
   condsWithTarget = nub $ sort $ fill $ (0,concat target) `insert` map toTuple (concat conds)
   condsWithoutTarget = nub $ sort $ fill $ (0,[]) `insert` map toTuple (concat conds)
   condsByInd = groupBy fstEq condsWithTarget :: [[(Integer, [Tag])]]
   condsByIndWoT = groupBy fstEq condsWithoutTarget
   ti = 999 `fromMaybe` findIndex (elem (0,[])) condsByInd

   isOK :: [Lit] -> [(Integer, [Tag])] -> IO Bool
   isOK lits numsConds = do
     let cls = slConds lits numsConds
     testLits <- sequence [ newLit s | _ <- cls ]
     sequence_ [ addClause s (neg tl:cl) | (tl,cl) <- zip testLits cls ]
     b <- solve s testLits
     return b

--TODO: apply more rules to the output produced by the first rule!
{- Idea:
first rule produces a minimum length sequence, and for each word in sequence, variables
that indicate whether one of the tags is true.
For instance, with tagset [det, n, v] and 2 words, we get

[v0,v1,v2] == w1_is_det, w1_is_n, w1_is_v  ; and
[v3,v4,v5] == w2_is_det, w2_is_n, w2_is_v  .

When we apply a next rule, it will have new cond and target words.
We can check if we can unify any of the words, 
e.g. condition of first rule is the same as target for the second rule.
If yes, we add the relevant clauses to the solver state so far.
If not, we add words to the sequence, AND we must check the new sequence with the old rule!
Otherwise we could have 

1) REMOVE v ;
----> output one word with w1_is_v=False

2) SELECT v -- no v left, so we add w2
----> before check: output two words with w1_is_v=False and w2_is_v=True
----> after check: rule 1 makes w2_is_v False
----> conflict, breakdown D:

-}


{-- NEW PLAN

for rule in Pres:
  for w in symbolicSent:
     rl has no effect on w, because one of the following:

     1) conditions are out of scope (trivial, no clause)
     2) conditions in scope, but one doesn't hold
     3) tag has been removed in w
     4) all readings of w have the desired tag (cannot remove)



general stuff:

we know length of symbolic sentence from the width of R
we know the target from R
conditions: if no C, then all tag combinations that have the tag in the condition
            if C, pick one of them (disjoint clauses?)
if empty space in conds, empty places must have >= 1 readings (but any will do)

--}
--------------------------------------------------------------------------------

slConds :: [Lit] -> [(Integer, [Tag])] -> [Clause]
slConds lits numsConds = 
  let lu    = zip globaltags lits
      conds = concatMap snd numsConds in  --concatMap won't work for contexts with OR
  [ [lit] | (tag, lit) <- lu
          , tag `elem` conds ]

rmTarget :: [Lit] -> [Tag] -> [Clause]
rmTarget lits targets = let lu = zip globaltags lits in
  [ lit | (tag, lit) <- lu
        , tag `notElem` targets ]
  :
  [ [neg lit] | (tag, lit) <- lu
              , tag `elem` targets ]

slTarget :: [Lit] -> [Tag] -> [Clause]
slTarget lits targets = let lu = zip globaltags lits in
  [ [lit] | (tag, lit) <- lu
          , tag `elem` targets ]
  ++
  [ [neg lit] | (tag, lit) <- lu
              , tag `notElem` targets ]

--------------------------------------------------------------------------------
--TODO: negative condition
toTuple (C pos (_b,tags)) = (n, concat $ toTags tags)
  where n = case pos of
              Exactly _ i -> i
              AtLeast _ i -> i
              Barrier i _ -> i

-- x:xs must be sorted
fill [] = []
fill (x@(n,_):xs) = go (x:xs) n []
  where go []           _m res = reverse res
        go (x@(n,_):xs) m  res
           | n-m == 1 ||
               n-m == 0 = go xs n (x:res)
           | otherwise  = go xs n (x : (filled++res))
           where filled = [ (k,[]) | k <- [m..n-1] ]



--------------------------------------------------------------------------------

chunk :: Sentence -> [(Integer,[Tag])]
chunk sent = concat $ go sent 1
   where go []    _m = []
         go (x:xs) m = map ((,) m) (addWF m x) : go xs (m+1)
         addWF m = map (WF ("w" ++ show m) :)

dechunk' :: [(Integer,[Tag])] -> Sentence
dechunk' ts = map (map snd) $ groupBy fstEq ts


fstEq (a,_) (b,_) = a==b

--------------------------------------------------------------------------------

