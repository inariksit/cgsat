import CG_base
import CG_SAT hiding (chunk)
import CG_parse
import Control.Lens (element, (.~), (&)) --for updating list index with bazooka
import Control.Monad
import Data.List
import Data.Maybe
import Debug.Trace
import SAT
import System.Environment



--TODO: get tags from the grammar to be tested
--globaltags = map Tag ["det", "n", "v", "pri", "prs", "imp", "p3", "predet", "prn", "adj", "pr"]
globaltags = map Tag ["det", "v", "p3", "imp", "prs"] -- , "predet", "prn"]

randomrules = parseRules False "REMOVE:r1 (v) IF (-1C (det)) ;\nREMOVE:r2 (prs) ;\nREMOVE:r3 (imp) IF (0 (p3)) ;\nSELECT:s4 (p3) IF (-1 det) (1 v) ;\nREMOVE:r5 (p3) IF (-2 det) (-1 v) (1 imp) (2 prs) ;"

goodrules = parseRules False "REMOVE:r1 (v) IF (-1C (det)) ;\nREMOVE:r2 (v) ;"
badrules = parseRules False "REMOVE:r1 (v) ;\nREMOVE:r2 (v) IF (-1C (det)) ;"

main = do
  args <- getArgs
  case args of
   []    -> mapM_ foo (concat randomrules)
   (r:o) -> undefined
      
type Clause = [Lit]

foo :: Rule -> IO [Clause]
foo rule = case rule of
  (Remove _ target cond) -> print rule >> makeFirstSentence rmTarget (toTags target) (toConds cond)
  (Select _ target cond) -> print rule >> makeFirstSentence slTarget (toTags target) (toConds cond)

--makeFirstSentence :: (a->b->c) -> [[Tag]] -> [[Condition]] -> IO [Clause]
makeFirstSentence rmOrSl target conds = do
  s <- newSolver

  lits <- sequence 
           [ sequence [ newLit s | _ <- globaltags ] | _ <- condsByInd ]
  -- e.g. [[v0,v1,v2,v3], [v4,v5,v6,v7]]
  -- where v0=word1_isDet , v1=word1_isN ...
  --       v4=word2_isDet , v5=word2_isN ...
  -- print lits
  -- print ti

  let condcls = zipWith slConds lits condsByInd
  -- print condcls

  let targetcls = condcls!!ti ++ rmOrSl (lits!!ti) (concat target)

  -- print targetcls
  let allcls = concat $ condcls & element ti .~ targetcls
  print allcls
  return allcls
  where 
   condsAsTuples = sort $ (0,[]) `insert` map toTuple (concat conds) --for now, only AND in conds
   condsByInd = groupBy fstEq condsAsTuples
   ti = 999 `fromMaybe` findIndex (elem (0,[])) condsByInd

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
           where filled = [ (k,[]) | k <- [m..n] ]



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

