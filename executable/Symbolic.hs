import CG_base
import CG_SAT hiding (chunk)
import CG_parse
import Control.Monad
import Data.List
import Data.Maybe
import Debug.Trace
import SAT
import SAT.Optimize
import SAT.Unary hiding (modelValue)
import System.Environment
import System.IO.Unsafe



--TODO: get tags from the grammar to be tested
--globaltags = map Tag ["det", "n", "v", "pri", "prs", "imp", "p3", "predet", "prn", "adj", "pr"]
globaltags = map Tag ["det", "v", "p3", "imp", "prs"] -- , "predet", "prn"]

randomrules = parseRules False "REMOVE:r1 (v) IF (-1C (det)) ;\nREMOVE:r2 (prs) ;\nSELECT:r3 (imp) IF (0 (p3)) ;"

goodrules = parseRules False "REMOVE:r1 (v) IF (-1C (det)) ;\nREMOVE:r2 (v) ;"
badrules = parseRules False "REMOVE:r1 (v) ;\nREMOVE:r2 (v) IF (-1C (det)) ;"

main = do
  args <- getArgs
  case args of
   []    -> mapM_ foo (concat randomrules)
   -- ["v"] -> do print "v" ; mapM_ (testRules True) [goodrules, badrules, randomrules]
   (r:o) -> undefined
      
type Clause = [Lit]

foo :: Rule -> IO [Clause]
foo rule = case rule of
  (Remove _ target cond) -> do print rule ; doStuff rmTarget (toTags target) (toConds cond)
  (Select _ target cond) -> do print rule ; doStuff slTarget (toTags target) (toConds cond)

--doStuff :: Bool -> [[Tag]] -> [[Condition]] -> IO [Clause]
doStuff rmOrSl target conds = do
  s <- newSolver

  lits <- sequence 
           [ sequence [ newLit s | _ <- globaltags ] | _ <- condsByInd ]
  -- e.g. [[v0,v1,v2,v3], [v4,v5,v6,v7]]
  -- where v0=word1_isDet , v1=word1_isN ...
  --       v4=word2_isDet , v5=word2_isN ...
  print lits
  print ti

  let condcls = zipWith slConds lits condsByInd
  print condcls

  let targetcls = condcls!!ti ++ rmOrSl (lits!!ti) (concat target)

  print targetcls
  return []
  where 
   condsAsTuples = sort $ (0,[]) `insert` map toTuple (concat conds) --for now, only AND in conds
   condsByInd = groupBy fstEq condsAsTuples
   ti = 999 `fromMaybe` findIndex (elem (0,[])) condsByInd

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

-- Not needed for now, but maybe later if we push the nondeterminism to the SAT solver side and work with multiple models.
solveMoreMax :: Solver -> [Lit] -> [Lit] -> IO [Bool]
solveMoreMax s as xs = do
  bs <- sequence [ modelValue s x | x <- xs ]
  a <- newLit s
  addClause s (neg a : [ if b == True then neg x else x | (x,b) <- xs `zip` bs ])
  b <- count s xs >>= solveMaximize s (a:as) -- give max True count in xs to solveMax
  addClause s [neg a] -- cleanup: remove (a:as) by adding ~a as unit clause
  if b then do
     sequence [ modelValue s x | x <- xs ]
   else return []
