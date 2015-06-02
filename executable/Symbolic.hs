import CG_base
import CG_SAT hiding (chunk)
import CG_parse
import Control.Monad
import Data.List
import Debug.Trace
import SAT
import SAT.Optimize
import SAT.Unary hiding (modelValue)
import System.Environment
import System.IO.Unsafe


n = 2

--dummytags = map (Tag . (:[])) ['b'..'e']
--dummyrules = parseRules False "REMOVE:r1 (d) IF (-1C (c) LINK 1 (e)) ;\nREMOVE:r2 (b) ;\nREMOVE:r3 (e) IF (-1C (c)) ;"

--TODO: get tags from the grammar to be tested
tags = map Tag ["det", "n", "v", "pri", "prs", "imp", "p3", "predet", "prn", "adj", "pr"]
--tags = map Tag ["det", "v", "p3", "imp", "prs"] -- , "predet", "prn"]

randomrules = parseRules False "REMOVE:r1 (v) IF (-1C (det)) ;\nREMOVE:r2 (prs) ;\nREMOVE:r3 (imp) IF (0 (p3)) ;"

goodrules = parseRules False "REMOVE:r1 (v) IF (-1C (det)) ;\nREMOVE:r2 (v) ;"
badrules = parseRules False "REMOVE:r1 (v) ;\nREMOVE:r2 (v) IF (-1C (det)) ;"

main = do
  args <- getArgs
  case args of
   []    -> mapM_ foo (concat goodrules)
   -- ["v"] -> do print "v" ; mapM_ (testRules True) [goodrules, badrules, randomrules]
   (r:o) -> undefined
      
type Clause = [Lit]

foo :: Rule -> IO [Clause]
foo rule = case rule of
  (Remove _ target cond) -> doStuff True (toTags target) (toConds cond)
  (Select _ target cond) -> doStuff False (toTags target) (toConds cond)

doStuff :: Bool -> [[Tag]] -> [[Condition]] -> IO [Clause]
doStuff isRemove target conds =
  undefined
 
chunk :: Sentence -> [(Integer,[Tag])]
chunk sent = concat $ go sent 1
   where go []    _m = []
         go (x:xs) m = map ((,) m) (addWF m x) : go xs (m+1)
         addWF m = map (WF ("w" ++ show m) :)

dechunk' :: [(Integer,[Tag])] -> Sentence
dechunk' ts = map (map snd) $ groupBy fstEq ts


fstEq (a,_) (b,_) = a==b

------------------------------------------------

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
