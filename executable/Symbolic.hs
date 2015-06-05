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


--globaltags = map Tag ["det", "n", "v", "pri", "prs", "imp", "p3", "predet", "prn", "adj", "pr"]
tagcombs = [ [Tag "det", Tag "def"], [Tag "det", Tag "indef"] ] ++
           [ [Tag "v", Tag m, Tag p] | m <- ["imp","prs"]
                                     , p <- ["p3","p1"] ]

lookup' :: [[Tag]] -> [Tag] -> [Int] --list of indices where the wanted taglist is found
lookup' allTagCombs tagsInCond = 
  findIndices (\tc -> all (\t -> t `elem` tc) tagsInCond) allTagCombs

randomrules = concat $ parseRules False "LIST Person = (p1) (p3) ;\n REMOVE:r1 (v p1) IF (-1C (det)) (1 def);\nREMOVE:r2 (prs) ;\n REMOVE:r3 (imp) IF (0 (p3)) ;\nSELECT:s4 (v) IF (-1 det) (1 Person) ;\nREMOVE:r5 (p3) IF (-2 det) (-1 v) (1 imp) (5 prs) ;"

goodrules = concat $ parseRules False "REMOVE:r1 (v) IF (-1C (det)) ;\nREMOVE:r2 (v) ;"
badrules = concat $ parseRules False "REMOVE:r1 (v) ;\nREMOVE:r2 (v) IF (-1C (det)) ;"

main = do
  args <- getArgs
  case args of
   []    -> do s <- newSolver
               let testedRuleWidth = width (head randomrules)
               allLits <- sequence 
                       [ sequence [ newLit s | _ <- tagcombs ]
                                             | _ <- testedRuleWidth ]
               let symbolicSentence = concat
                    [ [ ((m, addWF m tags),lit) | (lit, tags) <- zip lits tagcombs ]
                                         | lits <- allLits ,
                                           m <- [1..length allLits] :: [Int]
                                         ] -- :: [Token]

               putStrLn $ showSentence (dechunk symbolicSentence)
               print symbolicSentence
               print allLits
               stuff <- mapM (checkIfApplies s allLits) (tail randomrules)
               print stuff
               print tagcombs

   (r:o) -> undefined
   where 
         addWF m = (WF ("w" ++ show m) :)


width :: Rule -> [[(Int,[[Tag]])]]
width rule = case rule of
  (Select _ target conds) -> doStuff target conds
  (Remove _ target conds) -> doStuff target conds
  where doStuff t cs = groupBy fstEq $ nub $ sort $ fill $ (0, toTags t) `insert` map toTuple (concat (toConds cs))


checkIfApplies ::  Solver
                  -> [[Lit]]
                  -> Rule 
                  -> IO [Clause]
checkIfApplies s lits rule = do
  if length lits >= length sswidth 
    then do putStrLn "sufficiently long sentence"
            let inds = map (concatMap (map (lookup' tagcombs) . snd)) sswidth
            print inds --e.g. [[[0,1]],[[2,3,4,5]],[[3,5],[2,4]]]
            --for lit in lits:
            --
            print sswidth
            return []
    else do putStrLn "conditions out of scope"
            return []
  where sswidth = width rule

{--

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

      
type Clause = [Lit]

sh :: Bool -> Char
sh True = '1' 
sh False = '0'

slConds :: [Lit] -> [(Int, [[Tag]])] -> [Clause]
slConds lits numsConds = 
  let lu    = zip tagcombs lits
      conds = concatMap snd numsConds in  --concatMap won't work for contexts with OR
  [ [lit] | (tag, lit) <- lu
          , tag `elem` conds ]

rmTarget :: [Lit] -> [[Tag]] -> [Clause]
rmTarget lits targets = let lu = zip tagcombs lits in
  [ lit | (tag, lit) <- lu
        , tag `notElem` targets ]
  :
  [ [neg lit] | (tag, lit) <- lu
              , tag `elem` targets ]

slTarget :: [Lit] -> [[Tag]] -> [Clause]
slTarget lits targets = let lu = zip tagcombs lits in
  [ [lit] | (tag, lit) <- lu
          , tag `elem` targets ]
  ++
  [ [neg lit] | (tag, lit) <- lu
              , tag `notElem` targets ]

--------------------------------------------------------------------------------
--TODO: negative condition
toTuple (C pos (_b,tags)) = (n, toTags tags)
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
           where filled = [ (k,[[]]) | k <- [m+1..n-1] ]



--------------------------------------------------------------------------------

chunk :: Sentence -> [(Int,[Tag])]
chunk sent = concat $ go sent 1
   where go []    _m = []
         go (x:xs) m = map ((,) m) (addWF m x) : go xs (m+1)
         addWF m = map (WF ("w" ++ show m) :)

dechunk' :: [(Int,[Tag])] -> Sentence
dechunk' ts = map (map snd) $ groupBy fstEq ts


fstEq (a,_) (b,_) = a==b

--------------------------------------------------------------------------------

