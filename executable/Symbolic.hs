import CG_base
import CG_SAT hiding (chunk)
import CG_parse
import Data.List
import Debug.Trace
import SAT
import System.Environment


--globaltags = map Tag ["det", "n", "v", "pri", "prs", "imp", "p3", "predet", "prn", "adj", "pr"]
tagcombs = [ [Tag "det", Tag "def"], [Tag "det", Tag "indef"] ] ++
           [ [Tag "v", Tag m, Tag p] | m <- ["imp","prs"]
                                     , p <- ["p3","p1","p2"] ]

lookup' :: [Tag] -> [Int] --list of indices where the wanted taglist is found
lookup' tagsInCond = 
  findIndices (\tc -> all (\t -> t `elem` tc) tagsInCond) tagcombs

randomrules = concat $ parseRules False "LIST Det = (det def) (det indef) ; \n REMOVE:r1 (v) IF (-1C (det));\nREMOVE:r2 (prs) ;\n REMOVE:r3 (imp p2) IF (0 (p3)) ;\nREMOVE:s4 (v) IF (1 p1) ;" -- \nREMOVE:r5 (p3) IF (-2 det) (-1 v) (1 imp) (5 prs) ;"

goodrules = concat $ parseRules False "REMOVE:r1 (v) IF (-1C (det)) ;\nREMOVE:r2 (v) ;"
badrules = concat $ parseRules False "REMOVE:r1 (v) IF (0 (v)) ;\nREMOVE:r2 (v) IF (-1C (det)) ;"

main = do
  args <- getArgs
  case args of
   []    -> do let spl = head $ splits (reverse randomrules)
               print spl
               uncurry testRule spl
   (r:o) -> undefined

  where splits list = list >>= \x -> return (x, delete x list)

testRule rule rules = do
  s <- newSolver
  let ruleWidth = width rule
  allLits <- sequence 
              [ sequence [ newLit s | _ <- tagcombs ] | _ <- ruleWidth ]
  let ss = concat
             [ [ ((m, addWF m tags),lit) | (lit, tags) <- zip lits tagcombs ]
                   | (lits, m) <- zip allLits [1..length allLits]
             ]  :: [Token]
               
  let cls = concat [ f wn cond | (wn, cond) <- zip allLits ruleWidth 
                              , let f = if isTarget cond 
                                            then slOrRm
                                            else slCond ]

  putStrLn $ "rule: " ++ show rule
  putStrLn $ "cls: " ++ show cls
  
  sequence_ [ print cl >> addClause s cl | cl <- cls ]
  b <- solve s []
  print b

  putStrLn "\n---------\n"

  let applied = nub [ (rl, cl) | rl <- rules
                               , cl <- applyRule rl ss 
                               , (not.null) cl ] :: [(Rule, [Lit])]

--  print applied
  sequence_ [ do addClause s cl
                 b <- solve s []
                 if True then do
                    putStr $ show rl ++ ": "
                    print cl 
                    as <- sequence [ modelValue s x | x <- concat allLits ]
                    putStrLn $ map sh as
                 else return ()
                 print b | (rl,cl) <- applied ]
  b <- solve s []
  print b
  as <- sequence [ modelValue s x | x <- concat allLits ]
  let truetoks = [ t | (True, t) <- zip as ss ]
  putStrLn $ showSentence (dechunk truetoks)

  putStrLn "\n---------\n"

  -- stuff <- mapM (checkIfApplies s allLits) (tail randomrules)
  -- print stuff
  -- print tagcombs

  where addWF m = (WF ("w" ++ show m) :)

        isC (((pos,b),_):_) = b

        slCond wn cond = if isC cond 
           then [[ wn !! ind | tags <- getTags' cond
                             , ind <- lookup' tags ]]
                ++
                [[neg(wn!!ind) | tags <- getTags' cond
                               , ind <- lookup' tags ]] ---- generalise for >2 lits
           else [[ wn !! ind | tags <- getTags' cond
                             , ind <- lookup' tags ]]

        slOrRm wn trg = let n = length tagcombs - 1 in
           [[ wn !! ind | tags <- getTags' trg --disjunction: >=1 context tag 
                       , ind  <- [0..n] \\ lookup' tags ]] 
           ++
           [[ wn !! ind | tags <- getTags' trg
                         , ind  <- lookup' tags ]] --unit clauses: readings to sl/rm
 

         --getTags' :: [((Int,Bool), [[Tag]])] -> [[Tag]]
        getTags' []           = []
        getTags' ((_,ts):its) = ts ++ getTags' its

        isTarget []        = False
        isTarget (((i,_),_):_) = i==0

width :: Rule -> [[((Int,Bool),[[Tag]])]]
width rule = case rule of
  (Select _ target conds) -> doStuff target conds
  (Remove _ target conds) -> doStuff target conds
  where doStuff t cs = groupBy fstEq $ nub $ sort $ fill $ ((0,False), toTags t) `insert` map toTuple (concat (toConds cs))


checkIfApplies ::  Solver
                  -> [[Lit]]
                  -> Rule 
                  -> IO [Clause]
checkIfApplies s lits rule = do
  if length lits >= length sswidth 
    then do putStrLn "sufficiently long sentence"
            let inds = map (concatMap (map lookup' . snd)) sswidth
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

rmTarget :: [Lit] -> [(Int, [[Tag]])] -> [Clause]
rmTarget lits numsTargets = 
  let lu = zip tagcombs lits 
      trgs = concatMap snd numsTargets in
  [ lit | (tag, lit) <- lu
        , tag `notElem` trgs ]
  :
  [ [neg lit] | (tag, lit) <- lu
              , tag `elem` trgs ]

slTarget :: [Lit] -> [(Int, [[Tag]])] -> [Clause]
slTarget lits numsTargets = 
  let lu = zip tagcombs lits 
      trgs = concatMap snd numsTargets in
  [ [lit] | (tag, lit) <- lu
          , tag `elem` trgs ]
  ++
  [ [neg lit] | (tag, lit) <- lu
              , tag `notElem` trgs ]

--------------------------------------------------------------------------------
--TODO: negative condition
toTuple :: Condition -> ((Int,Bool),[[Tag]]) --bool: cautious or not
toTuple Always            = ((0,False), [[]])
toTuple (C pos (_b,tags)) = ((n,b), toTags tags)
  where (n,b) = case pos of
                 Exactly b i -> (i,b)
                 AtLeast b i -> (i,b)
                 Barrier i _ -> (i,False)

-- x:xs must be sorted
fill [] = []
fill (x@((n,b),_):xs) = go (x:xs) n []
  where go []           _m res = reverse res
        go (x@((n,b),_):xs) m  res
           | n-m == 1 ||
               n-m == 0 = go xs n (x:res)
           | otherwise  = go xs n (x : (filled++res))
           where filled = [ ((k,False), [[]]) | k <- [m+1..n-1] ]



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

