import CG_base
import CG_SAT hiding (chunk)
import CG_parse
import Data.List
import Debug.Trace
import SAT
import System.Environment

tagfile = "data/spa_tagcombs.txt"



lookup' :: [[Tag]] -> [Tag] -> [Int] --list of indices where the wanted taglist is found
lookup' tagcombs tagsInCond = 
  findIndices (\tc -> all (\t -> t `elem` tc) tagsInCond) tagcombs



goodrules = concat $ parseRules False "REMOVE:r1 (v) IF (-1C (det)) ;\nREMOVE:r2 (v) ;"
badrules = concat $ parseRules False "REMOVE:r1 (v) IF (0 (v)) ;\nREMOVE:r2 (v) IF (-1C (det)) ;"

main = do
  tc <- take 70 `fmap` map parse `fmap` words `fmap` readFile tagfile
  mapM_ print tc
  -- print $ length tc -- 2193
  -- print $ length $ lookup' tc [Tag "mf"] --1632

  args <- getArgs
  case args of
   []    -> do let spl = head $ splits (reverse goodrules)
               print spl
               uncurry (testRule tc) spl
   (r:o) -> do let verbose = "v" `elem` o
               rules <- concat `fmap` readRules r
               let spl = head $ splits (reverse rules)
               uncurry (testRule tc) spl

  where splits list = list >>= \x -> return (x, delete x list)

testRule tagcombs rule rules = do
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

  putStr $ "rule " ++ show rule
  putStrLn $ ": " ++ show (length cls) ++ " clauses"
  
  sequence_ [ {-print cl >> -} addClause s cl | cl <- cls ]
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
                    print $ length cl 
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
                             , ind <- lookup' tagcombs tags ]]
                ++
                [[neg(wn!!ind) | tags <- getTags' cond
                               , ind <- lookup' tagcombs tags ]] ---- generalise for >2 lits
           else [[ wn !! ind | tags <- getTags' cond
                             , ind <- lookup' tagcombs tags ]]

        slOrRm wn trg = let n = length tagcombs - 1 in
           [[ wn !! ind | tags <- getTags' trg --disjunction: >=1 context tag 
                       , ind  <- [0..n] \\ lookup' tagcombs tags ]] 
           ++
           [[ wn !! ind | tags <- getTags' trg
                         , ind  <- lookup' tagcombs tags ]] --unit clauses: readings to sl/rm
 

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
            let inds = map (concatMap (map (lookup' [[]]) . snd)) sswidth
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

slConds :: [[Tag]] -> [Lit] -> [(Int, [[Tag]])] -> [Clause]
slConds tc lits numsConds = 
  let lu    = zip tc lits
      conds = concatMap snd numsConds in  --concatMap won't work for contexts with OR
  [ [lit] | (tag, lit) <- lu
          , tag `elem` conds ]

rmTarget :: [[Tag]] -> [Lit] -> [(Int, [[Tag]])] -> [Clause]
rmTarget tc lits numsTargets = 
  let lu = zip tc lits 
      trgs = concatMap snd numsTargets in
  [ lit | (tag, lit) <- lu
        , tag `notElem` trgs ]
  :
  [ [neg lit] | (tag, lit) <- lu
              , tag `elem` trgs ]

slTarget :: [[Tag]] -> [Lit] -> [(Int, [[Tag]])] -> [Clause]
slTarget tc lits numsTargets = 
  let lu = zip tc lits 
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

parse :: String -> [Tag]
parse str = map toTag $ filter (not.null) $ split isValid str
  where isValid c = c=='<' || c=='+'
        toTag ">>>" = BOS
        toTag "<<<" = EOS
        toTag []    = error "empty tag"
        toTag str = if last str=='>' then Tag (init str) else Lem str

split :: (a -> Bool) -> [a] -> [[a]]
split p [] = []
split p xs = takeWhile (not . p) xs : split p (drop 1 (dropWhile (not . p) xs))