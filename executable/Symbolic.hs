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

tagcfile = "data/spa_tagcombs.txt"
tagfile = "data/spa_tags.txt"


lookup' :: [[Tag]] -> [Tag] -> [Int] --list of indices where the wanted taglist is found
-- For empty tagsInCond, returns all indices in tagcombs.
-- This is wanted behaviour; when filling conditions with empty spaces, 
-- those spaces can have any tag.
-- TODO: Barrier case
lookup' tagcombs tagsInCond = --trace ("lookup': " ++ show tagsInCond) $
  findIndices (\tc -> all (\t -> t `elem` tc) tagsInCond) tagcombs


testrules = concat $ parseRules False "REMOVE:r1 (aa) IF (-1C (mf)) ;\nREMOVE:r2 (aa) IF (-1C (acr)) ;"

main = do
  ts <- filter (not.null) `fmap` map parse `fmap` words `fmap` readFile tagfile :: IO [[Tag]]
  print ts
  tc <- take 2191 `fmap` map parse `fmap` words `fmap` readFile tagcfile
  -- mapM_ print tc
  -- print $ length tc -- 2191
  -- print $ length $ lookup' tc [Tag "mf"] --1632

  args <- getArgs
  case args of
   []    -> do putStrLn "test"
               let spl = splits testrules
               print spl
               mapM_ (uncurry (testRule True ts tc)) spl
   (r:o) -> do let verbose = "v" `elem` o
               rules <- concat `fmap` readRules r
               let spl = head $ splits (reverse rules)
               print spl
               uncurry (testRule verbose ts tc) spl

  where splits list = list >>= \x -> return (x, delete x list)

testRule verbose alltags tagcombs rule rules = do
  putStrLn $ "Testing with " ++ show rule ++ " as the last rule"
  s <- newSolver
  let ruleWidth = width rule alltags
  allLits <- sequence 
              [ sequence [ newLit s | _ <- tagcombs ] | _ <- ruleWidth ]
  let ss = concat
             [ [ ((m, addWF m tags),lit) | (lit, tags) <- zip lits tagcombs ]
                   | (lits, m) <- zip allLits [1..length allLits]
             ]  :: [Token]
      

  cls <- concat `fmap` sequence [ f wn cond | (wn, cond) <- zip allLits ruleWidth 
                               , let f = if isTarget cond 
                                            then slOrRm
                                            else slCond s ]

  putStr $ "rule " ++ show rule
  putStrLn $ ": " ++ show (length cls) ++ " clauses"
  
  sequence_ [  addClause s cl | cl <- cls ]

  b <- solve s []
  print b

  putStrLn "\n---------\n"

  applied <- sequence [ applyRule s rl ss 
                          | rl <- rules
                          , length (width rl alltags) <= length ruleWidth ]


                               -- , cl <- cl'
                               -- , (not.null) cl ] :: [(Rule, [Lit])]

--  print applied
  sequence_ [ do addClause s cl
                 --b <- solve s []
                 when verbose $ do
                   --putStrLn $ show rl ++ ": " ++ show cl 
                   printFancy $ show rl ++ ": " ++ show cl
                   -- when True $ do
                   --   as <- sequence [ modelValue s x | x <- concat allLits ]
                   --   printFancy (map sh as)
                   --putStrLn $ "Solution after prev clause: " ++ show b 
                 | (rl,cl') <- zip rules applied 
                 , cl <- cl' ]
  b <- solve s []
  putStr "Solution after all clauses: "
  print b
  if b then do
    as <- sequence [ modelValue s x | x <- concat allLits ]
    let truetoks = [ t | (True, t) <- zip as ss ]
    putStrLn $ showSentence (dechunk truetoks)
       else print "bad D:"
  putStrLn "\n---------\n"

  -- stuff <- mapM (checkIfApplies s allLits) (tail randomrules)
  -- print stuff
  -- print tagcombs

 where 
  addWF m = (WF ("w" ++ show m) :)

  sh True = '1' 
  sh False = '0'

  isC (((pos,b),_):_) = b

  -- conditions for one word; there can be many but the index is the same
  slCond :: Solver -> [Lit] -> [((Int,Cautious), [[Tag]])] -> IO [Clause]
  slCond s wn cond = trace ("slCond: " ++ show (concatMap (\(_,ts) -> ts) cond)) $
   do
    let n = length tagcombs - 1 
        tagsInCond = concatMap snd cond :: [[Tag]]
    newlits <- sequence [ newLit s | _ <- tagsInCond ]
    let f = if isC cond then disjunctionC else disjunction
    let disjs = filter notNegUnit $ nub $ concat
              [ f n nl tags | (nl, tags) <- zip newlits tagsInCond ]
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

  slOrRm wn trg = do
    let n = length tagcombs - 1 
    return $ [[ wn !! ind | tags <- concatMap snd trg --disjunction: >=1 context tag 
                          , ind  <- [0..n] \\ lookup' tagcombs tags ]] 
             ++ 
             [[ wn !! ind | tags <- concatMap snd trg
                          , ind  <- lookup' tagcombs tags ]] --readings to select/remove
 

  isTarget []            = False
  isTarget (((i,_),_):_) = i==0

--------------------------------------------------------------------------------
      
type Clause = [Lit]

width :: Rule -> [[Tag]] -> [[((Int,Cautious),[[Tag]])]]
width rule alltags = case rule of
  (Select _ target Always) -> [[((0,False), toTags target)]]
  (Remove _ target Always) -> [[((0,False), toTags target)]]
  (Select _ target conds) -> doStuff target conds
  (Remove _ target conds) -> doStuff target conds
 where 
  doStuff t cs = 
    groupBy fstEq $ nub $ sort $ fill $ 
      ((0,False), toTags t) `insert` map (toTuple alltags) (concat (toConds cs))


--------------------------------------------------------------------------------

toTuple :: [[Tag]] -> Condition -> ((Int,Cautious),[[Tag]])
toTuple _ Always              = error "toTuple applied to Always: this should not happen"
toTuple _ (C pos (True,tags)) = ((n,b), toTags tags)
 where 
  (n,b) = case pos of
                 Exactly b i -> (i,b)
                 AtLeast b i -> (i,b)
                 Barrier i _ -> (i,False)
toTuple alltags (C pos (False,tags)) = trace ("toTuple neg: " ++ show tags ++ "\\ " ++ show complement) $ ((n,b), complement)
 where 
  (n,b) = case pos of
                 Exactly b i -> (i,False)
                 AtLeast b i -> (i,False)
                 Barrier i _ -> (i,False)
  complement = alltags \\ toTags tags


-- x:xs must be sorted
fill [] = []
fill (x@((n,b),_):xs) = go (x:xs) n []
 where
  go []              _m res = reverse res
  go (x@((n,b),_):xs) m res | n-m == 1 || n-m == 0 = go xs n (x:res)
                            | otherwise            = go xs n (x : (filled++res))
           where filled = [ ((k,False), [[]]) | k <- [m+1..n-1] ]



--------------------------------------------------------------------------------

chunk :: Sentence -> [(Int,[Tag])]
chunk sent = concat $ go sent 1
   where go []    _m = []
         go (x:xs) m = map ((,) m) (addWF m x) : go xs (m+1)
         addWF m = map (WF ("w" ++ show m) :)

dechunk' :: [(Int,[Tag])] -> Sentence
dechunk' ts = map (map snd) $ groupBy fstEq ts

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

{--

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