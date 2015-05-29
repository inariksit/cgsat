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

verbose = False

--tags = map (Tag . (:[])) ['b'..'e']
tags = map Tag ["det", "n", "v", "pri", "prs", "imp", "p3", "predet", "prn", "adj", "pr"]
--tags = map Tag ["prs", "imp", "p3", "v"]



--rules = parseRules False "REMOVE:r1 (d) IF (-1C (c) LINK 1 (e)) ;\nREMOVE:r2 (b) ;\nREMOVE:r3 (e) IF (-1C (c)) ;"
randomrules = parseRules False "REMOVE:r1 (v) IF (-1C (det)) ;\nREMOVE:r2 (prs) ;\nREMOVE:r3 (imp) IF (0 (p3)) ;"

goodrules = parseRules False "REMOVE:r1 (v) IF (-1C (det)) ;\nREMOVE:r2 (v) ;"
badrules = parseRules False "REMOVE:r1 (v) ;\nREMOVE:r2 (v) IF (-1C (det)) ;"

{- 
all subsequences of t should be considered for the position of one reading

symbolic must have at least one word with >1 reading, because of the requirement that applying R must make a difference

First rule: we must find some input so that it applies, no other requirements.
Second rule: we must find some input so that it applies, and first rule doesn't apply.

-----

First task: find such input that a rule will have effect, or prove there is none
1) Target    of the rule must be in the analysis
2) Condition of the rule must be in the context
-}


main = do
  args <- getArgs
  case args of
   []    -> mapM_ testRules [goodrules, badrules, randomrules]
   (r:_) -> readRules r >>= testRules
  
  {- If rules can only consist of one tag (not a list/set) in target and condition,
     is it enough that all readings in the symbolic sentence have just one tag?

     Number of readings is potentially |subsequences tags| (???), but can be smaller.
  -}       

testRules :: [[Rule]] -> IO ()
testRules rules = do
  putStrLn "Testing rules! So much fun!"
  let ruleAndPrevs = loop (concat rules) [] :: [(Rule, [Rule])]


  let chunkedSymbSent = chunk $ replicate n (map (:[]) tags)

  let ndSymbChunks = map concat $ sequence
       [ subsequences cohort | cohort <- groupBy fstEq chunkedSymbSent ]

  let allCombs rule prev = do
        chunks <- ndSymbChunks
        guard $ (not.null) chunks && length chunks < 4
        let potFst = unsafePerformIO $ constrain True verbose chunks prev
        guard $ all (==True) potFst
        let potSnd = unsafePerformIO $ constrain False verbose chunks rule
        guard $ any (==False) potSnd
        return $ dechunk' chunks
 

  let symbSents = [ ((rule,prev), allCombs rule prev)
                            | (rule, prevs) <- ruleAndPrevs
                            , prev <- prevs ] :: [((Rule,Rule),[Sentence])]
  
  putStrLn $ "combinations:  " ++ show (length symbSents)
  putStrLn $ "all solutions: " ++ show (sum $ map (length.snd) symbSents)
  mapM_ pr symbSents
  putStrLn "\n--------\n"

  where loop []     _     = [] 
        loop (r:rs) prevs = (r, prevs) : loop rs (r:prevs)

        pr ((rl,prev), symbs) = do
           putStrLn $ "\n1) `" ++ show prev ++ "'"
           putStrLn $ "2) `" ++ show rl ++ "'"
           if (null symbs) 
             then do
               putStrLn "is a conflicting sequence" 
             else do
               putStrLn $ "is satisfied by following sentences:"
               putStrLn $ showSentence (head symbs)
               putStrLn $ "and " ++ (show $ length $ tail symbs) ++ " others\n"



constrain :: Bool -> Bool -> [(Integer,[Tag])] -> Rule -> IO [Bool]
constrain isPrev verbose chunks rule = do
  --print chunks
  --later:
  -- let n = length $ concat . toConds $ cond rule
  -- print n
  -- let chunkedSymbSent = chunk $ replicate n (map (:[]) tags)

  -- let ndSymbToks = map concat $ sequence
  --      [ (not.null) `filter` subsequences cohort 
  --           | cohort <- groupBy (\(a,_) (b,_) -> a==b) chunkedSymbSent ]

  s <- newSolver
  lits <- sequence [ newLit s | _ <- chunks ]
  let toks = zip chunks lits :: [Token]
  sequence_ [ do when verbose $ print cl ; addClause s cl 
                | cl <- anchor toks ]

  sequence_ [ do when verbose $ print cl ; addClause s cl 
                | cl <-  applyRule rule toks ]

  lt <- count s lits
  b <- solveMaximize s [] lt 
  as <- sequence [ modelValue s x | x <- lits ]


  when verbose $ do
        putStrLn "---------------------"
        putStr $ if isPrev then "prev: " else "rule: "
        print rule
        let alltoks = [ ((i, (WF w:((Tag (sc++t)):ts))), lit) 
                | (b, ((i, (WF w:((Tag      t ):ts))), lit)) <- zip as toks                , let sc = if b then "" else "; " ]
        putStrLn $ showSentence (dechunk alltoks)


  return as 

--  let truetoks = [ t | (True, t) <- zip as toks ] 
  



solveMore :: Solver -> [Lit] -> [Lit] -> IO [Bool]
solveMore s as xs = do
  bs <- sequence [ modelValue s x | x <- xs ]
  a <- newLit s
  addClause s (neg a : [ if b == True then neg x else x | (x,b) <- xs `zip` bs ])
  b <- count s xs >>= solveMaximize s (a:as) -- give max True count in xs to solveMax
  addClause s [neg a] -- cleanup: remove (a:as) by adding ~a as unit clause
  if b then do
     sequence [ modelValue s x | x <- xs ]
   else return []

  
chunk :: Sentence -> [(Integer,[Tag])]
chunk sent = concat $ go sent 1
   where go []    _m = []
         go (x:xs) m = map ((,) m) (addWF m x) : go xs (m+1)
         addWF m = map (WF ("w" ++ show m) :)

dechunk' :: [(Integer,[Tag])] -> Sentence
dechunk' ts = map (map snd) $ groupBy fstEq ts


fstEq (a,_) (b,_) = a==b
------------------------------------------------


{- general design:

do
  tags_word1 <- choose from [ ["w1", a], ["w1", a, b], ["w1", b], ... ]
  tags_word2 <- choose from [ ["w2", c], ["w2", c, d], ["w2", c], ... ]
  ...
  tags_wordN <- choose from [ ... ]

  let sentence = tags_word1 ++ ... ++ tags_wordN
  
  --  for each pair of rules:
  newsent <- constrain sentence rule1
  newersent <- constrain newsent rule2
  guard $ all (==True) newsent && any (==False) newersent


TODO: why are you not working? ;____; e.g.

[~v430,~v3]
1010
REMOVE:r2 b  IF (0 (*))
"<w1>"
        c
"<w2>"
        ; b
        c
        ; e
-}

