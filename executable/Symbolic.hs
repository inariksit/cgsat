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

randomrules = parseRules False "REMOVE:r1 (v) IF (-1C (det)) ;\nREMOVE:r2 (prs) ;\nREMOVE:r3 (imp) IF (0 (p3)) ;"

goodrules = parseRules False "REMOVE:r1 (v) IF (-1C (det)) ;\nREMOVE:r2 (v) ;"
badrules = parseRules False "REMOVE:r1 (v) ;\nREMOVE:r2 (v) IF (-1C (det)) ;"

main = do
  args <- getArgs
  case args of
   []    -> mapM_ (testRules False) [goodrules, badrules, randomrules]
   ["v"] -> do print "v" ; mapM_ (testRules True) [goodrules, badrules, randomrules]
   (r:o) -> do let verbose = "v" `elem` o
               readRules r >>= testRules verbose
      

testRules :: Bool -> [[Rule]] -> IO ()
testRules verbose rules = do
  --later: let n and tags be determined by the rule(s)
  putStrLn "Testing rules! So much fun!"

  let symbChunks = chunk $ replicate n (map (:[]) tags)

  let ndSymbChunks = map concat $ sequence
        [ filter (not.null) $ subsequences cohort 
             | cohort <- groupBy fstEq symbChunks ]

  let allCombs rule prev = do
        chunks <- ndSymbChunks
                 
        guard $ n < length chunks && --at least one word has >1 analyses
                    length chunks < n+3 --not too many or it gets slow :-P
        let potFst = unsafePerformIO $ constrain True verbose chunks prev
        guard $ all (==True) potFst --first rule doesn't remove anything
        let potSnd = unsafePerformIO $ constrain False verbose chunks rule
        guard $ any (==False) potSnd --second rule removes something
        return $ dechunk' chunks
 

  let symbSents = [ ((rule,prev), allCombs rule prev)
                            | (rule, prevs) <- loop (concat rules) []
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


  when verbose $
   do putStrLn "---------------------"
      putStr $ if isPrev then "prev: " else "rule: "
      putStrLn $ show rule
      let alltoks = [ ((i, (WF w:((Tag (sc++t)):ts))), lit) 
                | (b, ((i, (WF w:((Tag      t ):ts))), lit)) <- zip as toks
                , let sc = if b then "" else "; " ]
      putStrLn $ showSentence (dechunk alltoks)

  return as

  
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
