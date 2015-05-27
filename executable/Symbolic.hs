import CG_base
import CG_SAT hiding (chunk)
import CG_parse
import Control.Monad
import Data.List
import SAT
import SAT.Optimize
import SAT.Unary hiding (modelValue)

n = 2

--tags = map Tag ["det", "n", "v", "pri", "prs", "imp", "p3", "sg", "pl"]
tags = map Tag ["b","c","d","e"]


rules = parseRules False "REMOVE:r1 (d) IF (-1C (c)) ;\nREMOVE:r2 (b) ;\nREMOVE:r3 (e) IF (-1C (c)) ;"
--rules = parseRules False "REMOVE:r1 (v) IF (-1C (det)) ;\nREMOVE:r2 (prs) ;\nREMOVE:r3 (imp) IF (0 (p3)) ;"

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
  s <- newSolver
  t <- sequence [ newLit s | _ <- tags
                           , _ <- [1..n] ] -- # of words in sentence

  {- If rules can only consist of one tag (not a list/set) in target and condition,
     is it enough that all readings in the symbolic sentence have just one tag?

     Number of readings is potentially |subsequences tags| (???), but can be smaller.
  -}

  let chunkedSymbolicSent = chunk $ replicate n (map (:[]) tags)
      symbolicToks = zip chunkedSymbolicSent t :: [Token]

  possibletoks <- constrain s symbolicToks ((concat rules) !! 1) :: IO [[Token]]
  moretoks <- sequence [ constrain s toks ((concat rules) !! 0) | toks <- possibletoks ]
  putStrLn "end"
--  print moretoks

constrain :: Solver -> [Token] -> Rule -> IO [[Token]]
constrain s toks rule = do

  let lits = map getLit toks
  putStrLn "---------------------"

  let allNotFalse = anchor toks :: [[Lit]]
  print allNotFalse
  sequence_ [addClause s cl | cl <- allNotFalse ]
  sequence_ [print cl >> addClause s cl | cl <- applyRule rule toks ]

  lt <- count s lits
  b <- solveMaximize s [] lt
  as <- sequence [ modelValue s x | x <- lits ]

  -- Get all possible maximal models.
  possiblebs <- sequence [ solveMore s [] lits | _ <- lits ]
{-solveMore maximises, so there can be a model for each lit in lits to be False. 
  (Not applicable if max model has >1 False value. Works for my ad hoc example.) -}
  let truecounts = reverse $ sort [ (length $ filter (==True) bs, bs) 
                                            | bs <- (as:possiblebs) ]
      maxbs = nub $ map snd $ head $ groupBy (\(a,_) (b,_) -> a==b) truecounts
  --print truecounts
  sequence_ [ putStrLn [ if b == True then '1' else '0' |  b <- bs ] | bs <- maxbs ]

  

  let pr bs = do 
        let alltoks = [ ((i, (WF w:((Tag (sc++t)):ts))), lit) 
                  | (b, ((i, (WF w:((Tag      t ):ts))), lit)) <- zip bs toks 
                  , let sc = if b then "" else "; " ]
        putStrLn $ showSentence (dechunk alltoks)
        putStrLn "-----"
  let truetoks = [ [ t | (True, t) <- zip bs toks ] | bs <- maxbs ]

  print rule
  mapM_ pr maxbs
  return truetoks

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