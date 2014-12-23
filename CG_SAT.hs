module CG_SAT where 

import CG
import Data.Boolean.SatSolver
import Data.List
import Control.Applicative
import Control.Monad


type Literal = ((Integer, Tag), Boolean)

instance Eq Boolean where
  Var n      == Var m        = m==n
  (n :||: m) == (n' :||: m') = n==n' && m==m'
  (n :&&: m) == (n' :&&: m') = n==n' && m==m'

-- SAT stuff

mkSymbols :: [[a]] -> [(Integer,a)]
mkSymbols as = concat $ go as 1
    where go []     n = []
          go (x:xs) n = map (\tag -> (n,tag)) x : go xs (n+1)

-- [((1,Art),Var 1),((2,N),Var 2),((2,V),Var 3),((3,N),Var 4),((3,V),Var 5)]
mkLiterals :: [b] -> [(b,Boolean)]
mkLiterals bs = go bs 1
    where go []     n = []
          go (x:xs) n = (x,Var n) : go xs (n+1)


-- 1) If something is unambiguous to start with, anchor that
anchor :: [Literal] -> [Boolean]
anchor lits = [bool | ((int, tag), bool) <- lits, 
                      isUniq int indices]
   where indices = map (\((int, _), _) -> int) lits
         isUniq x xs = length (findIndices (==x) xs) == 1

-- 2) Take all possible bigrams
mkBigrams :: [Literal] -> [Boolean]
mkBigrams lits = zipWith mkCombs  pres posts --[lits] [lits] -- 
    where len = length lits
          pres = take (len-1) $ groupBy number lits
          posts = tail $ groupBy number lits
          number ((n1,_),_) ((n2,_),_) = n1==n2 

mkCombs :: [Literal] -> [Literal] -> Boolean
mkCombs pre post = foldr1 (:||:) combinations
    where combinations = [bool1 :&&: bool2 | ((_, _), bool1) <- pre, 
                                             ((int, tag), bool2) <- post,
                                             bool1 /= bool2]


-- 3) Apply rules to literals
applyRule :: Rule -> [Literal] -> [Boolean]
applyRule rule@(Remove tags (C pre post)) lits = case rule of
               (Remove tags (C pre []))  -> [Not bool1 :||: Not bool2 
                                                  | ((n1, _), bool1) <- possiblyPre,
                                                    ((n2, _), bool2) <- possiblyTags,
                                                    n2-n1 == 1] --take only consecutive elements
               (Remove tags (C [] post)) -> [Not bool1 :||: Not bool2 
                                                  | ((n1, _), bool1) <- possiblyPost,
                                                    ((n2, _), bool2) <- possiblyTags,
                                                    n1-n2 == 1] --context comes after tag
               (Select tags (C [] post)) -> [Not bool1 :||: bool2
                                                  | ((n1, _), bool1) <- possiblyPost,
                                                    ((n2, _), bool2) <- possiblyTags,
                                                    n1-n2 == 1] 
   where possiblyTags = filter (\((int, tag), bool) -> tag `elem` tags) lits
         possiblyPre = filter (\((int, tag), bool) -> tag `elem` pre) lits
         possiblyPost = filter (\((int, tag), bool) -> tag `elem` post) lits




-- Test with example sentence [[Art], [N,V], [N,V], [Prep,Adv], [Det], [N,V]]
-- possible application: grammar writer wants to try two combinations of tag sets, would explode in the normal implementation, but sat solver would not be overkilol


-- Two simplifications: drop the lemma & allow only one tag in each analysis
ex1 = [ [Det]   --the
      , [N,V]   --bear
      , [N,V]   --sleeps
      , [Prep,Adv] --in
      , [Det]     --the
      , [N,V] ]   --house

lits = mkLiterals $ mkSymbols ex1

formulae = concat $ [anchor, mkBigrams, applyRule slNoun, applyRule rmVerb, applyRule rmAdv] <*> [lits]

notmain :: IO ()
notmain = do 
   solver <- foldM (flip assertTrue) newSatSolver formulae
   --print solver
   solution <- solve solver
   print solution
   let truetags = filter (\(_,(Var int)) ->lookupVar int solution == Just True) lits
   putStrLn "\nTag sequence:"
   mapM_ putStrLn $ map showTag truetags


showTag :: (Show t, Num t) => ((t, Tag), Boolean) -> String
showTag ((t,tag),_) = show t ++ ": " ++ show tag

{-
Literals:
0art, 1n, 1v, 2n, 2v

Clauses:
-- If something is unambiguous to start with, anchor that:

0art
0art & 1n | 0art & 1v
1n   & 2n | 1n   & 2v | 1v & 2n | 1v & 2v

rules:
  Remove verb (C art [])

translates into 
  (x-1 art) => ~(x verb)

and in CNF:
  ~(x-1 art) | ~(x verb)

for all indices x

  ~0art | ~1v


  Select noun (C [] verb)

translates into
  (x+1 verb) => x noun  

and in CNF
  ~(x+1 verb) | x noun

for all indices x:

  ~2v | 1n

Whole rule set:

0art
0art & 1n | 0art & 1v
1n   & 2n | 1n   & 2v | 1v & 2n | 1v & 2v
~0art | ~1v
~2v | 1n

-}