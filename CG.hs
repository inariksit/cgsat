import Data.Boolean.SatSolver
import Data.List
import Control.Applicative
import Control.Monad

{-
Version 0.1: 
 - only tags matter, not lemmas
 - only one tag per analysis (for no other reason than my laziness when doing manual examples)
 - only 1 token before or after
-}

data Tag = Art | Adj | Adv | Det | N | PN | V | V2 | VV | Sg | Particle | Pl | Prep | P1 | P2 | P3 | Subj | Imper | Cond | Inf deriving (Show,Read,Eq)

data Analysis = A {s :: String, t :: [Tag]} deriving (Show,Read,Eq)


--type Sentence = [[Analysis]] --doesn't apply for now

data Context = C {prev :: [Tag], next :: [Tag]} deriving (Show)

data Rule = Select [Tag] Context | Remove [Tag] Context deriving (Show)

type Literal = ((Integer, Tag), Boolean)

instance Eq Boolean where
  Var n      == Var m        = m==n
  (n :||: m) == (n' :||: m') = n==n' && m==m'
  (n :&&: m) == (n' :&&: m') = n==n' && m==m'


-- Sets of tags
verb = [V,V2,VV]
noun = [N,PN]
det  = [Art,Det]
adv  = [Adv,Particle]

-- Rules
rmVerb = Remove verb (C det [])
slNoun = Select noun (C [] verb)
rmAdv = Remove adv (C [] det)

-- Analyses
the = A "the" [Det]
bear = A "bear" [N,Sg] : [A "bear" [V,y,z] | y <- [Sg,Pl], z <- [P1,P2]] -- w <- [Subj,Imper,Cond] and so on
sleeps = [A "sleep" [N,Pl],
          A "sleep" [V,Sg,P3]]
in_ = [A "in" [Prep],
       A "in" [Adv]]

-- Sentence
ex0 = [ [A "the" [Art]] --unambiguous
      , bear
      , sleeps ]


-- Two simplifications: drop the lemma & allow only one tag in each analysis
ex1 = [ [Det]
      , [N,V]
      , [N,V] 
      , [Prep,Adv]
      , [Det]
      , [N,V] ]


-- 

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
mkBigrams lits = zipWith mkCombs pres posts --[lits] [lits]
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
applyRule (Remove tags (C pre [])) lits = [Not bool1 :||: Not bool2 
                                            | ((n1, _), bool1) <- possiblyPre,
                                              ((n2, _), bool2) <- possiblyTags,
                                              n2-n1 == 1] --take only consecutive elements
   where possiblyTags = filter (\((int, tag), bool) -> tag `elem` tags) lits
         possiblyPre = filter (\((int, tag), bool) -> tag `elem` pre) lits

applyRule (Remove tags (C [] post)) lits = [Not bool1 :||: Not bool2 
                                            | ((n1, _), bool1) <- possiblyPost,
                                              ((n2, _), bool2) <- possiblyTags,
                                              n1-n2 == 1] --context comes after tag
   where possiblyTags = filter (\((int, tag), bool) -> tag `elem` tags) lits
         possiblyPost = filter (\((int, tag), bool) -> tag `elem` post) lits

applyRule (Select tags (C [] post)) lits = [Not bool1 :||: bool2
                                            | ((n1, _), bool1) <- possiblyPost,
                                              ((n2, _), bool2) <- possiblyTags,
                                              n1-n2 == 1] 
   where possiblyTags = filter (\((int, tag), bool) -> tag `elem` tags) lits
         possiblyPost = filter (\((int, tag), bool) -> tag `elem` post) lits



-- Test with example sentence [[Art], [N,V], [N,V]]

lits = mkLiterals $ mkSymbols ex1

formulae = concat $ [anchor, mkBigrams, applyRule slNoun, applyRule rmVerb, applyRule rmAdv] <*> [lits]
--formulae = concat $ [applyRule slNoun, applyRule rmVerb, anchor] <*> [lits]

main :: IO ()
main = do 
   solver <- foldM (flip assertTrue) newSatSolver formulae
   --print solver
   solution <- solve solver
   print solution
   let truetags = filter (\(_,(Var int)) ->lookupVar int solution == Just True) lits
   putStrLn "\nTag sequence:"
   mapM_ putStrLn $ map showTag truetags


showTag :: (Show t, Num t) => ((t, Tag), Boolean) -> String
showTag ((t,tag),_) = show t ++ ": " ++ show tag

fliplookup :: (Eq a, Eq b) => b -> [(a,b)] -> a
fliplookup _ [] = error "not in HashMap<Node<? super E>> D:"
fliplookup x ((y,x'):yxs) = if x==x' then y else fliplookup x yxs

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