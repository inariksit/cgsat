--module CG_SAT where 

import CG
import CG_data
import Data.Boolean.SatSolver
import Data.List
import Control.Monad
import System.Environment
import Debug.Trace


type Literal = ((Integer, [Tag]), Boolean)

instance Eq Boolean where
  Var n      == Var m        = m==n
  Var n      == _            = False
  _          == Var n        = False
  (n :||: m) == (n' :||: m') = n==n' && m==m'
  (n :||: m) == _            = False
  (n :&&: m) == (n' :&&: m') = n==n' && m==m'
  (n :&&: m) == _            = False
  Not bool   == Not bool'    = bool == bool
  Not bool   == _            = False
  Yes        == Yes          = True
  Yes        == _            = False
  No         == No           = True
  No         == _            = False



-- SAT stuff

-- | Input: sentence, as in list of analyses.
-- | Output: for each word in sentence, 
-- | * its position in the sentence
-- | * one of the suggested tag sequences
-- | * id of that hypothesis in the SAT solver
-- [((1,[Lem "the", Det]),Var 1),
--  ((2,[Lem "bear", N,Sg]),Var 2),
--  ((2,[Lem "bear", V,Pl]),Var 3),
--  ((3,[Lem "sleep", N,Pl]),Var 4),
--  ((3,[Lem "sleep", V,Sg]),Var 5)]
mkLits :: Sentence -> [((Integer,[Tag]),Boolean)]
mkLits = mkLiterals . mkSymbols

mkSymbols :: [[a]] -> [(Integer,a)]
mkSymbols as = concat $ go as 1
    where go []     n = []
          go (x:xs) n = map (\tag -> (n,tag)) x : go xs (n+1)

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
          pres = take (len-1) $ groupBy sameNumber lits
          posts = tail $ groupBy sameNumber lits
          sameNumber ((n1,_),_) ((n2,_),_) = n1==n2 

mkCombs :: [Literal] -> [Literal] -> Boolean
mkCombs pre post = foldr1 (:||:) combinations
    where combinations = [bool1 :&&: bool2 | (_, bool1) <- pre, 
                                             (_, bool2) <- post,
                                             bool1 /= bool2]


-- 3) Apply rules to literals. 
applyRule :: Rule -> [Literal] -> [Boolean]


applyRule rule lits = 
  case rule of

-- empty condition, remove/select <tags> everywhere
    (Remove tags c@(C _ [])) -> [Not bool | (_,bool) <- chosen tags]
    (Select tags c@(C _ [])) -> [bool | (_,bool) <- chosen tags] ++ 
                                [Not bool | (_,bool) <- other tags]

-- conditions must all hold
    (Remove tags c@(AND c1 c2)) -> trace (show rule) $ applyRuleAnd rule (toCList c) lits
    (Select tags c@(AND c1 c2)) -> trace (show rule) $ applyRuleAnd rule (toCList c) lits

-- condition(s) must not hold
    (Remove tags c@(NOT c1)) -> trace (show rule) $ applyRuleNot rule (toCList c) lits
    (Select tags c@(NOT c1)) -> trace (show rule) $ applyRuleNot rule (toCList c) lits

-- either we have a simple condition or OR: apply all rules in sequence
    (Remove tags c) -> trace (show rule) $ applyRuleOr rule (toCList c) lits
    (Select tags c) -> trace (show rule) $ applyRuleOr rule (toCList c) lits
  where chosen tags = filter (\((int, tags'), bool) -> tags' `multiElem` tags) lits
        other tags  = filter (\((int, tags'), bool) -> tags' `multiNotElem` tags) lits


applyRuleNot :: Rule -> [Condition] -> [Literal] -> [Boolean]
applyRuleNot = undefined 

-- Helper function for OR case
applyRuleOr :: Rule -> [Condition] -> [Literal] -> [Boolean]
applyRuleOr rule [] lits = trace (show rule ++ "\n") $ []
applyRuleOr rule (c@(C p contextTags):cs) lits = applyRuleOr rule cs lits ++
  case rule of 
    (Remove chosenTags _) -> [Not bool | ((_,tags),bool) <- contextN, tags `multiElem` chosenTags]

    (Select chosenTags _) -> [bool | ((_,tags),bool) <- contextN, tags `multiElem` chosenTags] ++
                             [Not bool | ((_,tags),bool) <- contextN, tags `multiNotElem` chosenTags]

  where 
        -- has a context tag at exactly n places away.
        -- if context is 0, word itself must have context tag.
        contextN = case p of
                      (Exactly 0) -> filter hasContextTag lits
                      (AtLeast 0) -> lits -- at least 0 means basically remove everywhere 
                      (Exactly n) -> filter (hasContextTags . exactlyN n) lits
                      (AtLeast n) -> filter (hasContextTags . atleastN n) lits

        --for each tag, get a list of tags that are exactly n places away
        exactlyN :: Integer -> Literal -> [Literal]
        exactlyN n ((int,_),_) = filter (\((int',_),_) -> int+n == int') lits

        --same but list of tags that are at least n places away
        atleastN n ((int,_),_) = filter (\((int',_),_) -> int+n >= int') lits


        hasContextTag ((int,tags),bool) = tags `multiElem` contextTags

        hasContextTags [] = False
        hasContextTags (x:xs) = if hasContextTag x then True else hasContextTags xs
 


applyRuleAnd rule cs lits = 
  case rule of 
    (Remove chosenTags c) -> [Not bool | ((_,tags),bool) <- contextN, tags `multiElem` chosenTags]

    (Select chosenTags c) -> [bool | ((_,tags),bool) <- contextN, tags `multiElem` chosenTags] ++
                             [Not bool | ((_,tags),bool) <- contextN, tags `multiNotElem` chosenTags]
  where contextN = getContext lits cs

getContext :: [Literal] -> [Condition] -> [Literal]
getContext chosen []                       = chosen
getContext chosen (c@(C p contextTags):cs) = getContext newChosen cs
  

  where 
        newChosen = case p of
                      (Exactly 0) -> filter hasContextTag chosen
                      (Exactly n) -> filter (hasContextTags . exactlyN n) chosen
                      (AtLeast n) -> filter (hasContextTags . atleastN n) chosen

        --for each tag, get a list of tags that are exactly n places away
        exactlyN :: Integer -> Literal -> [Literal]
        exactlyN n ((int,_),_) = filter (\((int',_),_) -> int+n == int') chosen

        --same but list of tags that are at least n places away
        atleastN n ((int,_),_) = filter (\((int',_),_) -> int+n >= int') chosen


        hasContextTag ((int,tags),bool) = tags `multiElem` contextTags
        hasContextTags [] = False
        hasContextTags (x:xs) = if hasContextTag x then True else hasContextTags xs
        

        

--True if any of the items in AS is in BS
multiElem :: (Eq a) => [a] -> [a] -> Bool
multiElem as bs = or $ map (\a -> a `elem` bs) as

-- True if none if the items in AS is in BS
multiNotElem :: (Eq a) => [a] -> [a] -> Bool
multiNotElem as bs = and $ map (\a -> a `notElem` bs) as

showTag :: (Show t, Num t) => ((t, [Tag]), Boolean) -> String
showTag ((t,tags),_) = show t ++ ": " ++ show tags

basicRules = [ anchor , mkBigrams ]

moreRules  = [ applyRule slNounIfBear
             , applyRule rmVerbIfBear
             , applyRule rmVerbIfDet
             , applyRule slPrepIfDet 
             , applyRule rmAdvIfDet 
             , applyRule andTest ]

rules = basicRules ++ moreRules


---- Main stuff

disambiguate :: [Analysis] -> IO ()
disambiguate analyses = do
   let lits = mkLits analyses
       formulae = nub $ concatMap (\rule -> rule lits) rules
   putStrLn "\nliterals:"
   mapM_ print lits
   putStrLn "\nformulae:"
   mapM_ print formulae
   putStrLn "---------\n"

   solver <- foldM (flip assertTrue) newSatSolver formulae
   solution <- solve solver
   print solution
   let truetags = filter (\(_,(Var int)) -> lookupVar int solution == Just True) lits
   putStrLn "\nTag sequence:"
   mapM_ putStrLn $ map showTag truetags

   putStrLn "-----------\n"

main :: IO ()
main = do 
   --args <- getArgs
   mapM_ disambiguate exs


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