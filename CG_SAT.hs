--module CG_SAT where 

import CG
import CG_data
import Data.Boolean.SatSolver
import Data.List
import Control.Applicative
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
-- possible application: grammar writer wants to try two combinations of tag sets, would explode in the normal implementation, but sat solver would not be overkill

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


-- 3) Apply rules to literals. Assuming everything is OR!
applyRule :: Rule -> [Literal] -> [Boolean]

-- a) remove/select <tags> everywhere
applyRule (Or rs tags []) lits = 
  case rs of
    Remove -> [Not bool | (_,bool) <- chosenTags]
    Select -> [bool | (_,bool) <- chosenTags] ++ [Not bool | (_,bool) <- otherTags]
  where chosenTags = filter (\((int, tags'), bool) -> tags' `multiElem` tags) lits
        otherTags  = filter (\((int, tags'), bool) -> tags' `multiNotElem` tags) lits

-- b) remove analyses containing <tags> if the word itself contains any of <tags'>
applyRule rule lits = applyRule' rule lits

-- Helper function for recursive case
applyRule' rule@(Or _ _ []) lits = trace (show rule ++ "\n") $ []
applyRule' rule@(Or rs chosenTags  (c@(C p contextTags):cs)) lits = 
  trace (show rule ++ "\ncontextN: " ++  show contextN ++
         "\nchosenTags: " ++ show chosenTags ++
         "\ncontextTags: " ++ show contextTags ++
         "\nisMultiNotElem: " ++ show isMultiNotElem ++
         "\nisMultiElem: " ++ show isMultiElem) $
  applyRule' (Or rs chosenTags cs) lits ++
  case rs of 
    Remove -> [Not bool | ((_,tags),bool) <- contextN, tags `multiElem` chosenTags]

    Select -> [bool | ((_,tags),bool) <- contextN, tags `multiElem` chosenTags] 
              ++  [Not bool | ((_,tags),bool) <- contextN, tags `multiNotElem` chosenTags]

  where 
        -- has a context tag at exactly n places away.
        -- if context is 0, word itself must have context tag.
        contextN = if p==0 then filter hasContextTag lits
                           else filter (hasContextTags . exactlyN) lits

        --for each tag, get a list of tags that are exactly n places away
        exactlyN :: ((Integer, [Tag]), Boolean) -> [((Integer, [Tag]), Boolean)]
        exactlyN ((int,_),_) = filter (\((int',_),_) -> int+p == int') lits

        --same but list of tags that are at least n places away
        atleastN ((int,_),_) = filter (\((int',_),_) -> int+p >= int') lits


        hasContextTag ((int,tags),bool) = tags `multiElem` contextTags

        hasContextTags [] = False
        hasContextTags (x:xs) = if hasContextTag x then True else hasContextTags xs

        --for debugging
        isMultiNotElem = [((foo,tags),bool) | ((foo,tags),bool) <- contextN, tags `multiNotElem` chosenTags]
        isMultiElem = [((foo,tags),bool) | ((foo,tags),bool) <- contextN, tags `multiElem` chosenTags] 


applyRule' r@(And rs chosenTags cs) lits = 
  case rs of 
    Remove -> [Not bool | ((_,tags),bool) <- contextN, tags `multiElem` chosenTags]

    Select -> [bool | ((_,tags),bool) <- contextN, tags `multiElem` chosenTags] 
              ++  [Not bool | ((_,tags),bool) <- contextN, tags `multiNotElem` chosenTags]
  where contextN = getContext lits r cs

getContext :: [((Integer, [Tag]), Boolean)] -> Rule -> [Condition] -> [((Integer, [Tag]), Boolean)]
getContext chosen _ []                       = chosen
getContext chosen r (c@(C p contextTags):cs) = getContext newChosen r cs
  

  where 
        newChosen = if p==0 then filter hasContextTag chosen
                           else filter (hasContextTags . exactlyN) chosen


        hasContextTag ((int,tags),bool) = tags `multiElem` contextTags
        hasContextTags [] = False
        hasContextTags (x:xs) = if hasContextTag x then True else hasContextTags xs
        
        exactlyN :: ((Integer, [Tag]), Boolean) -> [((Integer, [Tag]), Boolean)]
        exactlyN ((int,_),_) = filter (\((int',_),_) -> int+p == int') chosen
--first 
        

--True if any of the items in AS is in BS
multiElem :: (Eq a) => [a] -> [a] -> Bool
multiElem as bs = or $ map (\a -> a `elem` bs) as --elem <$> as <*> [bs]

-- True if none if the items in AS is in BS
multiNotElem :: (Eq a) => [a] -> [a] -> Bool
multiNotElem as bs = and $ map (\a -> a `notElem` bs) as

showTag :: (Show t, Num t) => ((t, [Tag]), Boolean) -> String
showTag ((t,tags),_) = show t ++ ": " ++ show tags

basicRules = [ anchor , mkBigrams ]

moreRules  = [ applyRule slNounIfBear
       --      , applyRule rmVerbIfBear
       --      , applyRule rmVerbIfDet
       --      , applyRule slPrepIfDet 
       --      , applyRule rmAdvIfDet 
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