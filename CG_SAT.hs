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
anchor lits = [bool | ((ind,tag),bool) <- lits, 
                      isUniq ind indices]
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

-- a) empty condition, remove/select <tags> everywhere
    (Remove tags c@(C _ [])) -> trace ("applyRule: " ++ show c) $ [Not bool | (_,bool) <- chosen tags]
    (Select tags c@(C _ [])) -> trace ("applyRule: " ++ show c ++ "\nother  : " ++ show (other tags) ++ "\nchosen : " ++ (show $ chosen tags)) $ 
                                [bool | (_,bool) <- chosen tags] ++ 
                                [Not bool | (_,bool) <- other tags]

-- b) condition(s) must not hold 
-- TODO nesting of NOTs with AND and OR -- is it even relevant? do linguists write horribly nested and confusing stuff?
    (Remove tags c@(NOT c1)) -> map Not $ applyRules rule (toLists c) lits
    (Select tags c@(NOT c1)) -> map Not $ applyRules rule (toLists c) lits

-- c) general case, there can be nested ANDs, ORs or just plain rules
    (Remove tags c) -> applyRules rule (toLists c) lits
    (Select tags c) -> applyRules rule (toLists c) lits

  where -- chosen is simple: just get tags' that are in tags
        chosen tags = filter (\((ind,tags'),bool) -> tags' `multiElem` tags) lits

        -- other must filter tags' that are not in tags, but not from all lits:
        -- just from the words that have somewhere an analysis which is in tags
        other tags  = filter (\((ind,tags'),bool) -> tags' `multiNotElem` tags) (allWithReading tags)

        -- list of positions in the sentence, where one of the analyses is in tags. e.g.
        --      lits = (1,[N,Pl]), (2,[V,Sg]), (2,[N,Pl], (3,[Pron]), tags = [N]
        -- ====> [1,2] is returned.
        chosenInds tags = [ind | ((ind,_),_) <- chosen tags]

        -- all lemmas that have the desired reading in one of their analyses. e.g.
        --      lits = (1,[N,Pl]), (2,[V,Sg]), (2,[N,Pl]), tags = [V]
        -- ====> (2,[V,Sg]) and (2,[N,Pl]) are returned
        allWithReading tags = filter (\((ind,_),_) -> ind `elem` chosenInds tags) lits

        negate (Not b) = b
        negate b       = Not b

applyRules :: Rule -> [[Condition]] -> [Literal] -> [Boolean]
applyRules rule [] lits       = []
applyRules rule x@(xs:xxs) lits = trace ("\napplyRules: " ++ show x) $ applyRules rule xxs lits ++
  case rule of 
    (Remove tags c) -> [Not bool | ((_,tags'),bool) <- chosen tags]

    (Select tags c) -> [bool | ((_,tags'),bool) <- chosen tags] ++
                       [Not bool | ((_,tags'),bool) <- allWithReading tags, tags' `multiNotElem` tags]
  where contextN = getContext lits lits xs 

        --main difference here is that tags are chosen from contextN, not lits
        chosen tags = filter (\((ind,tags'),bool) -> tags' `multiElem` tags) contextN
        other tags  = filter (\((ind,tags'),bool) -> tags' `multiNotElem` tags) (allWithReading tags)

        chosenInds tags = [ind | ((ind,_),_) <- chosen tags]

        allWithReading tags = filter (\((ind,_),_) -> ind `elem` chosenInds tags) contextN


--for singleton lists, goes just one time and chooses all lits that apply
--for lists with more members, chooses lits where all conditions apply
getContext :: [Literal] -> [Literal] -> [Condition] -> [Literal]
getContext lits chosen []                       = trace ("getContext: " ++ show chosen) $ chosen
getContext lits chosen (c@(C p contextTags):cs) = trace ("getContext: " ++ show chosen) $ getContext lits newChosen cs
  
  where 
        newChosen = case p of
                      (Exactly 0) -> filter hasContextTag chosen
                      (AtLeast 0) -> lits -- at least 0 means remove/select everywhere 
                      (Exactly n) -> filter (hasContextTags . exactlyN n) chosen
                      (AtLeast n) -> filter (hasContextTags . atleastN n) chosen

        --for each tag, get a list of tags that are exactly n places away
        exactlyN :: Integer -> Literal -> [Literal]
        exactlyN n ((ind,_),_) = filter (\((ind',_),_) -> ind+n == ind') lits --chosen

        --same but list of tags that are at least n places away
        atleastN n ((ind,_),_) = filter (\((ind',_),_) -> ind+n >= ind') lits --chosen


        hasContextTag ((ind,tags),bool) = tags `multiElem` contextTags
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

moreRules  = [ rmParticle
             -- , slNounIfBear
             , slVerbAlways 
             , rmVerbIfDet ]
             -- , rmNounIfPron
             -- , slPrepIfDet 
             -- , rmAdvIfDet 
             -- , notTest
             -- , notOrTest 
             -- , andTest ]


rules = basicRules ++ map applyRule moreRules


---- Main stuff

disambiguate :: [Analysis] -> IO ()
disambiguate analyses = do
   let lits = mkLits analyses
       formulae = nub $ concatMap (\rule -> rule lits) rules --doesn't always add all rules -- why?
--       formulae = concatMap (\rule -> rule lits) rules --gives (user error: mzero) if rules conflict
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

