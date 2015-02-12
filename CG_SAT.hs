--module CG_SAT where 
{-# LANGUAGE ScopedTypeVariables #-}
import CG
import CG_data
import Data.Boolean.SatSolver
import Data.List
import Data.Maybe
import Control.Monad
import Control.Exception
--import System.Environment
import Debug.Trace


type Literal = ((Integer, [Tag]), Boolean)

--we want to often compare the indices
sameInd :: Literal -> Literal -> Bool
sameInd lit lit' = getInd lit == getInd lit'

getInd :: Literal -> Integer
getInd ((i,_),_) = i

getTags :: Literal -> [Tag]
getTags ((_,t),_) = t

getBool :: Literal -> Boolean
getBool (_,b) = b

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
anchor lits = [getBool lit | lit <- lits, isUniq lit]
  where isUniq lit = length (filter (sameInd lit) lits) == 1

-- 2) Take all possible bigrams
mkBigrams :: [Literal] -> [Boolean]
mkBigrams lits = zipWith mkCombs pres posts --[lits] [lits]
   where len = length lits
         pres = take (len-1) $ groupBy sameInd lits
         posts = tail $ groupBy sameInd lits
         

mkCombs :: [Literal] -> [Literal] -> Boolean
mkCombs pre post = foldr1 (:||:) combinations
   where combinations = [bool1 :&&: bool2 | (_, bool1) <- pre, 
                                             (_, bool2) <- post,
                                             bool1 /= bool2]

-- 3) Exclude hypotheses for same index so that not all can be true at the same time
-- (probably disregard, doesn't fit in with the original philosophy of CG)
exclude :: [Literal] -> [Boolean]
exclude lits = map Not combinations
  where boolsByIndex = (map . map) getBool (groupBy sameInd lits) :: [[Boolean]]
        ambiguous = filter (atLeast 1) boolsByIndex               :: [[Boolean]]
        combinations = map (foldr1 (:&&:)) ambiguous              :: [Boolean]
        
        atLeast 0 _      = True
        atLeast _ []     = False
        atLeast n (_:ys) = atLeast (n-1) ys

-- 4) Apply rules to literals. 
applyRule :: Rule -> [Literal] -> [Boolean]
applyRule rule lits = trace (show rule) $
  case rule of
-- a) condition(s) must not hold 
-- TODO nesting of NOTs with AND and OR -- is it even relevant? do linguists write horribly nested and confusing stuff?
--for each literal, if getContext is empty, remove/select it
    (Remove tags c@(NEG c1)) -> [Not (getBool lit) | lit <- lits
                                                   , getTags lit `multiElem` tags
                                                   , ctxt lit c1==[]]
    (Select tags c@(NEG c1)) -> [getBool lit | lit <- lits
                                             , getTags lit `multiElem` tags
                                             , ctxt lit c1==[]] 
                             ++ [Not (getBool lit) | lit <- lits
                                                   , getTags lit `multiElem` tags
                                                   ,  ctxt lit c1/=[]]

    (Remove tags (NEG c)) -> map Not $ applyRules rule (toLists c) lits
    (Select tags (NEG c)) -> map Not $ applyRules rule (toLists c) lits

-- b) general case, there can be nested ANDs, ORs or just plain rules
    (Remove tags (POS c)) -> applyRules rule (toLists c) lits
    (Select tags (POS c)) -> applyRules rule (toLists c) lits

  where ctxt lit c = getContext' lit lits $ head (toLists c) -------- bad

--pseudocode
        -- slct :: Literal -> Boolean
        -- slct lit = (context => lit),      context = getContext lit foo bar, there are ctxttags
--                 ++ (context => Not lit) and there are no context tags anywhere

applyRules :: Rule -> [[Condition]] -> [Literal] -> [Boolean]
applyRules rule []         lits = []
applyRules rule x@(xs:xxs) lits = trace (show x) $ applyRules rule xxs lits ++
  case rule of 
    (Remove tags c) -> mkVars (chosen tags) Not 
    (Select tags c) -> mkVars (chosen tags) id ++ mkVars (other tags) Not


  where 
     -- reason is e.g. "-1 is noun" and consequence is "remove verb"
     -- needed because the word at -1 could have many tags, and they could conflict.

     -- reason => consequence    translates into  Not reason || consequence.
     -- for example,
     -- -1Pron && +1Verb => Not Noun  =========>  Not (-1Pron && +1Verb) || Not Noun
     -- SAT solver will transform it into conjunctive normal form
        mkVars tags neg = [Not (foldr1 (:&&:) reason) :||: (neg . getBool) conseq
                            | conseq <- tags, 
                              let reason = fromMaybe [Yes] (lookup conseq contextReasons) ]

        contextReasons = getContext lits (zip lits (repeat [Yes])) xs :: [(Literal,[Boolean])]
        (context,_) = unzip contextReasons

        -- chosen is simple: just get tags' that are in tags
        chosen tags = filter (\lit -> getTags lit `multiElem` tags) context :: [Literal]
        

        -- other must filter tags' that are not in tags, but not from all lits in contextN:
        -- just from the words that have somewhere an analysis which is in tags
        other tags  = filter (\lit -> getTags lit `multiNotElem` tags) (allWithReading tags)

        -- all words that have the desired reading in one of their analyses. e.g.
        --      lits = [(1,[N,Pl]), (2,[V,Sg]), (2,[N,Pl])]
        --      tags = [V]
        -- ====> chosen tags will return (2,[V,Sg)
        -- ====> (2,[V,Sg]) and (2,[N,Pl]) are returned
        allWithReading tags = intersectBy sameInd lits (chosen tags)


--for singleton lists, goes just one time and chooses all lits that apply
--for lists with more members, chooses lits where all conditions apply
                           --just for one literal, then can be reused for NOT case
getContext :: [Literal] -> [(Literal,[Boolean])] -> [Condition] -> [(Literal,[Boolean])]
getContext original chosen ((C _ (_,[])):cs)        = chosen
getContext original chosen []                       = chosen 
getContext original chosen (c@(C p (yOrN,contextTags)):cs) = getContext original newChosen cs
  
  where 
        newChosen =  -- should include reasons for the previous as well 
          case p of
            (Exactly 0) -> chosen
            (AtLeast 0) -> chosen -- at least 0 means remove/select everywhere 
            (Exactly n) -> choose (exactly n)
            (AtLeast n) -> choose (atleast n)

            (Barrier n bs)  -> choose (barrier n bs) --Negative ind?

        choose ctxFun = case yOrN of
                          True -> [(lit, map getBool $ filter hasContextTag $ ctxFun lit)
                                   | lit <- fst $ unzip chosen
                                   , any hasContextTag $ ctxFun lit]
                          False ->  [(lit, map getBool $ filter noContextTag $ ctxFun lit)
                                   | lit <- fst $ unzip chosen]

        hasContextTag lit = getTags lit `multiElem` contextTags
        noContextTag lit  = getTags lit `multiNotElem` contextTags


        --given word and n, returns list of words that are n places away in original sentence
        exactly :: Integer -> Literal -> [Literal]
        exactly n ((ind,_),_) = [lit | lit@((ind',_),_) <- original, ind' == ind+n]

        --same but list of tags that are at least n places away
        atleast n ((ind,_),_) = [lit | lit@((ind',_),_) <- original, ind' >= ind+n ]

        -- between m and n places away
        between m n ((ind,_),_) = [lit | lit@((ind',_),_) <- original
                                       , ind+m <= ind' && ind' <= ind+n ]

        barrier n btags lit | dists==[] = [] 
                            | otherwise = between n mindist lit
           where
                 barinds = [ind | ((ind,tags),_) <- original, tags `multiElem` btags]
                 dists   = map (\i -> i - getInd lit) barinds :: [Integer]
                 mindist = minimum dists
                 
getContext' :: Literal -> [Literal] -> [Condition] -> [Boolean]
getContext' lit allLits ((C _ (_,[])):cs)            = [getBool lit]
getContext' lit allLits []                       = trace ("getContext': empty cond") $ []
getContext' lit allLits (c@(C p (yOrN,contextTags)):cs) = trace ("getContext': " ++ show lit) $ getContext' lit allLits cs ++
  
         -- should include reasons for the previous as well 
          case p of
            (Exactly 0) -> [getBool lit]
            (AtLeast 0) -> [getBool lit] -- at least 0 means remove/select everywhere 
            (Exactly n) -> choose (exactly n) lit
            (AtLeast n) -> choose (atleast n) lit

            (Barrier n bs)  -> choose (barrier n bs) lit
  where 

        -- choose ctxFun lit = map getBool $ filter hasContextTag $ ctxFun lit


        choose ctxFun lit = case yOrN of
                          True -> map getBool $ filter hasContextTag $ ctxFun lit
                          False -> map getBool $ filter noContextTag $ ctxFun lit

        hasContextTag lit = getTags lit `multiElem` contextTags
        noContextTag lit = getTags lit `multiNotElem` contextTags


        --given word and n, returns list of words that are n places away in original sentence
        exactly :: Integer -> Literal -> [Literal]
        exactly n ((ind,_),_) = [lit | lit@((ind',_),_) <- allLits, ind' == ind+n]

        --same but list of tags that are at least n places away
        atleast n ((ind,_),_) = [lit | lit@((ind',_),_) <- allLits, ind' >= ind+n ]

        -- between m and n places away
        between m n ((ind,_),_) = [lit | lit@((ind',_),_) <- allLits
                                       , ind+m <= ind' && ind' <= ind+n ]
        barrier n btags lit | dists==[] = [] 
                            | otherwise = between n mindist lit
           where barinds = [ind | ((ind,tags),_) <- allLits, tags `multiElem` btags]
                 dists   = map (\i -> i - getInd lit) barinds :: [Integer]
                 mindist = minimum dists
                 
           

--True if any of the items in AS is in BS
multiElem :: (Eq a) => [a] -> [a] -> Bool
multiElem as bs = any (\a -> a `elem` bs) as

-- True if none if the items in AS is in BS
multiNotElem :: (Eq a) => [a] -> [a] -> Bool
multiNotElem as bs = all (\a -> a `notElem` bs) as

showTag :: (Show t, Num t) => ((t, [Tag]), Boolean) -> String
showTag ((t,tags),_) = show t ++ ": " ++ show tags

basicRules :: [[Literal] -> [Boolean]]
basicRules = [ anchor , mkBigrams] --, exclude ]

moreRules  = [ rmParticle 
             , rmVerbIfDet
             , rmNounIfPron
             , slNounAfterConj
             , slCCifCC             
             , slPrepIfDet 
             , rmAdvIfDet 
             , rmPlIfSg
             , rmSgIfPl
             , negTest
             , negOrTest 
             , slVerbAlways --conflicts with anything that selects other than V 
             -- , slNounIfBear 
             , rmParticle ]


rules = basicRules ++ map applyRule moreRules


---- Main stuff

disambiguate :: [Analysis] -> IO ()
disambiguate analyses = do
   let lits = mkLits analyses
       basic = concatMap ($ lits) basicRules
   putStrLn "\nliterals:"
   mapM_ print lits
   putStrLn "\nformulae:"
   mapM_ print basic
   solver <- foldM (flip assertTrue) newSatSolver basic

   solver2 <- goodRules lits moreRules solver 
   putStrLn "---------\n"

   solution <- solve solver2
   print solution
   let truetags = filter (\(_,(Var int)) -> lookupVar int solution == Just True) lits
   putStrLn "\nTag sequence:"
   mapM_ putStrLn $ map showTag truetags

   putStrLn "-----------\n"

goodRules :: [Literal] -> [Rule] -> SatSolver -> IO SatSolver
goodRules lits []       solver = return solver
goodRules lits (rl:rls) solver = do
  let formulae = applyRule rl lits
  x <- try $ foldM (flip assertTrue) solver formulae
  case x of 
    Left (error :: IOError) -> do
        putStrLn ("Rule " ++ show rl ++ " conflicts, not added!")
        goodRules lits rls solver
    Right solver' -> do
        mapM_ print formulae
        goodRules lits rls solver'

  
   

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

