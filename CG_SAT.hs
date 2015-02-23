{-# LANGUAGE ScopedTypeVariables #-}
---- ^ that's for the IO error

module CG_SAT where 


import CG
import CG_data
import CG_parse
import Data.Boolean.SatSolver
import Data.List
import Data.Maybe
import Data.Set (Set, fromList, isSubsetOf)
import Control.Monad
import Control.Exception
import System.Environment
import Debug.Trace


type Literal = ((Integer, [Tag]), Boolean)

getInd :: Literal -> Integer
getInd ((i,_),_) = i

getTags :: Literal -> [Tag]
getTags ((_,t),_) = t

getBool :: Literal -> Boolean
getBool (_,b) = b

--we want to often compare the indices
sameInd :: Literal -> Literal -> Bool
sameInd lit lit' = getInd lit == getInd lit'

atLeast :: Int -> [a] -> Bool
atLeast 0 _      = True
atLeast _ []     = False
atLeast n (_:xs) = atLeast (n-1) xs


--Rule     has [[Tag]].
--Analysis has [Tag].
--At least one complete sublist in the rule must be found in the analysis.
tagsMatchRule :: [[Tag]] -> Literal -> Bool
tagsMatchRule tags lit = or $ map (\tagset -> isSubsetOf tagset tagsInAna) tagsInRule
  where tagsInAna  = fromList $ getTags lit :: Set Tag
        tagsInRule = map fromList tags :: [Set Tag]

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
mkLits :: Sentence -> [Literal]
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
anchor lits = [getBool lit | lit <- lits, isUniq (filter (sameInd lit) lits)]
  where isUniq [x] = True
        isUniq _   = False

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

-- 4) Apply rules to literals. 
applyRule :: Rule -> [Literal] -> [Boolean]
applyRule rule lits = --trace (show rule) $
  case rule of
-- a) condition(s) must not hold 
--for each literal, if getContext is empty, remove/select it
    (Remove tags (NEG conds)) -> [Not (getBool lit) | lit <- lits
                                                    , tagsMatchRule tags lit
                                                    , null (ctxt lit conds)]

    (Select tags (NEG conds)) -> [getBool lit | lit <- lits
                                              , tagsMatchRule tags lit
                                              , null (ctxt lit conds)] 
                              ++ [Not (getBool lit) | lit <- lits
                                                    , tagsMatchRule tags lit
                                                    , (not.null) (ctxt lit conds)]

-- b) general case, there can be nested ANDs, ORs or just plain rules
    (Remove tags (POS conds)) -> applyRules rule (toLists conds) lits
    (Select tags (POS conds)) -> applyRules rule (toLists conds) lits

  where ctxt lit conds = concatMap (getContext lit lits) (toLists conds)


applyRules :: Rule -> [[Condition]] -> [Literal] -> [Boolean]
applyRules rule []         allLits = []
applyRules rule (conds:cs) allLits = trace (show rule ++ "\n" ++ (show (map (\lit -> getContext lit allLits conds) allLits))) $ applyRules rule cs allLits ++
  case rule of 
    (Remove tags c) -> mkVars (chosen tags) Not 
    (Select tags c) -> mkVars (chosen tags) id ++ mkVars (other tags) Not


  where
        allCondsHold :: [[Literal]] -> Bool
        allCondsHold context | null context = True
                             | otherwise    =  length (filter (not.null) context) >= 1 --length context == length (filter (not.null) context)


     -- cause => consequence    translates into     Not cause || consequence.
     -- cause is e.g. "-1 is noun" and consequence is "remove verb"
     -- needed because the word at -1 could have many tags, and they could conflict.
        mkVars :: [(Literal,[Literal])] -> (Boolean -> Boolean) -> [Boolean]
        mkVars litctx neg = [Not (foldr1 (:&&:) causes) :||: neg conseq
                             | (l, ls) <- litctx
                              , let conseq = getBool l
                              , let causes = map getBool ls ]


        -- chosen: analyses that have the wanted readings and context
        chosen :: TagSet -> [(Literal,[Literal])]
        chosen tags = [(lit, concat context) | lit <- allLits
                                             , tagsMatchRule tags lit
                                             , let context = getContext lit allLits conds
                                             , allCondsHold context]
 
        -- other: analyses that don't have the wanted readings,
        -- but some word in the same location does have the wanted reading(s)
        other :: TagSet -> [(Literal,[Literal])]
        other tags = [(lit, concat context) | lit <- allWithReading tags
                                            , not (tagsMatchRule tags lit)
                                            , let context = getContext lit allLits conds]
                                           -- , allCondsHold context]
                                --no need to check allCondsHold; it comes from allWithReadings

 
        -- all words that have the desired reading in one of their analyses. e.g.
        --      allLits = [(1,[N,Pl]), (2,[V,Sg]), (2,[N,Pl])]
        --      tags    = [V]
        -- ====> `chosen tags' will return (2,[V,Sg)
        -- ====> (2,[V,Sg]) and (2,[N,Pl]) are returned
        allWithReading tags = intersectBy sameInd allLits wantedLits
          where wantedLits = map fst $ chosen tags


getContext :: Literal          -- ^ a single analysis
               -> [Literal]    -- ^ list of all analyses
               -> [Condition]  -- ^ list of conditions grouped by AND
               -> [[Literal]]  -- ^ context for the first arg. If all conditions match for a literal, there should be as many non-empty Literal lists as Conditions.
getContext lit allLits []     = [[]]
getContext lit allLits ((C position (bool,ctags)):cs) = getContext lit allLits cs ++
  case ctags of
    [[]]   -> [[lit]] -- empty tags in condition = remove/select always
    (t:ts) -> case position of
                (Exactly 0) -> [[lit]]
                (AtLeast 0) -> [[lit]] -- position 0* = remove/select always
                (Exactly n) -> [filter (neg . tagsMatchRule ctags) (exactly n lit)]
                (AtLeast n) -> [filter (neg . tagsMatchRule ctags) (atleast n lit)]
                (Barrier n bs)  -> [filter (neg . tagsMatchRule ctags) (barrier n bs lit)]

  where neg = if bool then id else not

        --given word and n, returns list of words that are n places away in original sentence
        exactly :: Integer -> Literal -> [Literal]
        exactly n ((ind,_),_) = [lit | lit@((ind',_),_) <- allLits, ind' == ind+n]

        --same but list of tags that are at least n places away
        atleast n ((ind,_),_) = [lit | lit@((ind',_),_) <- allLits, ind' >= ind+n ]

        -- between m and n places away
        between m n ((ind,_),_) = [lit | lit@((ind',_),_) <- allLits
                                       , ind+m <= ind' && ind' <= ind+n ]

        barrier n btags lit | barinds==[] = atleast n lit
                            | n < 0     = between mindist n lit
                            | otherwise = between n mindist lit
           where barinds = [getInd lit | lit <- allLits
                                       , tagsMatchRule btags lit]
                 dists   = map (\i -> i - getInd lit) barinds :: [Integer]
                 mindist = minimum dists
                 


basicRules :: [[Literal] -> [Boolean]]
basicRules = [ anchor , mkBigrams] --, exclude ]


---- Main stuff

disambiguate :: [Rule] -> [Analysis] -> IO ()
disambiguate rules analyses = do
  let lits = mkLits analyses
      basic = concatMap ($ lits) basicRules
  putStrLn "\nliterals:"
  mapM_ print lits
  putStrLn "\nformulae:"
  mapM_ print basic
  solver <- foldM (flip assertTrue) newSatSolver basic

  solver2 <- removeConflicting lits rules solver 
  putStrLn "---------\n"

  solution <- solve solver2
  print solution
  let truetags = filter (\(_,(Var int)) -> lookupVar int solution == Just True) lits
  putStrLn "\nTag sequence:"
  mapM_ putStrLn $ map prTag truetags

  putStrLn "-----------\n"

  where prTag :: (Show t, Num t) => ((t, [Tag]), Boolean) -> String
        prTag ((t,tags),_) = show t ++ ": " ++ showTags tags
        showTags (l:as) = show l ++ ' ':(unwords $ map show as)



removeConflicting :: [Literal] -> [Rule] -> SatSolver -> IO SatSolver
removeConflicting lits []       solver = return solver
removeConflicting lits (rl:rls) solver = do
  let formulae = applyRule rl lits
  x <- try $ foldM (flip assertTrue) solver formulae
  case x of 
    Left (error :: IOError) -> do
        putStrLn ("Rule " ++ show rl ++ " conflicts, not added!")
        removeConflicting lits rls solver
    Right solver' -> do
        mapM_ print formulae
        removeConflicting lits rls solver'



main' :: IO ()
main' = do
  args <- getArgs
  
  let (rFile, dFile) = case args of
                             [f1, f2] -> (f1, f2)
                             _        -> ("../../data/hun_cg2.rlx", "../../data/morph-output.txt")
  data' <- readFile dFile >>= getData 
  rules <- readFile rFile >>= getRules
  mapM_ (disambiguate rules) data'

main'' :: String -> String -> IO ()
main'' rFile dFile = do 
  rules <- readFile rFile >>= getRules
  data' <- readFile dFile >>= getData 
  mapM_ (disambiguate rules) data'

test :: IO ()
test = mapM_ (disambiguate rls) CG_data.exs

  where rls = [rmVerbIfDet
             , rmNounIfPron
             , slNounAfterConj
             , slCCifCC             
             , slPrepIfDet 
             , rmAdvIfDet 
             , rmPlIfSg
             , rmSgIfPl
             , negTest
             , negOrTest 
             , slNounIfBear 
             --, slVerbAlways --conflicts with anything that selects other than V 
             , rmParticle ]
