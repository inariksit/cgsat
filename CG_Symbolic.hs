module CG_Symbolic where 

import CG_base hiding ( Sentence, showSentence )
import SAT ( Solver(..), newSolver, deleteSolver )
import SAT.Named

import Control.Monad
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace
import Prelude hiding ( Word )
import Text.Regex


--------------------------------------------------------------------------------
--Indices start at 1. More intuitive to talk about w1, w2 vs. w0, w1.
--(Ab)using Data.IntMap because lookup and updates go nicely with builtin functions
type Word     = IM.IntMap Lit
type Sentence = IM.IntMap Word
type TagMap   = M.Map Tag WIndSet
type WIndex   = Int
type SIndex   = Int
type WIndSet  = IS.IntSet
type TrgIS    = IS.IntSet
type DifIS    = IS.IntSet



type TrgInds = (TrgIS,DifIS) --,WhoCares = all \ trg. 
                         -- Would be needed for cautious and onlyTrgLeft.
                         -- Question: is it better to store it or compute it for each rule?
type CondInds = (TrgIS,DifIS)

data Condition' = C' Position (Bool,[CondInds]) --like CG_base's Condition, but s/TagSet/CondInds/
data Rule' = R { trg :: TrgInds --Rule allows disjunction in targets, but Rule' does not. 
                                --We translate one such Rule into [Rule'].
               , cnd :: [[Condition']]
               , isSelect' :: Bool
               }

--We call this once for every rule, lookupTag does some expensive intersections and stuff
--Also many rules share targets and conditions--can we save the results?
--This translates one Rule into [Rule']: if there are disjoint targets, this is just way easier to do. REMOVE foo OR bar means actually remove anything if it is foo or bar. So disjunction=union.
--For disjoint conditions, mkCond does already the right thing (or that was the intention).
ruleToRules' :: TagMap -> IS.IntSet -> Rule -> [Rule']
ruleToRules' tagmap allinds rule = 
 [ R ti_di conds isSel | trg_dif <- toTags $ target rule
                       , let ti_di = luTag trg_dif ] 

 where
  luTag = lookupTag tagmap allinds
  isSel = isSelect rule
  conds = [  [ C' index (positive, map luTag (toTags ctags))
             |  C index (positive, ctags) <- conditions ]
           | conditions <- toConds (cond rule) ]
          

--------------------------------------------------------------------------------

mkSentence :: Solver -> Int -> [[Tag]] -> IO Sentence
mkSentence s w tcs = IM.fromList `fmap` sequence 
                       [ ((,) n . IM.fromList) `fmap` sequence 
                         [ (,) m `fmap` newLit s (shTC t n) | (m, t) <- zip [1..] tcs ]
                            | n <- [1..w] ] 
 where shTC ts i = "w" ++ show i ++ (concatMap (\t -> '<':show t++">") ts)

mkTagMap :: [Tag] -> [[Tag]] -> TagMap
mkTagMap ts tcs = M.fromList $
                   ts `for` \t -> let getInds = IS.fromList . map (1+) . findIndices (elem t)
                                  in (t, getInds tcs) 

lookupTag :: TagMap -> WIndSet -> (Trg,Dif) -> (WIndSet,WIndSet)
lookupTag alltags allinds (trg,dif) = 
  let trgInds = if trg==[[]] then allinds --this means IF (-1 (*))
                   else IS.unions $ map (go allinds) trg --trg::[[Tag]]
      difInds = if dif==[[]] then IS.empty --this means IF (-1 something \\ nothing)
                      else IS.unions $ map (go allinds) dif --dif::[[Tag]]
          
  in  (trgInds IS.\\ difInds, difInds)
 where
  go acc []     = acc         --default is empty set, because intersect [] _ == []
  go acc (t:ts) = let inds = case t of
                              (Rgx r s) -> IS.empty `fromMaybe` lookupRegex t alltags
                              _         -> IS.empty `fromMaybe` M.lookup t alltags
                  in go (IS.intersection acc inds) ts
                                
lookupRegex :: Tag -> TagMap -> Maybe WIndSet
lookupRegex (Rgx r s) tagmap = trace ("lookupRegex: " ++ s) $
  Just $ IS.unions $ M.elems $ M.filterWithKey (\t _ -> matchTag r t) tagmap
 where
  matchTag regex (WF str)  = isJust $ matchRegex regex str
  matchTag regex (Lem str) = isJust $ matchRegex regex str
  matchTag regex _         = False
--lookupRegex tag = lookup tag tagmap shouldn't happen anyway


lookupLit :: Sentence -> SIndex -> WIndex -> Lit
lookupLit sentence si wi = 
  case IM.lookup si sentence of
    Just ts -> case IM.lookup wi ts of
                 Just tag -> tag
                 Nothing  -> error "lookupLit: tag combination not found"
    Nothing -> true --If index is out of bounds, we are sent here by negated rule.
                    --If there is no -100, then `NOT -100 foo' is true.

--------------------------------------------------------------------------------

width :: [[Condition]] -> (Int,SIndex)
width []   = (1,1)
width [[]] = (1,1)
width cs   = (length [minInd..maxInd], 
             1+(fromJust $ elemIndex 0 [minInd..maxInd]))
 where
  minInd = 0 `min` minimum poss
  maxInd = 0 `max` maximum poss
  poss = [ i | c <- concat cs 
              , let i = case c of
                         C pos _ -> posToInt pos
                         Always  -> 0  
                         _       -> error $ "expected condition, got " ++ show cs]

--for (C)BARRIER, count an extra place to place the barrier tag
posToInt :: Position -> Int
posToInt (Exactly _ i) = i
posToInt (AtLeast _ i) = i
posToInt (Barrier _ i _)  = if i<0 then i-1 else i+1
posToInt (CBarrier _ i _) = if i<0 then i-1 else i+1

isCareful :: Position -> Bool
isCareful (Exactly b _) = b
isCareful (AtLeast b _) = b
isCareful (Barrier b _ _) = b
isCareful (CBarrier b _ _) = b

getPos :: Condition -> SIndex
getPos (C (Barrier  _ i _) _) = i
getPos (C (CBarrier _ i _) _) = i
getPos (C  position        _) = posToInt position
getPos _ = error "trying to apply to complex condition"

for :: (Functor f) => f a -> (a -> b) -> f b
for = flip fmap

singleton :: [a] -> Bool
singleton [x] = True
singleton _   = False

--------------------------------------------------------------------------------

parse :: String -> [Tag]
parse str = map toTag $ filter (not.null) $ split isValid str
 where 
  isValid c = c=='<' || c=='+'
  toTag ">>>" = BOS
  toTag "<<<" = EOS
  toTag []    = error "empty tag"
  toTag str = if last str=='>' then Tag (init str) else Lem str

split :: (a -> Bool) -> [a] -> [[a]]
split p [] = []
split p xs = takeWhile (not . p) xs : split p (drop 1 (dropWhile (not . p) xs))