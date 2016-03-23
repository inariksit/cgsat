module CG_SAT (
  apply
, mkTagMap
, ruleToRule'
, mkSentence
)
where

import CG_base hiding ( Cohort, Sentence )
--import CG_base as CGB
import SAT ( Solver(..), newSolver, deleteSolver )
import qualified SAT
import SAT.Named
import AmbiguityClass

import Control.Monad
import Data.Foldable ( asum )
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace
import Text.Regex


--------------------------------------------------------------------------------  

-- vblex |-> [vblex sg p3, vblex pl p1, vblex ]
type TagMap   = M.Map Tag IS.IntSet

type Cohort   = IM.IntMap Lit
type Sentence = IM.IntMap Cohort

--------------------------------------------------------------------------------

type TrgIS    = IS.IntSet
type DifIS    = IS.IntSet
type TrgInds = (TrgIS,DifIS)
type CondInds = (TrgIS,DifIS)

--like CG_base's Condition, but s/TagSet/CondInds/
data Condition' = C' Position (Bool,[CondInds]) | Always' deriving (Show,Eq)

data Rule' = R { trg :: TrgInds 
               , cnd :: [[Condition']]
               , isSelect' :: Bool
               , show' :: String
               } deriving (Eq)

instance Show Rule' where
  show = show'

--We call this once for every rule, lookupTag does some expensive intersections and stuff
--Also many rules share targets and conditions--can we save the results?
ruleToRule' :: TagMap -> IS.IntSet -> Rule -> Rule'
ruleToRule' tagmap allinds rule = R trget conds isSel nm
 where
  nm = show rule
  isSel = isSelect rule

  lu = lookupTag tagmap allinds

  -- TODO: will this work for `Det - Zijn OR Foo - Bar'?
  -- unions will just merge everything together and ignore the difInds.
  -- Note that mkCond handles this properly, with two nested lists.
  -- Will look into it if I see a grammar rule that has disjoint diffs in target.
  (trgInds,difInds) = unzip [ lu (trg,dif) | (trg,dif) <- toTags $ target rule ]
  trget = (IS.unions trgInds, IS.empty)

  conds = --trace ("ruleToRule' cnd: " ++ show (toConds $ cond rule)) $
           (map.map) condToCond' (toConds $ cond rule)

  condToCond' (C index (positive, ctags)) = 
     let yesInds_noInds = --trace ("condToCond': " ++ (show $ (toTags ctags, map lu (toTags ctags)))) $ 
                           map lu (toTags ctags) 
     in  C' index (positive, yesInds_noInds)
  condToCond' Always = Always'

--------------------------------------------------------------------------------

apply :: Solver -> Sentence -> Rule' -> IO Sentence
apply s sentence rule = do

  --all possible readings that the rule targets
  let (allTrgInds,allDifInds) = trg rule


  let applyToWord sentence (i,cohort) = do
       let indsInCohort   = IM.keysSet cohort
       let (trgIndsRaw,otherIndsRaw) = IS.partition (\i -> IS.member i allTrgInds && IS.notMember i allDifInds) indsInCohort
       let (trgInds,otherInds) = if isSelect' rule --if Select, flip target and other
                                  then (otherIndsRaw,trgIndsRaw)
                                  else (trgIndsRaw,otherIndsRaw)

       disjConds <- mkConds s indsInCohort sentence i (cnd rule)
       case disjConds of
         Nothing -> return sentence --conditions out of scope, no changes in sentence
         Just cs -> do
           let trgIndsList = IS.toList trgInds

           condsHold <- orl' s cs
           let trgPos   = mapMaybe   (lu' cohort) trgIndsList
           let otherNeg = map (neg . lu cohort) (IS.toList otherInds)

           someTrgIsTrue <- orl' s trgPos 
           noOtherIsTrue <- andl' s otherNeg
           onlyTrgLeft <- andl' s [ someTrgIsTrue, noOtherIsTrue ]
           cannotApply <- orl' s [ neg condsHold, onlyTrgLeft ]

           newTrgLits <- sequence
             --wN<a>' is true if both of the following:
             [ andl s newTrgName [ oldTrgLit     --wN<a> was also true, and
                                 , cannotApply ] --rule cannot apply 
               | oldTrgLit <- trgPos
               , let newTrgName = show oldTrgLit ++ "'" ]
           let newcoh = foldl updateReading cohort (zip trgIndsList newTrgLits)
           return $ updateCohort sentence i newcoh
           
  foldM applyToWord sentence (IM.assocs sentence)

 where

  updateReading :: Cohort -> (Int,Lit) -> Cohort
  updateReading word (ana,newlit) = IM.adjust (const newlit) ana word

  updateCohort :: Sentence -> Int -> Cohort -> Sentence
  updateCohort sent i newsw = IM.adjust (const newsw) i sent

  lu' xs x = IM.lookup x xs
  lu  xs x = IM.findWithDefault false x xs -- neg will be called, so false will turn into true. I imagine that this is faster than call map twice?

--------------------------------------------------------------------------------  

--OBS. it's also perfectly fine to have an empty condition, ie. remove/select always!
mkConds :: Solver -> IS.IntSet -> Sentence -> Int -> [[Condition']] -> IO (Maybe [Lit])
mkConds s  sentence trgind disjconjconds str = do
  return Nothing
  -- * conds_absinds = all possible (absolute, not relative) SIndices in range 
  -- * call mkCond with arguments of type (Condition, [SIndex])
  -- * in mkCond, sometimes we can get away with a big orl that contains literals
  --   from different words, but with cautious or negation, can't do that

--------------------------------------------------------------------------------  


--We store only Ints in Sentence, but we have an [(Int,Reading)] handy 
--when we want to print out our readings.

mkSentence :: Solver -> [Reading] -> [[Reading]] -> IO Sentence
mkSentence s allrds rdss = do
  cohorts <- sequence [ IM.fromList `fmap` sequence
               [ (,) n `fmap` newLit s (showRd rd) | rd <- rds 
                                                 , let n = fromMaybe (error (show rd)) $ lookup rd rdMap ] -- ::[(Int,Lit)]
               | rds <- rdss ] -- ::[Cohort]
  let sentence = IM.fromList $ zip [1..] cohorts
  return sentence

 where
  rdMap = zip allrds [1..]
  showRd (WF foo:Lem bar:tags) = foo ++ "/" ++ bar ++ concatMap (\t -> '<':show t++">") tags

symbSentence :: Solver -> [Reading] -> Int -> IO Sentence
symbSentence s allrds w =
  IM.fromList `fmap` sequence 
     [ ((,) n . IM.fromList) `fmap` sequence 
       [ (,) m `fmap` newLit s (showSymbReading r n m) | (m, r) <- zip [1..] allrds ]
         | n <- [1..w] ] 
 where
  showSymbReading :: Reading -> Int -> Int -> String
  showSymbReading [Lem l] _ m =  l  ++ "_" ++ show m
  showSymbReading tags    i m = "w" ++ show i ++ concatMap (\t -> '<':show t++">") tags


--------------------------------------------------------------------------------  

mkTagMap :: [Tag] -> [[Tag]] -> TagMap
mkTagMap ts tcs = M.fromList $
                   ts `for` \t -> let getInds = IS.fromList . map (1+) . findIndices (elem t)
                                  in (t, getInds tcs) 
 where for = flip fmap


lookupTag :: TagMap -> IS.IntSet -> (Trg,Dif) -> (IS.IntSet,IS.IntSet)
lookupTag alltags allinds (trg,dif) = 
  let trgInds = if trg==[[]] then allinds --this means IF (-1 (*))
                   else IS.unions $ map (go allinds) trg --trg::[[Tag]]
      difInds = if dif==[[]] then IS.empty --this means IF (-1 something \\ nothing)
                      else IS.unions $ map (go allinds) dif --dif::[[Tag]]
          
  in  (trgInds IS.\\ difInds, difInds)
 where
  go acc []     = acc         --default is empty set, because intersect [] _ == []
  go acc (t:ts) = let inds = case t of
                              --(Rgx r s) -> IS.empty `fromMaybe` lookupRegex t alltags
                              _         -> IS.empty `fromMaybe` M.lookup t alltags
                  in go (IS.intersection acc inds) ts
                       
--lookupTag :: TagMap -> Cohort -> (Trg,Dif) -> (IS.IntSet,IS.IntSet)
--lookupTag alltags cohort (trg,dif) = 
-- let rdsInCohort = M.keys cohort :: IS.IntSet
--     trgLits = if trg==[[]] then allReadings --this means (*)
--                else concatMap (go rdsInCohort) trg
--     difLits = if dif==[[]] then [] --this means no - operator
--                else concatMap (go rdsInCohort) dif

-- in  (trgInds IS.\\ difInds, difInds)
-- where
--  go :: IS.IntSet -> [Tag] -> IS.IntSet -> IS.IntSet
--  go acc []     = acc         --default is empty set, because intersect [] _ == []
--  go acc (t:ts) = let inds = case t of
--                              --(Rgx r s) -> IS.empty `fromMaybe` lookupRegex t alltags
--                              _         -> IS.empty `fromMaybe` M.lookup t alltags
--                  in go (IS.intersection acc inds) ts


