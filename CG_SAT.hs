module CG_SAT (
  apply
)
where

import CG_base hiding ( Sentence, showSentence )
import CG_base as CGB
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
type TagMap   = M.Map Tag IntSet

type Cohort   = IM.IntMap Lit
type Sentence = IM.IntMap Cohort

--------------------------------------------------------------------------------


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


-- TODO!!! Make all the apply etc. functions here; unify format between Analyse and Disambiguate!

apply :: Solver -> Sentence -> Rule' -> IO Sentence
apply s sentence rule = do
  let ts_ds = trg rule
  let isSel = isSelect' rule

  let applyToWord sentence (i,cohort) = do
        let (trgLits,difLits) = lookupTag isSel ts_ds cohort -- :: (IntSet,IntSet)
                --if Select, then these are flipped already in lookupTag
        let allLits = M.elems cohort
        let otherLits = allLits \\ trgLits

        disjConds <- mkConds s sentence i (toConds $ cond rule)
        case disjConds of
          Nothing -> return sentence --conditions out of scope, no changes in sentence
          Just cs -> do
            

            condsHold <- orl' s cs
            let trgPos   = mapMaybe   (lu' word) trgIndsList
            let otherNeg = map (neg . lu word) (IS.toList otherInds)

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
           let newsw = foldl changeAna word (zip trgIndsList newTrgLits)
           return $ changeWord sentence i newsw
           
  foldM applyToWord sentence (IM.assocs sentence)

 where

  changeAna :: Cohort -> (Reading,Lit) -> Cohort
  changeAna word (ana,newlit) = IM.adjust (const newlit) ana word

  changeWord :: Sentence -> Int -> Word -> Sentence
  changeWord sent i newsw = IM.adjust (const newsw) i sent
--------------------------------------------------------------------------------  

--OBS. it's also perfectly fine to have an empty condition, ie. remove/select always!
mkConds :: Solver ->  Sentence -> SIndex -> [[Condition]] -> IO (Maybe Lit)
mkConds s  sentence trgind disjconjconds str = do
  return Nothing
  -- * conds_absinds = all possible (absolute, not relative) SIndices in range 
  -- * call mkCond with arguments of type (Condition, [SIndex])
  -- * in mkCond, sometimes we can get away with a big orl that contains literals
  --   from different words, but with cautious or negation, can't do that

--------------------------------------------------------------------------------  



mkSentence :: Solver -> [Reading] -> [[Reading]] -> IO Sentence
mkSentence s allrds rdss = 
 where
  readingMap = zip allrds [1..]
  cohorts = [ [ newLit s  | rd <- rds ]
              | rds <- rdss ]

  showReading rd

symbSentence :: Solver -> [Reading] -> Int -> IO Sentence
symbSentence s allrds w =
  IM.fromList `fmap` sequence 
     [ ((,) n . IM.fromList) `fmap` sequence 
       [ (,) m `fmap` newLit s (showSymbReading r n m) | (m, r) <- zip [1..] allrds ]
          n <- [1..w] ] 
 where
  showSymbReading :: Reading -> Int -> Int -> String
  showSymbReading [Lem l] _ m =  l  ++ "_" ++ show m
  showSymbReading tags    i m = "w" ++ show i ++ concatMap (\t -> '<':show t++">") tags

mkTagMap :: [Tag] -> [[Tag]] -> TagMap
mkTagMap ts tcs = M.fromList $
                   ts `for` \t -> let getInds = IS.fromList . map (1+) . findIndices (elem t)
                                  in (t, getInds tcs) 


--TODO: tagmap thingy, this doesn't work!
--Make it use Rule', then we can use same functions for both Analyse and Disambiguate.
{-
Trg and Dif contain [[Tag]]s, which is different from Reading!!! (also a [[Tag]])
Cohort has Readings, target has (Trg,Dif)s.
We need to find all the readings that have Trg and don't have Dif.

-}
                        
lookupTag :: Bool -> TagMap -> Cohort -> (Trg,Dif) -> (IntSet,IntSet)
lookupTag isSel alltags cohort (trg,dif) = 
 let allReadings = M.elems cohort :: IntSet
     trgLits = if trg==[[]] then allReadings --this means (*)
                else concatMap (go allReadings) trg
     difLits = if dif==[[]] then [] --this means no - operator
                else concatMap (go allReadings) dif
 where
  go :: [Lit] -> [Reading] -> [Lit]
  go acc []     = acc         --default is empty set, because intersect [] _ == []
  go acc (r:rs) = let inds = case t of
                              --(Rgx r s) -> IS.empty `fromMaybe` lookupRegex t alltags
                              _         -> IS.empty `fromMaybe` M.lookup t alltags
                  in go (IS.intersection acc inds) ts


