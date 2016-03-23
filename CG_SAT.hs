module CG_SAT (
  apply
, mkTagMap
, ruleToRule'
, mkSentence
, Rule' (..)
)
where

import CG_base hiding ( Cohort, Sentence )
import SAT ( Solver(..), newSolver, deleteSolver )
import SAT.Named

import Control.Monad
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import Data.List
import qualified Data.Map as M
import Data.Maybe
--import Debug.Trace
import Text.Regex


--------------------------------------------------------------------------------  

-- vblex |-> [vblex sg p3, vblex pl p1, vblex ]
type TagMap   = M.Map Tag IS.IntSet

type Cohort   = IM.IntMap Lit
type Sentence = IM.IntMap Cohort

--------------------------------------------------------------------------------

type WIndex   = Int
type SIndex   = Int
type WIndSet  = IS.IntSet
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
               } deriving (Eq,Show)

--instance Show Rule' where
--  show = show'

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
apply s sentence rule = let (allTrgInds,allDifInds) = trg rule in
 if IS.null allTrgInds
  then return sentence --rule doesn't target anything in the sentence
  else do 

    let applyToWord sentence (i,cohort) = do
          let indsInCohort = IM.keysSet cohort

          let (trgIndsRaw,otherIndsRaw) = 
                IS.partition (\i -> IS.member i allTrgInds && 
                                    IS.notMember i allDifInds)
                             indsInCohort
          let (trgInds,otherInds) = if isSelect' rule --if Select, flip target and other
                                      then (otherIndsRaw,trgIndsRaw)
                                      else (trgIndsRaw,otherIndsRaw)

          disjConds <- mkConds s indsInCohort sentence i (cnd rule)
          case disjConds of
            Nothing -> return sentence --conditions out of scope, no changes in sentence
            Just cs -> do
              let trgIndsList = IS.toList trgInds
              let trgPos   = mapMaybe (lu' cohort) trgIndsList
              let otherNeg = map (neg . lu cohort) (IS.toList otherInds)

              condsHold     <- orl' s cs
              someTrgIsTrue <- orl' s trgPos 
              noOtherIsTrue <- andl' s otherNeg
              onlyTrgLeft   <- andl' s [ someTrgIsTrue, noOtherIsTrue ]
              cannotApply   <- orl' s [ neg condsHold, onlyTrgLeft ]

              newTrgLits <- sequence               --wN<a>' is true if both of the following:
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
-- For non-symbolic case, allinds contains all readings in that sentence.
-- For symbolic case, allinds has all possible readings in the lexicon/grammar.
mkConds :: Solver -> WIndSet -> Sentence -> SIndex -> [[Condition']] -> IO (Maybe [Lit])
mkConds    s         readings   sentence    trgind  disjconjconds = do

  --Extracting only indices seems silly for the non-symbolic case.
  --But testing the match and passing cohorts is silly for symbolic.
  --Compromise: separate `mkConds' and `symbConds' , both use same mkCond?
  let cs_is = [ [ (cond, absinds) | cond <- conjconds
                                  , let absinds = IM.keys $
                                         IM.filterWithKey (matchCond trgind cond) sentence
                                  , not . null $ absinds ]
                | conjconds <- disjconjconds ]
  if all null cs_is 
    then return Nothing
    else Just `fmap` mapM (mkCond s readings sentence) cs_is

  -- * conds_absinds = all possible (absolute, not relative) SIndices in range 
  -- * call mkCond with arguments of type (Condition, [SIndex])


mkCond :: Solver -> WIndSet -> Sentence -> [(Condition',[SIndex])] -> IO Lit
mkCond    s         readings      sentence    conjconds_absinds = 
  case conjconds_absinds of
    [] -> error "mkCond: no conditions"
    -- *Every* condition has to be true in *some* index:
    --  = andl'                             = orl'
    cs -> andl' s =<< sequence
           [ orl' s =<< sequence [ go cond absind | absind <- absinds ] 
            | (cond,absinds) <- conjconds_absinds ]
 where 
  go :: Condition' -> Int -> IO Lit
  go Always' _ = return true
  go c@(C' position (positive,yesInds_difInds)) absind = do 

    let allinds = IM.keysSet sentence
    let yi_ALLINONE = IS.unions $ fst $ unzip yesInds_difInds
    let oi_ALLINONE = allinds IS.\\ yi_ALLINONE

    case position of
      (Barrier  foo bar btags) 
        -> do --putStrLn "found a barrier!"
              --addClause s [some nice barrier clause] --TODO
              return ()
                                         
      (CBarrier foo bar btags)
        -> do --putStrLn "found a cbarrier!"
              --addClause s [some nice barrier clause] --TODO
              return ()
      _ -> return ()
    return true
--------------------------------------------------------------------------------  

--This function only looks if a certain cohort matches condition.
--If condition has *:  IF (*1 pr)
-- 1     2      3         4     5      6
-- the   bear   sleeps    in    the    house
--       trgi=2           
matchCond :: SIndex -> Condition' -> SIndex -> Cohort -> Bool
matchCond _            Always'       _         _    = True
matchCond trgind (C' pos (b,cndinds)) absind cohort = inScope && tagsMatch

 where
  maxLen = 50 -- arbitrary maximum length of sentence
  inScope   = not b || --NOT -100 foo is always true, if there is no index -100
                absind `elem` possibleInds pos
  tagsMatch = tagsMatchRule cndinds cohort 

--  absIndices :: SIndex -> Position' -> [SIndex]
  possibleInds p = case p of 
                    Exactly _ i -> (trgind+) <$> [i]
                    AtLeast _ i -> let j = if (i<0) then i+1 else i-1
                                   in (trgind+) <$> take maxLen [i,j..]
                                     
                    --TODO fix barrier case
                    Barrier  _ i _ -> possibleInds (AtLeast False i)
                    CBarrier _ i _ -> possibleInds (AtLeast False i)
                    LINK _ child   -> possibleInds child


{- Cohort has [ (57,casa/casa<n><f><sg>),
              , (58,casa/casar<vblex><pri><p3><sg>)
              , (59,casa/casar<vblex><imp><p2><sg>) ]
Condition has [ (trg=[58,59,60], dif=[15,38,57])
              , (trg=[foo],      dif=[bar]) 
              , ...                         ] 

OBS. if "<casa>" had 57-59 from the beginning, it always matches 57-59,
     no matter if 57 has been negated!
For now, exclude dif from this function.
We can always access difs when we make SAT-clauses.

OBS. Rule' can have overlapping trg and dif;
     that's an internal conflict, and the rule should never apply anywhere.
Handling that too in the SAT-clauses.

-}
tagsMatchRule :: [(TrgIS,DifIS)] -> Cohort -> Bool
tagsMatchRule trg_difs cohort = any (match readings) trg_difs
 where
  readings = IM.keysSet cohort
  match rds (trg,_) = not $ IS.null $ IS.intersection trg rds



--------------------------------------------------------------------------------  

--We store only Ints in Sentence, but we have an [(Int,Reading)] handy 
--when we want to print out our readings.

mkSentence :: Solver -> [Reading] -> [[Reading]] -> IO Sentence
mkSentence s allrds rdss = do
  cohorts <- sequence [ IM.fromList `fmap` sequence
               [ (,) n `fmap` newLit s (shRd rd) | rd <- rds 
                                                 , let Just n = lookup rd rdMap ] -- ::[(Int,Lit)]
               | rds <- rdss ] -- ::[Cohort]
  let sentence = IM.fromList $ zip [1..] cohorts
  return sentence

 where
  rdMap = zip allrds [1..]
  shRd (WF foo:Lem bar:tags) = foo ++ "/" ++ bar ++ concatMap (\t -> '<':show t++">") tags

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


