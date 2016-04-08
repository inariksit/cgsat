module CG_SAT (
  apply
, applyParallel
, mkTagMap
, ruleToRule'
, mkSentence'
, Rule' (..)
, Sentence' (..)
, solveAndPrintSentence
)
where

import CG_base hiding (isCareful)
import qualified CG_base as CGB
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

type Cohort'   = IM.IntMap Lit
type Sentence' = IM.IntMap Cohort'

type Clause = [Lit]

--------------------------------------------------------------------------------

type WIndex   = Int
type SIndex   = Int
type WIndSet  = IS.IntSet
type TrgIS    = IS.IntSet
type DifIS    = IS.IntSet
type TrgInds = (TrgIS,DifIS)
type CondInds = (TrgIS,DifIS)

--Condition' is like CG_base's Condition, but s/TagSet/CondInds/.
--Storing TrgInds and CondInds instead of [[Tag]] for efficiency.
--With [[Tag]], every time we check if a rule matches a cohort,
--we check if any of the [[Tag]] in rule is a sublist of the [Tag] in cohort.

--Each reading is given an index, and a cohort is an IntMap 
--from an index to a literal which corresponds to that reading.
--A rule, both in condition and target, has also a set of readings.
--For example: `REMOVE verb sg p3' becomes `R {trg=fromList [320], ...} 
--assuming that `verb sg p3' corresponds to index 320.
--If the reading in rule is underspecified, we include indices of all readings 
--that contain the underspecified reading. For example:
--  REMOVE verb sg p3  ====> R {trg=fromList [318,319,320,321,322,333], ...} 
--This way, we need less lookups.
data Condition' = C' Position' (Polarity,[CondInds]) | Always' deriving (Show,Eq)

--We need to do the same for Position, because of the TagSets in Barriers.
data Position' = Exactly' {isCareful::Cautious, ind'::Int}
               | AtLeast' {isCareful::Cautious, ind'::Int}
               | Barrier' {isCareful::Cautious, ind'::Int, binds::[CondInds]}
               | CBarrier' {isCareful::Cautious, ind'::Int, binds::[CondInds]}
               | LINK' {parent::Position' , self::Position', isCareful::Cautious} deriving (Show,Eq)


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

  conds = (map.map) condToCond' (toConds $ cond rule)

  condToCond' (C position (polarity, ctags)) = 
     let ctags'    = map lu (toTags ctags)
         position' = posToPos' position
     in  C' position' (polarity, ctags')
  condToCond' Always = Always'

  posToPos' position = case position of
    Exactly c i -> Exactly' c i
    AtLeast c i -> AtLeast' c i
    Barrier c i btags -> Barrier' c i (map lu $ toTags btags)
    CBarrier c i btags -> CBarrier' c i (map lu $ toTags btags)
    LINK p1 p2  -> LINK' (posToPos' p1) (posToPos' p2) (CGB.isCareful p2)

--------------------------------------------------------------------------------

apply :: Solver -> Sentence' -> Rule' -> IO Sentence'
apply s sentence rule = let (allTrgInds,allDifInds) = trg rule in
 if IS.null allTrgInds
  then return sentence --rule doesn't target anything in the sentence
  else do 

    let applyToWord sentence (i,cohort) = do
          let indsInCohort' = IM.keysSet cohort

          let (trgIndsRaw,otherIndsRaw) = 
                IS.partition (\i -> IS.member i allTrgInds && 
                                    IS.notMember i allDifInds)
                             indsInCohort'
          let (trgInds,otherInds) = if isSelect' rule --if Select, flip target and other
                                      then (otherIndsRaw,trgIndsRaw)
                                      else (trgIndsRaw,otherIndsRaw)

          disjConds <- mkConds s sentence i (cnd rule)
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
              return $ updateCohort' sentence i newcoh

    foldM applyToWord sentence (IM.assocs sentence)

 where

  updateReading :: Cohort' -> (Int,Lit) -> Cohort'
  updateReading word (ana,newlit) = IM.adjust (const newlit) ana word

  updateCohort' :: Sentence' -> Int -> Cohort' -> Sentence'
  updateCohort' sent i newsw = IM.adjust (const newsw) i sent

  lu' xs x = IM.lookup x xs
  lu  xs x = IM.findWithDefault false x xs -- neg will be called, so false will turn into true. I imagine that this is faster than call map twice?

--------------------------------------------------------------------------------  

applyParallel :: Solver -> Sentence' -> Rule' -> IO [Clause]
applyParallel s sentence rule = let (allTrgInds,allDifInds) = trg rule in
 if IS.null allTrgInds
  then return [] --rule doesn't target anything in the sentence
  else do
    let applyToWord sentence (i,cohort) = do
          let (trgIndsRaw,otherIndsRaw) = 
                IS.partition (\i -> IS.member i allTrgInds && 
                                    IS.notMember i allDifInds)
                             (IM.keysSet cohort)
          let (trgInds,otherInds) = if isSelect' rule --if Select, flip target and other
                                      then (otherIndsRaw,trgIndsRaw)
                                      else (trgIndsRaw,otherIndsRaw)
          disjConds <- mkConds s sentence i (cnd rule)
          case disjConds of
            Nothing -> return [] --conditions out of scope, no clauses created
            Just cs -> do 
              let trgPos = mapMaybe (\x -> IM.lookup x cohort) (IS.toList trgInds)
              condsHold <- orl' s cs
              trgNeg <- andl' s (map neg trgPos) --must be and of "all neg", not neg of "and of all pos"
              return [neg condsHold, trgNeg] --going back to SAT-CG 2015 style
              --implies s "" condsHold trgNeg

    mapM (applyToWord sentence) (IM.assocs sentence)


--------------------------------------------------------------------------------  

--OBS. it's also perfectly fine to have an empty condition, ie. remove/select always!
mkConds :: Solver -> Sentence' -> SIndex -> [[Condition']] -> IO (Maybe [Lit])
mkConds    s         sentence    trgind  disjconjconds = do
  --Extracting only indices seems silly for the non-symbolic case.
  --But testing the match and passing cohorts is silly for symbolic.
  --Compromise: separate `mkConds' and `symbConds' , both use same mkCond?
  let cs_is = [ ci | conjconds <- disjconjconds 
                   , let ci = [ (c, is) | c <- conjconds
                                        , let is = IM.keys $
                                               IM.filterWithKey
                                                 (matchCond trgind c)
                                                 sentence
                                        , not . null $ is ]
                   , length ci == length conjconds ]
  if all null cs_is 
    then return Nothing
    else Just `fmap` mapM (mkCond s sentence trgind) cs_is

  -- * conds_absinds = all possible (absolute, not relative) SIndices in range
  -- * call mkCond with arguments of type (Condition, [SIndex])

{-Barrier: if a literal found in the absinds is earlier than any found barrier,
  we don't need to add barriers in the formula, just run `go'.
  General case (symbolic sentences): if we assume that every index contains also
  the barrier, we need to add `& ~barrier' to all clauses.
-} 
                             --for Barrier
mkCond :: Solver -> Sentence' -> SIndex -> [(Condition',[SIndex])] -> IO Lit
mkCond    s         sentence     abstrgind   conjconds_abscondinds = 
  case conjconds_abscondinds of
    [] -> error "mkCond: no conditions"
    -- *Every* condition has to be true in *some* index:
    --  = andl'                             = orl'
    cs -> andl' s =<< sequence
           [ orl' s =<< sequence [ go cond abscondind | abscondind <- abscondinds ] 
            | (cond,abscondinds) <- conjconds_abscondinds ]
 where 

  --TODO check if this is still valid, I just copypasted it from the old version
  lookup' :: Polarity -> SIndex -> WIndSet -> [Lit]
  lookup' p ai is = case IM.lookup ai sentence of
                      Nothing -> if p==Pos then error "mkCond: index out of bounds"
                                  else [true] --if no -100, then `NOT -100 foo' is true
                      Just wd -> mapMaybe (\i -> IM.lookup i wd) (IS.toList is)



  go :: Condition' -> Int -> IO Lit
  go Always' _ = return true
  go c@(C' position (polarity,yesinds_difinds)) abscondind = do 

    let allinds = IM.keysSet sentence
    let yi_ALLINONE = IS.unions $ fst $ unzip yesinds_difinds
    let oi_ALLINONE = allinds IS.\\ yi_ALLINONE

    let lu = lookup' polarity abscondind

    --TODO: check this another day, it's probably buggy
    fuckTheBarriers <- 
     case position of 
      (Barrier'  _ relcondind btags) 
        -> do let hackedCond = C' (AtLeast' False relcondind) (Pos,btags)

              --barind, i.e. the -1 in `if -1* foo BARRIER bar',
              --is included in hackedCond. 
              --trgind is the original target index all the way from apply.
              --abscondind is the current cohort we are looking, 
              --if we can find some valid conditions in it.
              --Now, if there is a barrier before abscondind, the only way for
              --the reading in abscondind to be valid is that the barrier is false.
              --Hence we need to make literal that says
              -- "all the potential barriers before abscondind are false"
              let scan = if (relcondind<0) then (>) else (<)
              let cohortsWithBtags = filter (scan abscondind) $ IM.keys $
                   IM.filterWithKey (matchCond abstrgind hackedCond) sentence
              let bInds = IS.unions [ yes | (yes,dif) <- btags ] --TODO make this work properly
              let bLits = concatMap (\ai -> lookup' Pos ai bInds) cohortsWithBtags
              andl s "all potential barriers are false" (map neg bLits)

      --(CBarrier' _ relcondind btags)     --only difference: s/False/True/
      --  -> do let hackedCond = C' (AtLeast' True barind) (Pos,bartags)

      _ -> return true

    case (polarity, isCareful position) of                  
      (Pos, False) -> orl s "" =<< sequence
                          [ do let yesLits = lu yi
                               let difLits = lu di
                               y <- orl  s "" yesLits
                               n <- andl s "" (map neg difLits)
                               andl s "" [y,n,fuckTheBarriers]
                            | (yi, di) <- yesinds_difinds ] 
                            --only case where we may need difInds:
                            --with rule like A\B OR C\D, we can have
                            --a)  MUST have one: [a, ac, ad, acd, ae ..] 
                            --    MAY NOT have:  [b, ab, ..]
                            --    CAN have:      [d, cd, e, ..]

                            --b)  MUST have one: [c, ac, bc, ace, ce ..] 
                            --    MAY NOT have:  [d, cd, ...]
                            --    CAN have:      [b, ab, e, ..]

      (Pos, True)  -> do let yesLits = lu yi_ALLINONE
                         let otherLits = lu oi_ALLINONE
                         y <- orl  s "" yesLits
                         n <- andl s "" (map neg otherLits)
                         andl s "" [y,n,fuckTheBarriers] 

                       --OBS. "yesInds" mean "noInds" for NOT
      (Neg,False) -> do let noLits = lu yi_ALLINONE
                          --let otherLits = lu oi_ALLINONE
                        andl s "" $ fuckTheBarriers:(map neg noLits)

      (Neg, True) -> do let otherLits = lu oi_ALLINONE
                        notCautious <- orl s "" otherLits
                        andl s "" [fuckTheBarriers,notCautious]

--------------------------------------------------------------------------------  

--Checks if give cohort matches given condition.
--If condition has *:  IF (*1 pr)
-- 1     2      3         4     5      6
-- the   bear   sleeps    in    the    house
--       trgi=2           
matchCond :: SIndex -> Condition' -> SIndex -> Cohort' -> Bool
matchCond _            Always'       _         _      = True
matchCond trgind (C' pos (pol,cndinds)) absind cohort = inScope && tagsMatch

 where
  maxLen    = 50 -- arbitrary maximum length of sentence
  inScope   = pol==Neg || --NOT -100 foo is always true, if there is no index -100
                absind `elem` possibleInds pos
  tagsMatch = tagsMatchRule cndinds cohort 

  possibleInds p = case p of 
                    Exactly' _ i -> [trgind+i]
                    AtLeast' _ i -> let j = if (i<0) then i+1 else i-1
                                    in (trgind+) <$> take maxLen [i,j..]
                                     
--Barriers look suspicious, but correct behaviour happens in mkConds.
                    Barrier'  _ i _ -> possibleInds (AtLeast' False i)
                    CBarrier' _ i _ -> possibleInds (AtLeast' False i)
                    LINK' _ child _ -> possibleInds child


tagsMatchRule :: [CondInds] -> Cohort' -> Bool
tagsMatchRule trg_difs cohort = any (match readings) trg_difs
 where
  readings = IM.keysSet cohort
  match rds (trg,_) = not $ IS.null $ IS.intersection trg rds

{- Cohort' has [ (57,casa/casa<n><f><sg>),
              , (58,casa/casar<vblex><pri><p3><sg>)
              , (59,casa/casar<vblex><imp><p2><sg>) ]
 CondInds has [ (trg=[58,59,60], dif=[15,38,57])
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


--------------------------------------------------------------------------------  

--We store only Ints in Sentence, but we have an [(Int,Reading)] handy 
--when we want to print out our readings.

mkSentence' :: Solver -> [Reading] -> [[Reading]] -> IO Sentence'
mkSentence' s allrds rdss = do
  cohorts <- sequence [ IM.fromList `fmap` sequence
               [ (,) n `fmap` newLit s (shRd rd) | rd <- rds 
                                                 , let Just n = lookup rd rdMap ] -- ::[(Int,Lit)]
               | rds <- rdss ] -- ::[Cohort']
  let sentence = IM.fromList $ zip [1..] cohorts
  return sentence

 where
  rdMap = zip allrds [1..]
  shRd (WF foo:Lem bar:tags) = foo ++ "/" ++ bar ++ concatMap (\t -> '<':show t++">") tags

symbSentence :: Solver -> [Reading] -> Int -> IO Sentence'
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

-------------------------------------------------------------------------------- 

solveAndPrintSentence :: Bool -> Solver -> [Lit] -> Sentence' -> IO ()
solveAndPrintSentence verbose s ass sent = do
  b <- solve s ass
  if b then do
          when verbose $ print ass
          vals <- sequence 
                   [ sequence [ modelValue s lit | lit <- IM.elems word ] 
                      | (sind,word) <- IM.assocs sent ]
          let trueAnas =
               [ "\"w" ++ show sind ++ "\"\n"
                  ++ unlines [ "\t"++show ana | (ana, True) <- zip (IM.elems word) vs ]
                 | ((sind,word), vs) <- zip (IM.assocs sent) vals ]
          mapM_ putStrLn trueAnas
          putStrLn "----"
      else do
        putStrLn $ "solveAndPrintSentence: Conflict with assumptions " ++ show ass