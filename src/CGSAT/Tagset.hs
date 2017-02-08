module CGSAT.Tagset where

import CGSAT.Base
import CGHS hiding ( Tag(..), Reading )
import qualified CGHS

import Data.Foldable ( fold )
import Data.List ( find, (\\) )
import Data.Maybe ( catMaybes, fromMaybe, mapMaybe )
--import qualified Data.IntMap as IM
import qualified Data.Map as M


--------------------------------------------------------------------------------

-- Match is properly normalised, no complements
data Match = M MatchType 
               SplitReading
               --(OrList WF) --Word forms
               --(OrList Lem) --Lemmas
               --IntSet       --Morph. readings
           | Bar (Int,Match) Match -- ^ Cohort matches the second Match, and requires that 
                                   -- there is no match to the first Match, up to the Int.
           | AllTags -- Cohort may contain any tag
           deriving ( Eq )



data MatchType
  = Mix -- ^ Cohort contains tags in IntSet, and may contain tags outside IntSet.
        -- OBS. not same as target, which *must* contain tags outside IntSet.
  | Cau -- ^ Cohort contains only tags in IntSet.
  | Not -- ^ Cohort contains no tags in IntSet.
  | NotCau -- ^ Cohort contains (not only) tags in IntSet:
           -- either of Not and Mix are valid.
 deriving ( Eq )

nullMatch :: Match -> Bool
nullMatch m = case m of
  AllTags -> False
  M _ (SR ws ls rs) -> null (getOrList ws) && null (getOrList ls) && null (getOrList rs)
  Bar _ m -> nullMatch m


-- TODO: make a proper function that makes a Match from a TagSet
foo :: TagSet -> OrList Match
foo tagset = Or [M Mix (SR (Or [])
                           (Or [])
                           (Or [])
                        ) 
                ]


splitReading :: CGHS.Reading -> SplitReading 
splitReading rd = SR wf lm rds
 where
  (onlymorph,lextags) = removeLexReading rd
  (wfs,lms) = (mapMaybe fromWF lextags,mapMaybe fromLem lextags)
  wf = Or $ if null wfs then [] else wfs
  lm = Or $ if null lms then [] else lms
  rds = Or $ if null onlymorph then [] else [fromReading onlymorph]


foldSplitReading :: (Foldable t, Functor t) => t SplitReading -> SplitReading
foldSplitReading srs = SR (fold $ fmap sr_w srs) (fold $ fmap sr_l srs) (fold $ fmap sr_r srs)

-- | Takes a tagset, with OrLists of underspecified readings,  and returns corresponding IntSets of fully specified readings.
-- Compare to normaliseRel in cghs/Rule: it only does the set operations relative to the underspecified readings, not with absolute IntSets.
-- That's why we cannot handle Diffs in normaliseRel, but only here.
normaliseTagsetAbs :: TagSet -> Env -> Either Match (OrList SplitReading)
normaliseTagsetAbs tagset e@(Env _ _ envRds _) = 
  --TODO: we can handle Cartesian products, and some other things nicer
  -- if we do it here without normaliseTagsetRel.
  -- Look into it later. Also finish Diff.
  case normaliseTagsetRel tagset of
    All ->  Left AllTags
    Set s -> Right (fmap splitReading s) -- OrList SplitReading
    Diff s s' 
     -> let ns = normaliseTagsetAbs s  e
            ns' = normaliseTagsetAbs s' e
        in case (ns,ns') of
             (Left AllTags, _) -> undefined  --TODO: this is a legit case
             (Right x, Right y) -> do -- TODO: this is very suspicious
                let allX = foldSplitReading x
                let allY = foldSplitReading y

                let specifiedXMorphRds = fold $ fmap (specifyMorphReading envRds) (sr_r allX) :: OrList MorphReading
                let specifiedYMorphRds = fold $ fmap (specifyMorphReading envRds) (sr_r allY) :: OrList MorphReading
                let diffMorphRds = Or $ getOrList specifiedXMorphRds \\ getOrList specifiedYMorphRds
                let diffWFs = Or $ (getOrList $ sr_w allX) \\ (getOrList $ sr_w allY)
                let diffLems = Or $ (getOrList $ sr_l allX) \\ (getOrList $ sr_l allY)
                Right (Or [SR diffWFs diffLems diffMorphRds])
             (Right x, Left y) -> Left AllTags
             (Left x, Left y) -> Left AllTags

    x  -> undefined --UnknownError ("normaliseTagsetAbs: weird tagset found " ++ show x)


 where
  specifyMorphReading :: OrList MorphReading -> MorphReading -> OrList MorphReading
  specifyMorphReading envrds (Rd underspTaglist) =
    let justTaglists = fmap (getReading) envrds :: OrList (AndList Tag)
        specifiedTagList = Or $ filter (`includes` underspTaglist) (getOrList justTaglists)
    in fmap Rd specifiedTagList

includes :: (Eq a, Foldable t) => t a -> t a -> Bool
includes t1 t2 = all (`elem` t1) t2

