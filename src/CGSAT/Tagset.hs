module CGSAT.Tagset where

import CGSAT.Base
import CGHS

import Data.List ( find )
import Data.Maybe ( catMaybes, fromMaybe )
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M

unknownLem :: Tag
unknownLem = Lem "unknown"

unknownWF :: Tag
unknownWF = WF "unknown"

unknownReading :: Reading
unknownReading = And [Tag "unknown reading"]

--------------------------------------------------------------------------------

-- Match is properly normalised, no complements
data Match = M MatchType 
               (OrList Tag) --Word forms
               (OrList Tag) --Lemmas
               IntSet       --Morph. readings
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
  M _ ws ls is -> null (getOrList ws) && null (getOrList ls) && IS.null is
  Bar _ m -> nullMatch m


-- TODO: make a proper function that makes a Match from a TagSet
foo :: TagSet -> Match
foo tagset = M Mix (Or [unknownWF])
                   (Or [unknownLem])
                   (IS.fromList [999])


-- TODO: sort out all this crap
{-
splitReading :: Reading -> SymbolicReading
splitReading (And tags) = SR wf lm rds
 where 
  wf = fromMaybe unknownWF (find isWF tags)
  lm = fromMaybe unknownLem (find isLem tags)
  onlyrds = filter (not.isLex) tags
  rds = if null onlyrds then unknownReading else onlyrds
  
  isLem (Lem _) = True
  isLem _       = False

  isWF (WF _) = True
  isWF _      = False

foo :: [SymbolicReading] -> Match
foo srs = undefined


normaliseYetAnother :: TagSet -> NotNormalMatch -- (??????)
normaliseYetAnother tagset = case normaliseTagsetRel tagset of
  AllTags -> undefined
  Diff s s' -> undefined -- split further into readings that have AnyBut for the s' side
  Set rds -> let symbReadings = fmap splitReading rds
  			 in 


complSetDiff :: ComplSet [a] -> ComplSet [a] -> ComplSet [a]
complSetDiff (CS anyof anybut) (CS anyof' anybut') 
  | null anybut && null anybut' = CS anyof anyof' -- A \ B = A ∩ cB
  | null anyof && null anyof' = CS anybut' anybut -- cA \ cB = B \ A



-- | Takes a tagset, with OrLists of underspecified readings,  and returns corresponding IntSets of fully specified readings.
-- Compare to normaliseRel in cghs/Rule: it only does the set operations relative to the underspecified readings, not with absolute IntSets.
-- That's why we cannot handle Diffs in normaliseRel, but only here.
-}
normaliseTagsetAbs :: TagSet -> Map Tag IntSet -> Either Match IntSet
normaliseTagsetAbs tagset tagmap = Left AllTags --TODO

{-
normaliseTagsetAbs tagset tagmap = case tagset of
  All -> Left AllTags
  Set s -> Right (lu s tagmap)
  Union t t' -> liftM2 IS.union (norm t) (norm t')
  Inters t t' -> liftM2 IS.intersection (norm t) (norm t')

{- Intended behaviour:
     adv adV (ada xx) `diff` ada == adv adV
     adv adV ada `diff` (ada xx) == adv adV (ada ((*) -xx)) 
     OBS. this all breaks when we split morph/synt. tags from lexical -}
  Diff t t' -> liftM2 IS.difference (norm t) (norm t')

          --if IS.null is -- only lexical tags on the left side
          --    then Left $ Not is' --TODO include Lex in what this returns too
          --    else Right $ 


{- say t=[n,adj] and t'=[m,f,mf]
   is  = specified readings that match t: (n foo), (n bar), (n mf), ... , (adj foo), (adj bar), (adj f), ..
   is' = specified readings that match t': (n m), (n f), (vblex sg p3 mf), (adj f), ..
   intersection of the specified readings returns those that match 
   sequence [[n,adj],[m,f,mf]]: (n m), (n f), ..., (adj f), (adj mf) -}
  Cart t t' -> liftM2 IS.intersection (norm t) (norm t')

 where
  norm = (`normaliseTagsetAbs` tagmap)

  lu :: OrList Reading -> Map Tag IntSet -> IntSet
  lu s m = IS.unions [ fold1 IS.intersection $ 
                         catMaybes [ M.lookup t m | t <- getAndList ts
                                                  , not $ isLex t ] -- don't include lexical tags
                        | ts <- getOrList s ]

  fold1 f [] = IS.empty
  fold1 f xs = foldl1 f xs


lemsAndWFs :: TagSet -> (OrList Tag,OrList Tag)
lemsAndWFs tagset = case normaliseTagsetRel tagset of
  Set s -> let onlytags = concatMap getAndList (getOrList s)
           in ( Or $ filter isLem onlytags, Or $ filter isWF onlytags )
  _     -> (Or [],Or []) -- TODO
 where 
  isLem (Lem _) = True
  isLem _       = False

  isWF (WF _) = True
  isWF _      = False

-}