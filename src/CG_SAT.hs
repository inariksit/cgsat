{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CG_SAT where

import Rule hiding ( Not )
import qualified Rule as R
import SAT ( Solver(..), newSolver, deleteSolver )
import SAT.Named

import Data.IntMap ( IntMap, (!) )
import qualified Data.IntMap as IM
import Data.IntSet ( IntSet, unions, union, intersection, difference )
import Data.List ( intercalate )
import qualified Data.Map as M
import Data.Maybe ( catMaybes )

import Control.Monad ( liftM2 )
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader

--------------------------------------------------------------------------------

type CGMonad = ReaderT Env (StateT CgState IO)

type CgState = IntMap () --TODO

{-  tagMap 
           ‾‾`v

         vblex |-> [321,         322,         323      ...]
                     |            |            |
                    vblex sg p1  vblex sg p2  vblex sg ...  
                ___,^
           rdMap 
-}
data Env = Env { tagMap :: M.Map Tag IntSet 
               , rdMap :: IntMap Reading 
               , allTags :: IntSet } 
               deriving (Show,Eq)



type Sentence = IntMap Cohort
type Cohort = IntMap Lit


data Pattern = Pat { positions :: OrList (Seq Int)
                   , contexts :: Seq Match } 
             | PatAlways deriving (Show,Eq,Ord)

data Match = Mix IntSet -- Cohort contains tags in IntSet and tags not in IntSet
           | Cau IntSet -- Cohort contains only tags in IntSet
           | Not IntSet -- Cohort contains no tags in IntSet
           | NotCau IntSet -- Cohort contains (not only) tags in IntSet:
                            -- either of Not and Mix are valid.
           | AllTags -- Cohort matches all tags
           deriving (Show,Eq,Ord)


--------------------------------------------------------------------------------

ctx2Pattern :: Int  -- ^ Sentence length
            -> Int  -- ^ Absolute position of the target in the sentence
            -> Context -- ^ Context to be transformed
            -> CGMonad (OrList Pattern) -- ^ List of patterns. Length >1 only if the context is template.
ctx2Pattern senlen origin ctx = case ctx of
  Always -> return $ Or [PatAlways]
  c@(Ctx posn polr tgst)
    -> do (ps,m) <- singleCtx2Pat c
          return $ Or [ Pat (fmap (\x -> Seq [x]) ps) (Seq [m]) ]
          
  Link ctxs 
    -> undefined
  Template ctxs
    -> undefined
  Negate ctx -> undefined

 where 
  singleCtx2Pat (Ctx posn polr tgst) = 
    do tagset <- normaliseAbs tgst
       let allPositions = pos2inds posn senlen origin
       let match = maybe AllTags (getMatch posn polr) tagset
       return (allPositions,match) 

pos2inds :: Position -> Int -> Int -> OrList Int
pos2inds posn senlen origin = 
  case scan posn of 
    Exactly -> Or [origin + relInd]
    _       -> Or absInds
 where
  relInd = R.pos posn
  absInds = if relInd<0 then [1 .. origin+relInd]
                else [origin+relInd .. senlen]

getMatch :: Position -> Polarity -> (IntSet -> Match)
getMatch (Pos _ NC _) Yes = Mix
getMatch (Pos _ NC _) R.Not = Not
getMatch (Pos _ C _)  Yes = Cau
getMatch (Pos _ C _)  R.Not = NotCau


-- | Takes a tagset, with OrLists of underspecified readings,
-- and returns corresponding IntSets of fully specified readings.
-- Compare to normaliseRel in cghs/Rule: it only does the set operations
-- relative to the underspecified readings, not with absolute IntSets.
-- That's why we cannot handle Diffs in normaliseRel, but only here.
normaliseAbs :: TagSet -> CGMonad (Maybe IntSet)
normaliseAbs tagset = case tagset of
  All   -> return Nothing

  Set s -> Just `fmap` lu s `fmap` asks tagMap

  Union t t' 
        -> do is  <- normaliseAbs t
              is' <- normaliseAbs t'
              return $ liftM2 union is is'
  Inters t t'
    -> do is  <- normaliseAbs t
          is' <- normaliseAbs t'
          return $ liftM2 intersection is is'

-- Intended behaviour:
--   adv adV (ada "very") `diff` ada == adv adV
--   adv adV ada `diff` (ada "very") == adv adV (ada xxx) (ada yyy) ... 
-- This works properly now: normaliseAbs returns IntSets, 
-- and `normaliseAbs (ada)` returns all of (ada xxx), (ada yyy), (ada "very"), ...
-- Then, the difference with `normaliseAbs (adv adV (ada "very")) will only exclude (ada "very").
  Diff t t' 
    -> do is  <- normaliseAbs t
          is' <- normaliseAbs t'
          return $ liftM2 difference is is'

-- say t=[n,adj] and t'=[m,f,mf]
-- is  = specified readings that match t: (n foo), (n bar), (n mf), ... , (adj foo), (adj bar), (adj f), ..
-- is' = specified readings that match t': (n m), (n f), (vblex sg p3 mf), (adj f), ..
-- intersection of the specified readings returns those that match 
-- sequence [[n,adj],[m,f,mf]]: (n m), (n f), ..., (adj f), (adj mf)
  Cart t t' 
    -> do is  <- normaliseAbs t
          is' <- normaliseAbs t'
          return $ liftM2 intersection is is'


 where
  lu :: OrList Reading -> M.Map Tag IntSet -> IntSet
  lu s m = unions [ foldl1 intersection $ 
                     catMaybes [ M.lookup t m | t <- getAndList ts ]
                      | ts <- getOrList s ]

{-
Make this only once, at the beginning:
rdMap --  1 |-> vblex sg p3
          2 |-> noun sg mf
               ...
       9023 |-> "aurrera" adv

Make this only once, at the beginning:
tagMap    --  vblex |-> IS(1,30,31,32,..,490)
              sg    |-> IS(1,2,3,120,1800)
              mf    |-> IS(2,20,210)
              adv   |-> IS()

Lookup from tagMap, and perform intersections with results,
many times per rule --- once for each tagset.

-}

--------------------------------------------------------------------------------
-- General-purpose helper data types

-- Different from AndList and OrList, Seq is meant for things that follow 
-- each other. Unlike contextual tests in a rule, or tags in a tag set,
-- Seq models the behaviour of positions and matches in a pattern.
newtype Seq a = Seq { getSeq :: [a] } deriving (Eq,Ord,Functor,Foldable,Monoid)

instance (Show a) => Show (Seq a) where
  show = intercalate "," . map show . getSeq