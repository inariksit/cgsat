{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CG_SAT where

import Rule
import SAT ( Solver(..), newSolver, deleteSolver )
import SAT.Named

import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM

--------------------------------------------------------------------------------


type Pattern = OrList [Int]
type TrgInd = Int

{-  tagMap ‾`v
         vblex |-> [321,         322,         323      ...]
                     |            |            |
                    vblex sg p1  vblex sg p2  vblex sg ...  <-- rdMap
-}

type TD = (TrgLits,DifLits)



type Sentence = IntMap Cohort
type Cohort = IntMap Lit


data Pattern = Pat { indices :: OrList (Seq CtxInd)
                   , trgdifs :: OrList (Seq TD) } deriving (Show,Eq)

applyParallel :: Solver -> Sentence -> Rule -> IO Sentence
applyParallel = apply


--------------------------------------------------------------------------------

-}

--------------------------------------------------------------------------------
-- General-purpose helper data types

-- Different from AndList and OrList, Seq is meant for things that follow 
-- each other. Unlike contextual tests in a rule, or tags in a tag set,
-- things that need to be in Seq are e.g. indices in a pattern.
newtype Seq a = Seq { getSeq :: [a] } deriving (Eq,Ord,Functor,Foldable,Monoid)

instance (Show a) => Show (Seq a) where
  show = intercalate "->" . map show . getSeq