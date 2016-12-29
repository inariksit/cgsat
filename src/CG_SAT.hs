{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CG_SAT where

import Rule
import SAT ( Solver(..), newSolver, deleteSolver )
import SAT.Named

import Data.IntMap ( IntMap, (!) )
import qualified Data.IntMap as IM
import Data.IntSet ( IntSet, intersection, unions )
import qualified Data.Map as M
import Data.List ( intercalate )
import Data.Maybe ( catMaybes )

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
               , rdMap :: IntMap Reading } 
               deriving (Show,Eq)



type Sentence = IntMap Cohort
type Cohort = IntMap Lit


data Pattern = Pat { positions :: OrList (Seq Int)
                   , contexts :: OrList (Seq Match) } 
             | PatAlways deriving (Show,Eq,Ord)

data Match = Mix IntSet -- Cohort contains tags in IntSet and tags not in IntSet
           | Cau IntSet -- Cohort contains only tags in IntSet
           | Not IntSet -- Cohort contains no tags in IntSet
           | AllTags -- Cohort matches all tags
           deriving (Show,Eq,Ord)


--------------------------------------------------------------------------------

ctx2Pattern :: Int  -- ^ Sentence length
            -> Int  -- ^ Absolute position of the target in the sentence
            -> Context -- ^ Context to be transformed
            -> CGMonad (OrList Pattern) -- ^ List of patterns. Length >1 only if the context is template.
ctx2Pattern senlen origin ctx = case ctx of
  Always -> return $ Or [PatAlways]
  Ctx posn polr tgst 
    -> undefined
  Link ctxs 
    -> undefined
  Template ctxs
    -> undefined
  Negate ctx -> undefined

 where 
  singleCtx2Pattern (Ctx posn polr tgst) = undefined



lu :: OrList Reading -> M.Map Tag IntSet -> IntSet
lu s m = unions [ foldl1 intersection $ 
                   catMaybes [ M.lookup t m | t <- getAndList ts ]
                    | ts <- getOrList s ]

normaliseAbs :: TagSet -> CGMonad (Maybe IntSet)
normaliseAbs tagset = case tagset of
  All -> return Nothing

  Set s {- For example: Set (Or [And [vblex, sg], And [n, pl]])
            ( Trg (Or [ IS(321,322,323) `intersect` IS(1,2,3,...,321,323) 
                      , IS(2,3,50,450) `intersect` IS(4,5,6,50,1908) ])
            , Dif mempty )
          -}
    -> do Just `fmap` lu s `fmap` asks tagMap


  Union t t' {- For example: Union (List (Or [And [vblex])
                                   (Diff (List (Or [And [vbser]) 
                                         (List (Or [And [aux]]) )
                This becomes two TD in the OrList: 
                Or [(Trg vblex, Dif mempty), (Trg vbser, Dif aux)].
                To match this OrList of TDs, a cohort has to match either. -}
    -> do td  <- normaliseAbs t
          td' <- normaliseAbs t'
          return undefined
  Inters t t'
    -> do td  <- normaliseAbs t
          td' <- normaliseAbs t'
          return undefined
  Diff t t' 
    -> undefined

      
  Cart ts ts' 
    -> undefined

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
  show = intercalate ">" . map show . getSeq