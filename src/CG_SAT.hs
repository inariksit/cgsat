{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CG_SAT where

import Rule
import SAT ( Solver(..), newSolver, deleteSolver )
import SAT.Named

import Data.IntMap ( IntMap, (!) )
import qualified Data.IntMap as IM
import Data.IntSet ( IntSet, intersection )
import qualified Data.Map as M
import Data.List ( intercalate )
import Data.Maybe ( catMaybes )

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
--import qualified Control.Monad.State as S
--import qualified Control.Monad.Reader as R

--------------------------------------------------------------------------------


type CGMonad = ReaderT Env (StateT CgState IO)

{-  tagMap ‾`v
         vblex |-> [321,         322,         323      ...]
                     |            |            |
                    vblex sg p1  vblex sg p2  vblex sg ...  <-- rdMap
-}
data Env = Env { tagMap :: M.Map Tag IntSet 
               , rdMap :: IntMap Reading } 
               deriving (Show,Eq)

type CgState = IntMap Lit

type Reading = [Tag]



type Sentence = IntMap Cohort
type Cohort = IntMap Lit


data Pattern = Pat { indices :: OrList (Seq CtxInd)
                   , trgdifs :: OrList (Seq TD) } deriving (Show,Eq)

type TrgInd = Int
type CtxInd = Int

type TD = (Trg,Dif)

data Trg = Trg (OrList IntSet) | AllTags deriving (Show,Eq)
data Dif = Dif (OrList IntSet) | Complement deriving (Show,Eq)

--prop. test: ensure that [TD]-list is as long as [CtxInd]-lists in patterns
--unit tests: all the test cases I've written down -- generalise to properties?
ctx2Pattern :: Int  -- ^ Sentence length
            -> Int  -- ^ Absolute index of the target in the sentence
            -> Context -- ^ Context to be transformed
            -> OrList Pattern -- ^ List of patterns. Length >1 only if the context is template.
ctx2Pattern senlen trgind ctx = undefined
{- tdFooBar = Or[ Seq [(Trg Foo, Dif mempty), (Trg Bar, Dif mempty)] ]

            6 4 (-1  Foo LINK 1  Bar) = Or [ ( Or [[3,4]]
                                             , td_FooBar ) 
                                           ]
            6 4 (-1* Foo LINK 1  Bar) = Or [ ( Or [[1,2],[2,3][3,4]]
                                             , td_FooBar )
                                           ]
            6 4 (-1  Foo LINK 1* Bar) = Or [ ( Or [[3,4],[3,5],[3,6]]
                                             , td_FooBar )
                                           ]
            6 4 (-1* Foo LINK 1* Bar) = Or [ ( Or [ [1,2],[1,3],[1,4],[1,5],[1,6]
                                                  , [2,3],[2,4],[2,5],[2,6]
                                                  , [3,4],[3,5],[3,6] ]
                                              , td_FooBar )
                                           ]
            6 4 (1 (F - B) OR (B - F)) = Or [ ( Or [[5]],
                                              , td_F-B_B-F ) 
                                            ]
            td_F-B_B-F = Or [ Seq [()]
                            , Seq [()] 
                            ]

            6 4 (-1* Foo) OR (1C Bar) = Or [ ( Or [[1],[2],[3]]
                                             , (Trg Foo, Dif mempty) )
                                           , ( Or [[5]]
                                             , (Trg Bar, Complement) )
                                           ]
            6 4 (-1 NOT Foo)          = Or [ ( Or [[3]]
                                             , (Trg AllTags, Dif Foo )]

-}

tagset2TDs :: TagSet -- ^ TagSet to be transformed
           -> CGMonad (OrList TD) -- ^ OrList of TrgDifs with indices.
                                  -- The list is >1 if there is a Union in TagSet.
                                  -- This is in order to represent disjoint Diffs.
tagset2TDs tagset = case tagset of
  All       -> return $ Or [(AllTags, Dif mempty)]

  {- For example: List (Or [And [vblex, sg], And [n, pl]])
     This becomes one item in the OrList TD.
     Inside the TD, two IntSets in the Trg, and none in the Dif:
       ( Trg (Or [ IS(321,322,323) `intersect` IS(1,2,3,...,321,323) 
               , IS(2,3,50,450) `intersect` IS(4,5,6,50,1908) ])
       , Dif mempty )
  -}
  List orts -> do tagmap <- asks tagMap
                  let intsets = 
                       [ foldl1 intersection $ catMaybes [ M.lookup t tagmap  | t <- getAndList andts ]
                         | andts <- getOrList orts ]
                  return (Or [(Trg (Or intsets), Dif mempty)])

  {- For example: Union (List (Or [And [vblex])
                        (Diff (List (Or [And [vbser]) (List (Or [And [aux]]))
     This becomes two items in the OrList TD.
     Inside the first TD, there's one IntSet in Trg, none in Dif.
     Inside the second TD, one IntSet in Trg, one in Dif.
     In order to match this OrList of TDs, a cohort has to match either of the TDs.

  -}
  Union ts ts' -> do td  <- tagset2TDs ts
                     td' <- tagset2TDs ts'
                     return (td `mappend` td')

  Diff ts ts' -> undefined
  Cart ts ts' -> undefined

{-

Make this only once, at the beginning:
readingMap -- [1 |-> vblex sg p3
               2 |-> noun sg mf
               ...
            9023 |-> "aurrera" adv]

Make this only once, at the beginning:
tagMap    -- [vblex |-> IS(1,30,31,32,..,490)
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
-- things that need to be in Seq are e.g. indices in a pattern.
newtype Seq a = Seq { getSeq :: [a] } deriving (Eq,Ord,Functor,Foldable,Monoid)

instance (Show a) => Show (Seq a) where
  show = intercalate "->" . map show . getSeq