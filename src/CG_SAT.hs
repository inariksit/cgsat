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

type CgState = IntMap Lit

type Reading = [Tag]



type Sentence = IntMap Cohort
type Cohort = IntMap Lit


data Pattern = Pat { indices :: OrList (Seq Int)
                   , trgdifs :: OrList (Seq TD) } 
             | PatAlways deriving (Show,Eq,Ord)

type TD = (Trg,Dif)

data Trg = Trg { getTrg :: OrList IntSet } | AllTags deriving (Show,Eq,Ord)
data Dif = Dif { getDif :: AndList IntSet } | Complement deriving (Show,Eq,Ord)


--------------------------------------------------------------------------------

ctx2Pattern :: Int  -- ^ Sentence length
            -> Int  -- ^ Absolute index of the target in the sentence
            -> Context -- ^ Context to be transformed
            -> CGMonad (OrList Pattern) -- ^ List of patterns. Length >1 only if the context is template.
ctx2Pattern senlen trgind ctx = case ctx of
  Always -> return $ Or [PatAlways]
  Ctx posn polr tgst 
    -> do tds <- tagset2TDs tgst polr
          return undefined
  Link ctxs 
    -> undefined
  Template ctxs
    -> undefined
  Negate ctx -> undefined

 where 
  singleCtx2Pattern (Ctx posn polr tgst) = undefined

tagset2TDs :: TagSet -- ^ TagSet to be transformed
           -> Polarity -- ^ Polarity: whether the tags are
           -> CGMonad (OrList TD) -- ^ OrList of TrgDifs with indices.
                                  -- The list is >1 if there is a Union in TagSet.
                                  -- This is in order to represent disjoint Diffs.
tagset2TDs tagset polr = case tagset of
  All -> return $ Or [(AllTags, Dif mempty)]

  List l {- For example: List (Or [And [vblex, sg], And [n, pl]])
            This becomes one TD in the OrList.
            Inside the TD, two IntSets in the Trg, and none in the Dif:
            ( Trg (Or [ IS(321,322,323) `intersect` IS(1,2,3,...,321,323) 
                      , IS(2,3,50,450) `intersect` IS(4,5,6,50,1908) ])
            , Dif mempty )
          -}
    -> do tagmap <- asks tagMap
          let intsets = 
               [ foldl1 intersection $ 
                        catMaybes [ M.lookup t tagmap | t <- getAndList ts ]
                    | ts <- getOrList l ]
          --TODO: check if this works properly !!!
          {- (NOT 1 Foo) ==> Foo translates to (Trg (89,130,..), Dif mempty)
                             Not makes it into (AllTags, Dif (89,130,..)) -}
          let (trg,dif) = case polr of 
                            Yes -> (Trg (Or intsets), Dif mempty)
                            Not -> (AllTags,   Dif (And intsets))
          return $ Or [(trg,dif)]


  Union t t' {- For example: Union (List (Or [And [vblex])
                                   (Diff (List (Or [And [vbser]) 
                                         (List (Or [And [aux]]) )
                This becomes two TD in the OrList: 
                Or [(Trg vblex, Dif mempty), (Trg vbser, Dif aux)].
                To match this OrList of TDs, a cohort has to match either. -}
    -> do td  <- tagset2TDs t polr
          td' <- tagset2TDs t' polr
          -- can also normalise if both td's are single Ctx
          return (td `mappend` td')

  Diff ts ts' 
    -> undefined -- maybe something like below:
    -- (foo | bar) - (baz | quux) === normalises to tag lists
    -- ( Adv - (AdA - "very")) === Trg (lookup tagmap Adv), = some of these must be true 
    --                             Dif (lookup tagmap AdA `difference` 
    --                                  lookup tagmap "very") = all of these must be neg
    --                           Not putting "very" in explicitly, it's just given a pass and not negated.
    -- ((Adv - AdA) | ("very"))  -  (("ver.*"r) - ("verikoe"))
    --  === Or [ (Trg (lookup tagmap Adv), Dif (lookup tagmap AdA `difference` (lu tm ver.* `diff` lu tm "verikoe") ))
    --         , (Trg (lu tm "very"), Dif ... same as above) ]
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
-- things that need to be in Seq are e.g. indices in a pattern.
newtype Seq a = Seq { getSeq :: [a] } deriving (Eq,Ord,Functor,Foldable,Monoid)

instance (Show a) => Show (Seq a) where
  show = intercalate "->" . map show . getSeq