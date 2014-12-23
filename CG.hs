module CG where

import Data.Boolean.SatSolver
import Data.List
import Control.Applicative
import Control.Monad

-- | All kinds of morphological tags are in the same data type: e.g.  Prep, P1, Conditional.
-- | We don't specify e.g. which tags can be part of an analysis for which word classes.
-- | An analysis can contain an arbitrary amount of tags.
-- | Lemma is a special type of tag: Lem String
data Tag = 
   Art | Adj | Adv | Det | N | PN | V | V2 | VV | Sg | Particle 
 | Pl | Prep | P1 | P2 | P3 | Subj | Imper | Cond | Inf 
 | Lem String deriving (Show,Read,Eq)

-- | Analysis is just a list of tags: for instance the word form "alusta" would get
-- | [Lem "alus", N, Sg, Part], [Lem "alustaa", V, Sg, Imperative]
type Analysis = [[Tag]]

-- | Sentence is just a list of analyses: e.g. "the bear sleeps"
-- | [ [[Lem "the", Det]], 
-- |   [[Lem "bear", V, Pl],[Lem "bear, N, Sg]], 
-- |   [[Lem "sleep", V, Sg, P3], [Lem "sleep", N, Pl]]
-- | ]
type Sentence = [Analysis]

data Position = P {num :: Int, scan :: Direction} deriving (Show)

data Direction = SLeft | SRight deriving (Show)

-- REMOVE Fin IF (-1* Fin) (1* Fin)
-- means: remove Fin if there is a Fin from position -1 anywhere left, and there is a Fin from position 1 anywhere to the right.

-- IF ((-1* (fin)) OR (1* (fin))) ;
data Condition = C Position [Tag] deriving (Show)

data Rule = Select [Tag] [Condition] | Remove [Tag] [Condition] deriving (Show)



-- Sets of tags
verb = [V,V2,VV]
noun = [N,PN]
det  = [Art,Det]
adv  = [Adv,Particle]

-- Rules
rmVerbIfBear = Remove verb [C (P 0 SRight) [Lem "bear"]]

-- Analyses
the = [[Lem "the", Det]]
bear = [Lem "bear", N,Sg] : [[Lem "bear", V,y,z,w] | y <- [Sg,Pl], z <- [P1,P2], w <- [Subj,Imper,Cond]]-- and so on
sleeps = [[Lem "sleep", N,Pl],
          [Lem "sleep", V,Sg,P3]]
in_ = [[Lem "in", Prep],
       [Lem "in", Adv]]

-- Sentence
ex0 = [ the --unambiguous
      , bear
      , sleeps ]

-- | Shows all analyses as string, each lemma+tags in one line
showAnalysis :: Analysis -> String
showAnalysis = concatMap showTags
  
showTags :: [Tag] -> String
showTags ((Lem l):as) = "\n      " ++ '"':l ++ '"':' ':analyses
  where analyses = unwords $ map show as


--(PhrUtt NoPConj (UttS (UseCl (TTAnt (TPres) (ASimul)) PPos (PredVP (DetCN (DetQuant DefArt NumSg) (UseN baby_N)) (UseV sleep_V)))) NoVoc)




