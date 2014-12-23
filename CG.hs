module CG where

import Data.Boolean.SatSolver
import Data.List
import Control.Applicative
import Control.Monad

{-
Version 0.1: 
 - only tags matter, not lemmas
 - only one tag per analysis (for no other reason than my laziness when doing manual examples)
 - only 1 token before or after
-}

data Tag = Art | Adj | Adv | Det | N | PN | V | V2 | VV | Sg | Particle | Pl | Prep | P1 | P2 | P3 | Subj | Imper | Cond | Inf deriving (Show,Read,Eq)

data Analysis = A {s :: String, t :: [Tag]} deriving (Show,Read,Eq)

data Position = P Int Scan

data Scan = ScanLeft | ScanRight 

--type Sentence = [[Analysis]] --doesn't apply for now

-- REMOVE Fin IF (-1* Fin) (1* Fin)
-- means: remove Fin if there is a Fin from position -1 anywhere left, and there is a Fin from position 1 anywhere to the right.

data Context = C {prev :: [Tag], next :: [Tag]} deriving (Show)

-- IF ((-1* (fin)) OR (1* (fin))) ;
data Condition = C_ Position [Tag]

data Rule = Select [Tag] Context | Remove [Tag] Context deriving (Show)



-- Sets of tags
verb = [V,V2,VV]
noun = [N,PN]
det  = [Art,Det]
adv  = [Adv,Particle]

-- Rules
rmVerb = Remove verb (C det [])
slNoun = Select noun (C [] verb)
rmAdv = Remove adv (C [] det)

-- Analyses
the = A "the" [Det]
bear = A "bear" [N,Sg] : [A "bear" [V,y,z] | y <- [Sg,Pl], z <- [P1,P2]] -- w <- [Subj,Imper,Cond] and so on
sleeps = [A "sleep" [N,Pl],
          A "sleep" [V,Sg,P3]]
in_ = [A "in" [Prep],
       A "in" [Adv]]

-- Sentence
ex0 = [ [A "the" [Det]] --unambiguous
      , bear
      , sleeps ]

--(PhrUtt NoPConj (UttS (UseCl (TTAnt (TPres) (ASimul)) PPos (PredVP (DetCN (DetQuant DefArt NumSg) (UseN baby_N)) (UseV sleep_V)))) NoVoc)




