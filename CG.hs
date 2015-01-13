module CG where

import Data.Boolean.SatSolver
import Data.List

-- | All kinds of morphological tags are in the same data type: e.g.  Prep, P1, Conditional.
-- | We don't specify e.g. which tags can be part of an analysis for which word classes.
-- | An analysis can contain an arbitrary amount of tags.
-- | Lemma is a special type of tag: Lem String
data Tag = 
   Art | Adj | Adv | Det | N | PN | V | V2 | VV 
 | Particle | Prep | CoordConj | Pron
 | Sg | Pl | P1 | P2 | P3 
 | Subj | Imper | Cond | Inf | Pres
 | Nom | Acc | Dat
 | Lem String deriving (Eq,Show,Read)

-- | Lemma should be first element in an analysis.
instance Ord Tag where
  Lem l `compare` Lem l' = l `compare` l'
  Lem l `compare` _      = LT
  _     `compare` Lem l  = GT
  tag   `compare` tag'   = show tag `compare` show tag'


-- | Analysis is just a list of tags: for instance the word form "alusta" would get
-- | [Lem "alus", N, Sg, Part], [Lem "alustaa", V, Sg, Imperative]
type Analysis = [[Tag]]

-- | Sentence is just a list of analyses: e.g. "the bear sleeps"
-- | [ [[Lem "the", Det]], 
-- |   [[Lem "bear", V, Pl],[Lem "bear", N, Sg]], 
-- |   [[Lem "sleep", V, Sg, P3], [Lem "sleep", N, Pl]]
-- | ]
type Sentence = [Analysis]


-- | Rule is either remove or select a list of tags, with condition(s).
-- | See the datatype for Condition.
data Rule = Remove [Tag] Condition | Select [Tag] Condition deriving (Show)

-- | There is no special constructor for empty condition (ie. remove/select tag everywhere),
-- | but `C _ []' is assumed to mean that.
data Condition = C Position [Tag]
               | NOT Condition
               | AND Condition Condition
               | OR Condition Condition deriving (Show)

-- | Position can be exact or at least.
-- | The meaning of numbers is 
-- | *  0: word itself
-- | * -n: to the left
-- | *  n: to the right.
data Position = Exactly Integer | AtLeast Integer deriving (Show,Eq,Read)



-- We want to have conditions in a list later on
toCList :: Condition -> [Condition]
toCList (AND c1 c2) = toCList c1 ++ toCList c2
toCList (OR c1 c2)  = toCList c1 ++ toCList c2
toCList (NOT c)     = toCList c
toCList c           = [c]

-- and a nice way of writing them
mkC :: String -> [Tag] -> Condition
mkC str tags | last str == '*' = C (AtLeast $ (read . init) str) tags
             | otherwise       = C (Exactly $ read str)          tags

lemmaBear :: Condition
lemmaBear = mkC "0" [Lem "bear"]


-- Sets of tags
verb = [V,V2,VV]
noun = [N,PN]
det  = [Art,Det]
adv  = [Adv,Particle]
conj = [CoordConj]


-- Rules
rmParticle = Remove [Particle] (mkC "0" [])
rmVerbIfBear = Remove verb lemmaBear
slNounIfBear = Select noun lemmaBear
rmVerbIfDet = Remove verb (mkC "-1" det)
rmAdvIfDet = Remove adv (mkC "1*" det)
slPrepIfDet = Select [Prep] (mkC "1" det)
andTest = Select verb (AND (mkC "-1" conj) (mkC "1" [Prep]))



-- | Shows all analyses as string, each lemma+tags in one line
showAnalysis :: Analysis -> String
showAnalysis = concatMap showTags
  
showTags :: [Tag] -> String
showTags ((Lem l):as) = '\n':'\t':'"':l ++ '"':' ':analyses
  where analyses = unwords $ map show as



