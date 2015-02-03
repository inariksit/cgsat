module CG where

import Control.Applicative
import Data.Boolean.SatSolver
import Data.List

-- | All kinds of morphological tags are in the same data type: e.g.  Prep, P1, Conditional.
-- | We don't specify e.g. which tags can be part of an analysis for which word classes.
-- | An analysis can contain an arbitrary amount of tags.
-- | Lemma is a special type of tag: Lem String
data Tag = 
   Art | Adj | Adv | Det | N | PN | V | V2 | VV 
 | Particle | Prep | Pron | Punct
 | CoordConj | SubordConj
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
data Position = Exactly Integer 
              | AtLeast Integer
              | Fwd Integer [Tag]  --Barrier rules
              | Bck Integer [Tag] deriving (Show,Eq,Read)





{-    
     OR
    /  \
   C1   AND
        / \
       C2  C3


[[Ca], [Cm, ..., Cn], [Co, ... , Cp]]

Everything inside the outer list is constructed by OR (or just C).
Everything inside an inner list is constructed by AND.
-}



toLists :: Condition -> [[Condition]]

--for OR, we make a list of lists, all in sequence.
--for AND, we need to make them parallel and put in a form with OR as the first constructor
--ie. AND (OR C1 C2) (C3) ---> OR (AND C1 C3) (AND C2 C3)
--just one level of nesting, should make this work for all inputs ... then again who writes millions of nested ands and ors
--for NOT, rather make some rules to get a normal form? e.g.
-- * NOT (OR a b)  = NOT a && NOT b
-- * NOT (AND a b) = NOT a || NOT b
toLists cond = case cond of
    (C position tags)               -> [[cond]]
    (OR  c1           c2)           -> toLists c1 ++ toLists c2 
    (NOT c1)                        -> toLists c1 --applyRule allows one NOT at top level
    (AND c1@(AND _ _) c2@(AND _ _)) -> [toListsAnd cond]
    (AND c1@(NOT _ )  c2         )  -> error "so complicated D:"
    (AND c1           c2@(NOT _) )  -> error "do you even know what you're doing ;__;"
    (AND c1           c2         )  -> map toListsAnd $ AND <$> simpleList c1 <*> simpleList c2  
  where toListsAnd (AND c1 c2) = concat (toLists c1 ++ toLists c2)
        toListsAnd c@(C _ _)   = [c]

        simpleList (AND c1 c2) = simpleList c1 ++ simpleList c2
        simpleList (OR c1 c2)  = simpleList c1 ++ simpleList c2
        simpleList (NOT c)     = simpleList c
        simpleList c           = [c]


-- Nicer way of writing conditions
mkC :: String -> [Tag] -> Condition
mkC str tags | last str == '*' = C (AtLeast $ (read . init) str) tags
             | otherwise       = C (Exactly $ read str)          tags

lemmaBear :: Condition
lemmaBear = mkC "0" [Lem "bear"]
always = mkC "0" []


-- Sets of tags
verb = [V,V2,VV]
noun = [N,PN]
det  = [Art,Det]
adv  = [Adv,Particle]
conj = [CoordConj,SubordConj]


-- Rules
rmParticle = Remove [Particle] always
slVerbAlways = Select verb always
slNounIfBear = Select noun lemmaBear
rmVerbIfDet = Remove verb (mkC "-1" det)
rmAdvIfDet = Remove adv (mkC "1" det)
rmNounIfPron = Remove noun (mkC "-1" [Pron])
slPrepIfDet = Select [Prep] (mkC "1" det)
andTest = Remove verb (AND (mkC "-2" (Adj:verb)) (mkC "-1" conj) )
notTest = Select verb (NOT (mkC "-1" [Prep]))
slNounAfterConj = Select noun ((mkC "-1" conj))
notOrTest = Select verb (NOT (OR (mkC "-1" conj) (mkC "1" [Prep])))
slCCifCC = Select [CoordConj] (C (Fwd 1 [Punct]) [CoordConj])






-- | Shows all analyses as string, each lemma+tags in one line
showAnalysis :: Analysis -> String
showAnalysis = concatMap showTags
  
showTags :: [Tag] -> String
showTags ((Lem l):as) = '\n':'\t':'"':l ++ '"':' ':analyses
  where analyses = unwords $ map show as



