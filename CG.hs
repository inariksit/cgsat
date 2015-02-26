module CG where

import Control.Applicative
import Data.Boolean.SatSolver
import Data.List

-- TODO:
-- * More complicated set operations for tag sets, not just conditions.
-- * CG2/CG3 decisions


-- | All kinds of morphological tags are in the same data type: e.g.  Prep, P1, Conditional.
-- | We don't specify e.g. which tags can be part of an analysis for which word classes.
-- | An analysis can contain an arbitrary amount of tags.
-- | Lemma is a special type of tag: Lem String
data Tag = Tag String | Lem String deriving (Eq,Read)


-- | Lemma should be first element in an analysis.
instance Ord Tag where
  Lem l `compare` Lem l' = l `compare` l'
  Lem l `compare` _      = LT
  _     `compare` Lem l  = GT
  Tag t `compare` Tag t' = t `compare` t'

instance Show Tag where
  show (Lem str) = "\"" ++ str ++ "\""
  show (Tag str) = "<" ++ str ++ ">"

-- | TagSet translates to [[Tag]] : outer list is bound by OR, inner lists by AND
--  For example, 
--    LIST DefArt = (det def) ;
--    LIST Dem    = "az" "ez" "amaz" "emez" ;
--
--  translate into
--    defArt = [[Tag "det", Tag "def"]]
--    dem    = [[Lem "az"],[Lem "ez"],[Lem "amaz"],[Lem "emez"]]
--  See the datatype for Test.
type TagSet = [[Tag]]

-- | Analysis is just list of tags: for instance the word form "alusta" would get
-- | [[Lem "alus", N, Sg, Part], [Lem "alustaa", V, Sg, Imperative]]
type Analysis = [[Tag]]

-- | Sentence is just a list of analyses: e.g. "the bear sleeps"
-- | [ [[Lem "the", Det]], 
-- |   [[Lem "bear", V, Pl],[Lem "bear", N, Sg]], 
-- |   [[Lem "sleep", V, Sg, P3], [Lem "sleep", N, Pl]]
-- | ]
type Sentence = [Analysis]


-- | Rule is either remove or select a list of tags, with condition(s).

data Rule = Remove TagSet Test | Select TagSet Test deriving (Show)


-- | Test is a condition with a possible NEG, to implement CG3's NEGATE
--   NEGATE negates the whole result, NOT just a single clause.
data Test = NEG Condition | POS Condition deriving (Show)

-- | There is no special constructor for empty condition (ie. remove/select tag everywhere),
--   but `C _ (_,[])' is assumed to mean that.
--   (Bool, [Tag]) emulates set negation NOT in CG3.
--   NOT foo === intersection with foo and the candidate is empty
data Condition = C Position (Bool, TagSet)
               | AND Condition Condition
               | OR Condition Condition deriving (Show)

-- | Position can be exact or at least.
-- | The meaning of numbers is 
-- | *  0: word itself
-- | * -n: to the left
-- | *  n: to the right.
data Position = Exactly Integer 
              | AtLeast Integer
              | Barrier Integer TagSet deriving (Show,Eq,Read)





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

toLists cond = case cond of
    (C _position      _tags)        -> [simpleList cond]
    (OR  c1           c2)           -> toLists c1 ++ toLists c2 
    (AND c1@(AND _ _) c2@(AND _ _)) -> [toListsAnd cond]
    (AND c1           c2)           -> map toListsAnd $ AND <$> simpleList c1 <*> simpleList c2  
  where toListsAnd (AND c1 c2) = concat (toLists c1 ++ toLists c2)
        toListsAnd c@(C _ _)   = [c]

        simpleList :: Condition -> [Condition]
        simpleList (AND c1 c2) = simpleList c1 ++ simpleList c2
        simpleList (OR c1 c2)  = simpleList c1 ++ simpleList c2
        simpleList c           = [c]


-- Shorthand for writing positive tests without barriers
-- TODO better parser
mkT :: String -> TagSet -> Test
mkT str tags = POS $ mkC str tags

mkC :: String -> TagSet -> Condition
mkC str tags | last str == '*' = C (AtLeast $ (read . init) str) (True, tags)
             | otherwise       = C (Exactly $ read str)          (True, tags)

neg :: Test -> Test
neg (POS t) = (NEG t)
neg (NEG t) = (POS t)

lemmaBear :: Condition
lemmaBear = mkC "0" [[Lem "bear"]]
always = mkC "0" []
andTest = AND lemmaBear always


-- Sets of tags
verb = (map . map) Tag [["vblex"],["vbser"],["vbmod"]]
noun = (map . map) Tag [["n", "np"]]
det  = [[Tag "det"]]
adv  = [[Tag "adv"]]
conj = (map . map) Tag [["cnjcoo"],["cnjsub"]]
prep = [[Tag "prep"]]
sg   = [[Tag "sg"]]
pl   = [[Tag "pl"]]
cnjcoo  = [[Tag "cnjcoo"]]

-- Rules
rmParticle = Remove [[Tag "particle"]] (POS always)
slVerbAlways = Select verb (POS always)
slNounIfBear = Select noun (POS lemmaBear)

rmVerbIfDet = Remove verb (mkT "-1" det)
rmAdvIfDet = Remove adv (mkT "1" det)
rmNounIfPron = Remove noun (mkT "-1" [[Tag "pron"]])
slPrepIfDet = Select prep (mkT "1" det)
slNounAfterConj = Select noun (mkT "-1" conj)

slCCifCC = Select cnjcoo (POS (C (Barrier 1 [[Tag "punct"]]) (True,cnjcoo)))

rmPlIfSg = Remove pl (POS (C (Exactly (-1)) (True,sg)))
rmSgIfPl = Remove sg (mkT "-1" pl)

negTest   = Select verb (neg (mkT "-1" prep))
negOrTest = Select verb (NEG (OR (mkC "-1" conj) (mkC "1" prep)))






-- | Shows all analyses as string, each lemma+tags in one line
showAnalysis :: Analysis -> String
showAnalysis = concatMap showTags
  
showTags :: [Tag] -> String
showTags (l:as) = show l ++ '\n':'\t':analyses
  where analyses = unwords $ map show as



