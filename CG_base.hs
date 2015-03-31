module CG_base where

import Control.Applicative
import Data.List

-- | All kinds of morphological tags are in the same data type: e.g.  Prep, P1, Conditional.
-- | We don't specify e.g. which tags can be part of an analysis for which word classes.
-- | An analysis can contain an arbitrary amount of tags.
-- | Lemma and word form are also in tags.
data Tag = Tag String | Lem String | WF String | EOS | BOS deriving (Eq,Read)


-- | Wordform should be first element in an analysis.
instance Ord Tag where
  WF l `compare` WF l' = l `compare` l'
  WF l `compare` _      = LT
  _     `compare` WF l  = GT
  Lem l `compare` Lem l' = l `compare` l'
  Tag t `compare` Tag t' = t `compare` t'
  foo   `compare` bar    = show foo `compare` show bar

-- | Following the conventions of vislcg3
instance Show Tag where
  show (WF str) = "\"<" ++ str ++ ">\""
  show (Lem str) = "\"" ++ str ++ "\""
  show (Tag str) = str
  show BOS       = "" --">>>"
  show EOS       = "" -- "<<<"

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
-- | [[WF "alusta", Lem "alus", N, Sg, Part], [WF "alusta", Lem "alustaa", V, Sg, Imperative]]
type Analysis = [[Tag]]

-- | Sentence is just a list of analyses
type Sentence = [Analysis]


-- | Rule is either remove or select a list of tags, with contextual tests
data Rule = Remove Name TagSet Condition | Select Name TagSet Condition
type Name = String

instance Show Rule where
  show (Remove name tags cond) = "REMOVE:" ++ name ++ " " ++
                                  show tags ++ " " ++ show cond 
  show (Select name tags cond) = "SELECT:" ++ name ++ " " ++
                                  show tags ++ " " ++ show cond 
  

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

--  (NOT -2 Verb) (NOT -2 Prep) (-1C Pro) (0 N OR Verb) 
-- -> OR (AND (NOT -2 Verb) (NOT -2 Prep) (-1C Pro) (0 N))
--       (AND (NOT -2 Verb) (NOT -2 Prep) (-1C Pro) (0 Verb))

toLists cond = case cond of
    C   _pos       _tags -> [[cond]]
    AND c1@(C _ _) c2    -> map (c1:) (toLists c2)
    OR  c1@(C _ _) c2    -> [c1]:(toLists c2)
    AND c2  c1@(C _ _)   -> map (c1:) (toLists c2)
    OR  c2  c1@(C _ _)   -> [c1]:(toLists c2)
    AND (AND c1 c2) (OR  c3 c4) -> toLists $ OR (AND c1 (AND c2 c3))
                                                (AND c1 (AND c2 c4))
    AND (OR  c3 c4) (AND c1 c2) -> toLists $ OR (AND c1 (AND c2 c3))
                                                (AND c1 (AND c2 c4))
    AND (OR  c1 c2) (OR  c3 c4) -> toLists $ OR (OR (AND c1 c3)
                                                    (AND c1 c4))
                                                (OR (AND c2 c3)
                                                    (AND c2 c4))
    AND c1@(AND _ _) c2@(AND _ _) -> [concat $ toLists c1 ++ toLists c2]
    OR  c1 c2 -> toLists c1 ++ toLists c2 
  where toListsAnd (AND c1 c2) = concat (toLists c1 ++ toLists c2)
        toListsAnd c@(C _ _)   = [c]

        simpleList :: Condition -> [Condition]
        simpleList (AND c1 c2) = simpleList c1 ++ simpleList c2
        simpleList (OR c1 c2)  = simpleList c1 ++ simpleList c2
        simpleList c           = [c]


-- Shorthand for writing conditions without barriers
mkC :: String -> TagSet -> Condition
mkC str tags | last str == '*' = C (AtLeast $ (read . init) str) (True, tags)
             | otherwise       = C (Exactly $ read str)          (True, tags)

lemmaBear :: Condition
lemmaBear = mkC "0" [[Lem "bear"]]
always = mkC "0" []
andTest = AND lemmaBear always

hasBoundary :: Rule -> Bool
hasBoundary rule = case rule of
  (Select _n _t c) -> findBoundary c
  (Remove _n _t c) -> findBoundary c
  where findBoundary c = or $ map hasB (concat (toLists c))
        hasB (C _pos (_b,tags)) = (not.null) $ [BOS,EOS] `intersect` concat tags

-- Sets of tags
verb = (map . map) Tag [["vblex"],["vbser"],["vbmod"]]
noun = (map . map) Tag [["n"], ["np"]]
det  = [[Tag "det"]]
adv  = [[Tag "adv"]]
conj = (map . map) Tag [["cnjcoo"],["cnjsub"]]
prep = [[Tag "prep"]]
sg   = [[Tag "sg"]]
pl   = [[Tag "pl"]]
cnjcoo  = [[Tag "cnjcoo"]]

-- Rules
rmParticle = Remove "r_part" [[Tag "particle"]] always
slVerbAlways = Select "s_verb" verb  always
slNounIfBear = Select "s_bear_n" [[Lem "bear", n] | n <- concat noun]  always

rmVerbIfDet = Remove "r_verb_det" verb (mkC "-1" det)
rmAdvIfDet = Remove "r_adv_det" adv (mkC "1" det)
rmNounIfPron = Remove "" noun (mkC "-1" [[Tag "pron"]])
slPrepIfDet = Select "" prep (mkC "1" det)
slNounAfterConj = Select "" noun (mkC "-1" conj)

slCCifCC = Select "" cnjcoo (C (Barrier 1 [[Tag "punct"]]) (True,cnjcoo))

rmPlIfSg = Remove "" pl (C (Exactly (-1)) (True,sg))
rmSgIfPl = Remove "" sg (mkC "-1" pl)

negTest   = Select "" verb (mkC "-1" prep)
negOrTest = Select "" verb (OR (mkC "-1" conj) (mkC "1" prep))


-- | Shows all analyses as string, each lemma+tags in one line
showSentence :: Sentence -> String
showSentence = concatMap showAnalysis

-- concatMap showTags returns this:
-- "<are>"
--	"be" vblex pres"<be>"
--	"are" n sg
-- so need some trickery
showAnalysis :: Analysis -> String
showAnalysis []     = []
showAnalysis (a:as) = unlines $ showTags a : map showTags as'
  where as' = (map.filter) notWF as
        notWF (WF _) = False
        notWF _      = True        
  
showTags :: [Tag] -> String
showTags ts@(wf:as) = 
  case wf of
    (WF s) -> show wf ++ '\n':'\t':showA as
    _      -> '\t':showA (wf:as)
  where showA = unwords . map show



