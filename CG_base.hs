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
  WF _ `compare` _      = LT
  _     `compare` WF _  = GT
  Lem l `compare` Lem l' = l `compare` l'
  Tag t `compare` Tag t' = t `compare` t'
  Lem _ `compare` Tag _  = LT
  Tag _ `compare` Lem _  = GT
  foo   `compare` bar    = show foo `compare` show bar

-- | Following the conventions of vislcg3
instance Show Tag where
  show (WF str) = "\"<" ++ str ++ ">\""
  show (Lem str) = "\"" ++ str ++ "\""
  show (Tag str) = str
  show BOS       = ">>>"
  show EOS       = "<<<"



data TagSet =
   TS [[Tag]]
 | Or TagSet TagSet
 | Diff TagSet TagSet
 | Cart TagSet TagSet
 | All
  deriving (Eq,Ord,Read)

instance Show TagSet where
  show (TS tags) = showTagset tags
  show (Or ts1 ts2) = show ts1 ++ " OR " ++ show ts2
  show (Diff ts1 ts2) = show ts1 ++ " - " ++ show ts2 
  show (Cart ts1 ts2) = show ts1 ++ " + " ++ show ts2
  show All = "(*)"

showTagset :: [[Tag]] -> String
--showTagset [[x]] = show x ++ " "
showTagset xs    = concatMap show' xs
  where show' [y] = show y ++ " "
        show' ys  = "(" ++ unwords (map show ys) ++ ")"

toTags :: TagSet -> [[Tag]]
-- | TagSet translates to [[Tag]] : outer list is bound by OR, inner lists by AND
--  For example, 
--    LIST DefArt = (det def) ;
--    LIST Dem    = "az" "ez" "amaz" "emez" ;
--
--  translate into
--    defArt = [[Tag "det", Tag "def"]]
--    dem    = [[Lem "az"],[Lem "ez"],[Lem "amaz"],[Lem "emez"]]

toTags (TS tags) = tags
toTags (Or ts1 ts2) = toTags ts1 ++ toTags ts2
toTags (Diff ts1 ts2) = toTags ts1 \\ toTags ts2
toTags (Cart ts1 ts2) = map concat $ sequence [(toTags ts1), (toTags ts2)]
toTags All = [[]] --matches all

-- | Analysis is just list of tags: for instance the word form "alusta" would get
-- | [[WF "alusta", Lem "alus", N, Sg, Part], [WF "alusta", Lem "alustaa", V, Sg, Imperative]]
type Analysis = [[Tag]]

-- | Sentence is just a list of analyses
type Sentence = [Analysis]


-- | Rule is either remove or select a list of tags, with contextual tests
data Rule = Remove {name :: Name, target :: TagSet, cond :: Condition} |
            Select {name :: Name, target :: TagSet, cond :: Condition} deriving (Eq) 
data Name = Name String | NoName deriving (Eq)

instance Show Rule where
  show (Remove (Name nm) tags Always) = "REMOVE:" ++ nm ++ " " ++ show tags
  show (Remove  NoName   tags Always) = "REMOVE " ++ show tags
  show (Remove (Name nm) tags cond) = "REMOVE:" ++ nm ++ " " ++
                                       show tags ++ " IF " ++ show cond 
  show (Remove  NoName   tags cond) = "REMOVE " ++ show tags ++ " IF " ++ show cond
  show (Select (Name nm) tags Always) = "SELECT:" ++ nm ++ " " ++ show tags
  show (Select  NoName   tags Always) = "SELECT " ++ show tags
  show (Select (Name nm) tags cond) = "SELECT:" ++ nm ++ " " ++
                                       show tags ++ " IF " ++ show cond 
  show (Select  NoName   tags cond) = "SELECT " ++ show tags ++ " IF " ++ show cond 

-- | There is no special constructor for empty condition (ie. remove/select tag everywhere),
--   but `C _ (_,[])' is assumed to mean that.
--   (Bool, TagSet) emulates set negation NOT in CG3.
--   NOT foo === intersection with foo and the candidate is empty
data Condition = C Position (Bool, TagSet)
               | Always
               | AND Condition Condition 
               | OR Condition Condition  deriving (Eq)

instance Show Condition where
  show (C pos (True, ts)) = "(" ++ fst (showPosTuple pos) ++ " " ++ show ts ++
                                   snd (showPosTuple pos) ++ ")"
  show (C pos (False, ts)) = "(NOT " ++ fst (showPosTuple pos) ++ " " ++ show ts
                                     ++ snd (showPosTuple pos) ++ ")"
  show (AND c1 c2) = show c1 ++ " " ++ show c2
  show (OR c1 c2) = show c1 ++ " ORc " ++ show c2
  show Always = ""

-- | Position can be exact or at least.
-- The meaning of numbers is 
--   *  0: word itself
--   * -n: to the left
--   *  n: to the right.
-- Bool is for cautious mode.

getTagset :: Condition -> TagSet
getTagset (C _pos (_, tagset)) = tagset
getTagset Always = TS [[]]

type Cautious = Bool

data Position = Exactly Cautious Int 
              | AtLeast Cautious Int
              | Barrier Cautious Int TagSet 
              | CBarrier Cautious Int TagSet deriving (Eq,Read)


instance Show Position where
  show (Exactly True i) = show i ++ "C"
  show (AtLeast True i) = "*" ++ show i ++ "C"
  show (Exactly False i) = show i
  show (AtLeast False i) = "*" ++ show i
  show p                 = let (a,b) = showPosTuple p in a++b

showPosTuple :: Position -> (String, String)
showPosTuple p@(Exactly _ _) = (show p, "")
showPosTuple p@(AtLeast _ _) = (show p, "")
showPosTuple (Barrier  True i ts) = ("*" ++ show i ++ "C ", " BARRIER " ++ show ts)
showPosTuple (CBarrier True i ts) = ("*" ++ show i ++ "C ", " CBARRIER " ++ show ts)
showPosTuple (Barrier  False i ts) = ("*" ++ show i ++ " ", " BARRIER " ++ show ts)
showPosTuple (CBarrier False i ts) = ("*" ++ show i ++ " ", " CBARRIER " ++ show ts)
  



toConds :: Condition -> [[Condition]]
{-    
     OR
    /  \
   C1   AND
        / \
       C2  C3


[[Ca], [Cm, ..., Cn], [Co, ... , Cp]]

Everything inside the outer list is constructed by OR (or just C).
Everything inside an inner list is constructed by AND.

for OR, we make a list of lists, all in sequence.
for AND, we need to make them parallel and put in a form with OR as the first constructor
ie. AND (OR C1 C2) (C3) ---> OR (AND C1 C3) (AND C2 C3)
-}

toConds cond = case cond of
    C   _pos       _tags -> [[cond]]
    Always               -> [[Always]]
    AND Always c2        -> [[Always]]
    AND c1 Always        -> [[Always]]
    OR Always c2         -> toConds c2
    OR c1 Always         -> toConds c1
    AND c1@(C _ _) c2    -> map (c1:) (toConds c2)
    OR  c1@(C _ _) c2    -> [c1]:(toConds c2)
    AND c2  c1@(C _ _)   -> map (c1:) (toConds c2)
    OR  c2  c1@(C _ _)   -> [c1]:(toConds c2)
    AND (AND c1 c2) (OR  c3 c4) -> toConds $ OR (AND c1 (AND c2 c3))
                                                (AND c1 (AND c2 c4))
    AND (OR  c3 c4) (AND c1 c2) -> toConds $ OR (AND c1 (AND c2 c3))
                                                (AND c1 (AND c2 c4))
    AND (OR  c1 c2) (OR  c3 c4) -> toConds $ OR (OR (AND c1 c3)
                                                    (AND c1 c4))
                                                (OR (AND c2 c3)
                                                    (AND c2 c4))
    AND c1@(AND _ _) c2@(AND _ _) -> [concat $ toConds c1 ++ toConds c2]
    OR  c1 c2 -> toConds c1 ++ toConds c2 


-- Shorthand for writing conditions without barriers
mkC :: String -> TagSet -> Condition
mkC s tags | last s == '*' = C (AtLeast False $ (read . init) s) (True, tags)
           | otherwise     = C (Exactly False $ read s)          (True, tags)

lemmaBear :: Condition
lemmaBear = mkC "0" (TS [[Lem "bear"]])
always = mkC "0" All

hasBoundary :: Rule -> Bool
hasBoundary rule = case rule of
  (Select _n _t c) -> findBoundary c
  (Remove _n _t c) -> findBoundary c
  where findBoundary c = any hasB (concat (toConds c))
        hasB (C _pos (_b,tags)) = (not.null) $ [BOS,EOS] `intersect` concat (toTags tags)

-- Sets of tags
verb = TS [[Tag "vblex"],[Tag "vbser"],[Tag "vbmod"]]
noun = TS [[Tag "n"], [Tag "np"]]
det  = TS [[Tag "det"]]
adv  = TS [[Tag "adv"]]
conj = TS [[Tag "cnjcoo"],[Tag "cnjsub"]]
prep = TS [[Tag "prep"]]
sg   = TS [[Tag "sg"]]
pl   = TS [[Tag "pl"]]
cnjcoo  = TS [[Tag "cnjcoo"]]

-- Rules
rmParticle = Remove NoName (TS [[Tag "particle"]]) always
slVerbAlways = Select NoName verb  always
slNounIfBear = Select NoName (TS [[Lem "bear", Tag "n"]]) always

rmVerbIfDet = Remove NoName verb (mkC "-1" det)
rmAdvIfDet = Remove NoName adv (mkC "1" det)
rmNounIfPron = Remove NoName noun (mkC "-1" (TS [[Tag "pron"]]))
slPrepIfDet = Select NoName prep (mkC "1" det)
slNounAfterConj = Select NoName noun (mkC "-1" conj)

slCCifCC = Select NoName cnjcoo (C (Barrier False 1 (TS [[Tag "punct"]])) (True,cnjcoo))

rmPlIfSg = Remove NoName pl (C (Exactly False (-1)) (True,sg))
rmSgIfPl = Remove NoName sg (mkC "-1" pl)

negTest   = Select NoName verb (mkC "-1" prep)
negOrTest = Select NoName verb (OR (mkC "-1" conj) (mkC "1" prep))


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
showTags []         = "[]"
showTags ts@(wf:as) = 
  case wf of
    (WF s) -> show wf ++ '\n':'\t':showA as
    _      -> '\t':showA (wf:as)
  where showA = unwords . map show



