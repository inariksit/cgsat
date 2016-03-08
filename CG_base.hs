module CG_base where

import Control.Applicative
import Data.List
import Debug.Trace
import Text.Regex

-- | All kinds of morphological tags are in the same data type: e.g.  Prep, P1, Conditional.
-- | We don't specify e.g. which tags can be part of an analysis for which word classes.
-- | An analysis can contain an arbitrary amount of tags.
-- | Lemma and word form are also in tags.
data Tag = Tag String | Lem String | WF String | Subreading Subpos Tag |
           {-Rgx Regex String | -} EOS | BOS 

data Subpos = FromStart Integer | FromEnd Integer | Wherever

instance Show Subpos where
  show (FromStart n) = show n
  show (FromEnd   n) = show (-n)
  show Wherever = "*"

instance Eq Subpos where
  Wherever    == anywhere    = True
  anywhere    == Wherever    = True
  FromStart n == FromStart m = n==m
  FromEnd   n == FromEnd   m = n==m
  _           == _           = False


--because Regex has no Eq instance
instance Eq Tag where
  WF  str == WF  str' = str == str'
  Lem str == Lem str' = str == str'
  Tag str == Tag str' = str == str'
  EOS     == EOS      = True
  BOS     == BOS      = True
  Subreading s' t' 
   == Subreading s t  = s==s' && t==t'
  _       == _        = False       

-- | Wordform should be first element in an analysis.
instance Ord Tag where
  BOS `compare` _   = GT
  _   `compare` BOS = LT
  EOS `compare` _   = GT
  _   `compare` EOS = LT 
  WF word `compare` WF word'  = word `compare` word'
  WF _    `compare` _         = LT
  _       `compare` WF _      = GT
  Lem lem `compare` Lem lem'  = lem `compare` lem'
  Lem lem `compare` Tag tag   = GT
  Tag tag `compare` Lem lem   = LT

  Tag tag `compare` Tag tag'  = tag `compare` tag'
--  Rgx _ s `compare` Rgx _ s' = s `compare` s'
  Subreading _ t `compare` 
       Subreading _ t'        = t `compare` t'
  Subreading _ t `compare` t' = LT
  t `compare` Subreading _ t' = GT

-- | Following the conventions of vislcg3
instance Show Tag where
  show (WF str) = "\"<" ++ str ++ ">\""
  show (Lem str) = "\"" ++ str ++ "\""
--  show (Rgx _r s) =  "\"" ++ s ++ "\"r"
  show (Tag str) = str
  show (Subreading n tag) = show n ++ "+" ++ show tag
  show BOS       = ">>>"
  show EOS       = "<<<"



data TagSet =
   TS [[Tag]]
 | Or TagSet TagSet
 | Diff TagSet TagSet
 | Cart TagSet TagSet
 | All
  deriving (Eq,Ord)

instance Show TagSet where
  show (TS tags) = showTagset tags
  show (Or ts1 ts2) = show ts1 ++ "|" ++ show ts2
  show (Diff ts1 ts2) = show ts1 ++ " - " ++ show ts2 
--  show (Cart ts1 ts2) = show $ map concat $ sequence [(toTags' ts1), (toTags' ts2)] 
  show (Cart ts1 ts2) = show ts1 ++ " + " ++ show ts2
  show All = "(*)"

showTagset :: [[Tag]] -> String
showTagset [[x]] = show x 
showTagset xs    = intercalate "|" $ map show' xs
  where show' [y] = show y 
        show' ys  = "(" ++ unwords (map show ys) ++ ")"


type Trg = [[Tag]]
type Dif = [[Tag]]
toTags :: TagSet -> [ (Trg,Dif) ]
-- | TagSet translates to pair of [[Tag]]s : outer list is bound by OR, inner lists by AND
--  First [[Tag]] is the target, second [[Tag]] is list of tags it should not match.
--  For example, 
--    LIST DefArt = (det def) ;
--    LIST Dem    = "az" "ez" "amaz" "emez" ;
--    SET  FooBar = foo bar - baz ;
--    SET  Complex = FooBar OR (hargle - bargle) ;
--  translate into
--    defArt = ([[Tag "det", Tag "def"]], [[]]) : []
--    dem    = ([[Lem "az"],[Lem "ez"],[Lem "amaz"],[Lem "emez"]], [[]]) : []
--    fooBar = ([[Tag "foo"],[Tag "bar"]], [[Tag "baz"]]) : []
--    complex = [ ([[Tag "foo"],[Tag "bar"]], [[Tag "baz"]])
--              , ([[Tag "hargle"]], [[Tag "bargle"]] )
--              ]  
-- it's a list of pairs is because of disjoint Diffs. 
-- A list can be a set in CG, so we just coerce list to set automatically
toTags ts = case ts of
  Or   ts1 ts2 -> toTags ts1 ++ toTags ts2
  Diff ts1 ts2 -> [(toTags' ts, toTags' ts2)]
  _            -> [(toTags' ts, [[]])]
  --where
  --  toTags' (TS tags) = tags
  --  toTags' (Or ts1 ts2) = toTags' ts1 ++ toTags' ts2
  --  toTags' (Diff ts1 ts2) = toTags' ts1 \\ toTags' ts2 
  --  toTags' (Cart ts1 ts2) = map concat $ sequence [(toTags' ts1), (toTags' ts2)]
  --  toTags' All = [[]] --matches all: from CG_SAT.tagsMatchRule
                       --"At least one complete sublist in the rule must be found in the analysis"
toTags' (TS tags) = tags
toTags' (Or ts1 ts2) = toTags' ts1 ++ toTags' ts2
toTags' (Diff ts1 ts2) = toTags' ts1 \\ toTags' ts2 
toTags' (Cart ts1 ts2) = map concat $ sequence [(toTags' ts1), (toTags' ts2)]
toTags' All = [[]] --matches all: from CG_SAT.tagsMatchRule

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

isSelect :: Rule -> Bool 
isSelect (Select _ _ _ ) = True
isSelect _               = False

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
  show (OR c1 c2) = "( " ++ show c1 ++ " ORc " ++ show c2 ++ " )"
  show Always = "Always"

-- | Position can be exact or at least.
-- The meaning of numbers is 
--   *  0: word itself
--   * -n: to the left
--   *  n: to the right.
-- Bool is for cautious mode.

getTagset :: Condition -> TagSet
getTagset (C _pos (_, tagset)) = tagset
getTagset Always = TS [[]]
getTagSet cond = error "getTagset: applied to complex condition " ++ show cond

type Cautious = Bool

data Position = Exactly Cautious Int 
              | AtLeast Cautious Int
              | Barrier Cautious Int TagSet 
              | CBarrier Cautious Int TagSet 
              | LINK {parent::Position , self::Position} deriving (Eq)


instance Show Position where
  show (Exactly True i) = show i ++ "C"
  show (AtLeast True i) = "*" ++ show i ++ "C"
  show (Exactly False i) = show i
  show (AtLeast False i) = "*" ++ show i
  show (LINK pos1 linkedpos) = show pos1 ++ " LINK " ++ show linkedpos
  show p                 = let (a,b) = showPosTuple p in a++b

showPosTuple :: Position -> (String, String)
showPosTuple (Barrier  True i ts) = ("*" ++ show i ++ "C ", " BARRIER " ++ show ts)
showPosTuple (CBarrier True i ts) = ("*" ++ show i ++ "C ", " CBARRIER " ++ show ts)
showPosTuple (Barrier  False i ts) = ("*" ++ show i ++ " ", " BARRIER " ++ show ts)
showPosTuple (CBarrier False i ts) = ("*" ++ show i ++ " ", " CBARRIER " ++ show ts)
showPosTuple p = (show p, "")

  



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

toCond :: [[Condition]] -> Condition
toCond condss = foldl1 OR [ foldl1 AND conds | conds <- condss ]

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
        hasB (C _pos (_b,tags)) = (not.null) $ [BOS,EOS] `intersect` concat ((concatMap fst . toTags) tags)

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



