import CG_base hiding ( Sentence )
import CG_parse
import CG_SAT
import SAT ( Solver(..), newSolver )
import SAT.Named

import Control.Monad
import Control.Monad.State.Lazy
import Data.List hiding ( lookup )
import Data.Map ( Map(..), fromList, toAscList, member, adjust, lookup )
import Data.Maybe
import Prelude hiding ( lookup )
import System.Environment

ex_abc1 = concat $ snd $ parseRules False
     ( "REMOVE:r1 (a) IF (-1 (b)) ; " ++
       "REMOVE:r2 (a) IF (-1 (*) - (b)) ; " ++
       "REMOVE:r3 (b) IF (-1 (a)) ; " ++
       "REMOVE:r4 (b) IF (-1 (*) - (a)) ; " ++
       "REMOVE:l (c) IF (-1 (c)) ; ")

ex_abc2 = concat $ snd $ parseRules False
     ( "REMOVE (a) IF (-1 (*) - (c)) ;" ++
       "REMOVE (c) IF (1 (a)) ;" ++
       "REMOVE (a) IF (-1 (c)) ;" ++
       "REMOVE (a) IF (-1 (b)) ;" )

main = do
  args <- getArgs
  let ts = map (Tag . (:[])) "abc"
  let tc = sequence [ts] ++ [[Tag "a",Tag "b"],[Tag "a",Tag "c"],[Tag "b",Tag "c"]]
  case args of 
    [] -> do let abc1 = splits ex_abc1
             let abc2 = splits ex_abc2
             
             mapM (testRule ts tc) (abc1++abc2)

    _  -> mapM (testRule ts tc) (splits ex_abc2)

  where 
   splits :: (Eq a) => [a] -> [(a,[a])]
   splits xs = xs `for` \x -> let Just ind = elemIndex x xs
                              in  (x, take ind xs)

   for = flip fmap

--------------------------------------------------------------------------------

--Indices start at 1. More intuitive to talk about w1, w2 vs. w0, w1.
--Using Data.Map because it's nicer to write lookup than (\x -> xs !! (x+1)) :-P
type Sentence = Map Index [Lit]
type TagMap   = Map Tag [Index]
type Index = Int

data World = W { ssent  :: Sentence
               , tagmap :: Map Tag [Index] } deriving (Show,Eq)


inRange :: Index -> Sentence -> Bool
inRange i ssent = i `member` ssent

testRule :: [Tag] -> [[Tag]] -> (Rule, [Rule]) -> IO Bool
testRule ts tcs (x,xs) = do 
  print $ (x, width x)
  print $ (xs, map width xs)
  let w = width x
  s <- newSolver
  symbwords <- fromList `fmap` sequence 
          [ (,) n `fmap` sequence [ newLit s (shTC t n) | t <- tcs ]
              | n <- [1..w] ] :: IO Sentence
  let taglookup = fromList $
                    ts `for` \t -> let getInds = map (1+) . findIndices (elem t)
                                   in  (t, getInds tcs) 
  finalSent <- foldM (apply s taglookup) symbwords xs
  print finalSent
  b <- solve s []
  return b

apply :: Solver -> TagMap -> Sentence -> Rule -> IO Sentence
apply s alltags sentence rule = do

  let trg_difs = toTags $ target rule
  let conds    = toConds $ cond rule

      -- :: Sentence -> (Index,[Lit]) -> Sentence
  let applyToWord sentence (i,sw) = do
       let trgInds_difInds = map lookupTagMap trg_difs -- [([Index],[Index])]
       let conds_positions =
            [ (cs, ps) | (cs, ps) <- conds `zip` (map.map) getPos conds
                       , all (\j -> member (i+j) sentence) (concat ps) ]
       if null conds_positions
        then do 
          print i
          return sentence --out of scope, nothing changed in the sentence
        else do
          --for each condition, make a literal "condition holds"
          --final condition_holds is an OR of all those "this condition holds" literals
          disjConds <- mapM mkCond conds_positions
          condsHold <- if singleton disjConds && (not.null) disjConds
                         then return (head disjConds)
                         else orl s "some_cond_holds" disjConds
          
          return $ changeWord sentence i [condsHold]


  foldM applyToWord sentence (toAscList sentence)

  where     
   mkCond :: ([Condition], --conjunction of conditions
              [[Index]])   --corresponding list of indices for each
           -> IO Lit       --all of that represented by just one literal   
   mkCond (conds,indss) = do
     let cond_inds = zip conds indss
     print cond_inds
     andl s "" =<< sequence 
      [ case (cautious, positive) of
          (False, True) -> do let (yesInds,noInds) = lookupTagMap $ head yes_nos --TODO
                              yesLit <- orl  s "" [true] --(map lookupLit yesInds)
                              noLit  <- andl s "" [false] --(map lookupLit noInds)
                              andl s "" [yesLit, noLit]
          _ -> return true
        | (C position (positive,ctags), inds) <- zip conds indss
        , let yes_nos = toTags ctags
        , let cautious = isCareful position ]

   changeWord ssent i newsw = adjust (const newsw) i ssent

   lookupTagMap :: (Trg,Dif) -> ([Index],[Index])
   lookupTagMap (trg,dif) = 
      let trgInds = concatMap (go []) trg --trg::[[Tag]]
          difInds = concatMap (go []) dif --dif::[[Tag]]
          
      in  (trgInds\\difInds, difInds)
    where
     go acc []     = acc         --default is [] because intersect [] _ == []
     go acc (t:ts) = let inds = [] `fromMaybe` lookup t alltags
                     in go (intersect acc inds) ts
   lookupLit ind = [] `fromMaybe` lookup ind sentence


{-
 TODO:

  We don't need such a heavy machinery as in normal CG_SAT for *finding* context.
  Unless run into a word boundary, then every word has the context to trigger a rule,
  and it will always be the same nth element of the mth word.
  So we don't need actually a getContext, we need just (!!).

    for (i,sw) in sentence:
       trgLits  = lookup target sw

       let condInds = map posToInt conds
       condLits
        <- if all (\j -> withinRange i+j) condInds then do
             cond@C cautious pos (positive,ctags) <- conds
             let yes_nos = toTags ctags
             let posInt = posToInt pos
             let sw = lookup (i+posInt) sentence
             
             case (cautious, positive) of
                (False, True) -> do (yes,nos) <- yes_nos
                                    let difInds  = map lookupTagMap nos
                                    let yesInds = map lookupTagMap yes \\ difInds
                                    yesLit <- orl  s (map lookupLit yesInds)
                                    noLit  <- andl s ... difInds
                                    return [yesLit, noLit]
                (True, True)  -> do ...
                                    difInds = map lookupTagMap nos
                                    yesInds = map lookupTagMap yes \\ difInds
                                    notCInds = allInds \\ yesInds
                                    return [yesLit, noLit]
            else do
              return 
             
       trgLit'   <- andl s [ trgLit, or [ condsHold
                                          , trgLit_is_only_reading ]
                             ]
       addClause ......

  If the rule made new trgLit', make an update to sentence.
  Otherwise not.
-}

--------------------------------------------------------------------------------

width :: Rule -> Int
width = fill . toConds . cond

fill :: [[Condition]] -> Int
fill []   = 1 -- should not happen, toConds returns [[]] as the default option
fill [[]] = 1
fill css = length [minInd..maxInd]
 where
  minInd = 0 `min` minimum poss
  maxInd = 0 `max` maximum poss
  poss = map (\(C pos _) -> posToInt pos) (concat css)

--for (C)BARRIER, count an extra place to place the barrier tag
posToInt :: Position -> Int
posToInt (Exactly _ i) = i
posToInt (AtLeast _ i) = i
posToInt (Barrier _ i _)  = if i<0 then i-1 else i+1
posToInt (CBarrier _ i _) = if i<0 then i-1 else i+1

isCareful :: Position -> Bool
isCareful (Exactly b _) = b
isCareful (AtLeast b _) = b
isCareful (Barrier b _ _) = b
isCareful (CBarrier b _ _) = b

getPos :: Condition -> [Index]
getPos (C (Barrier  _ i _) _) = if i<0 then [i-1, i] else [i, i+1]
getPos (C (CBarrier _ i _) _) = if i<0 then [i-1, i] else [i, i+1]
getPos (C  position        _) = [posToInt position]
getPos _ = error "trying to apply to complex condition"


shTC :: [Tag] -> Int -> String 
shTC ts i = "w" ++ show i ++ (concatMap (\t -> '<':show t++">") ts)

for :: (Functor f) => f a -> (a -> b) -> f b
for = flip fmap

singleton :: [a] -> Bool
singleton [x] = True
singleton _   = False


