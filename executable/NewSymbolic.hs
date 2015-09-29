import CG_base hiding ( Sentence )
import CG_parse
import CG_SAT
import SAT ( newSolver )
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
--Using map because lookup is nicer to write than (\x -> xs !! (x+1)) :-P
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
  symbwords <- sequence 
          [ (,) n `fmap` sequence [ newLit s (shTC t n) | t <- tcs ]
              | n <- [1..w] ] :: IO [(Index,[Lit])]
  let taglookup = ts `for` \t -> let getInds = map (1+) . findIndices (elem t)
                                 in  (t, getInds tcs) 
  let initialWorld = W (fromList symbwords) (fromList taglookup)
  finalWorld <- execStateT (apply x) initialWorld 
  b <- solve s []
  return b

apply :: Rule -> StateT World IO ()
apply r = do

  let trg_difs = toTags $ target r
  let conds    = toConds $ cond r

  sentence <- gets ssent
    -- [(1,[w1<a>,w1<b>,...]),(2,[...])]
  let words = toAscList sentence 
  return ()
  -- let applyToWord w = do
  --    let trgLits = lookupTagMap trg w

 -- modify (\w -> w { ssent = ssent w `changeWord` i sw })

  where
   changeWord ssent i newsw = adjust (const newsw) i ssent

   lookupTagMap :: (Trg,Dif) -> TagMap -> [Index]
   lookupTagMap (trg,dif) w = 
      let trgInds = concatMap (go []) trg --trg::[[Tag]]
          difInds = concatMap (go []) dif --dif::[[Tag]]
          
      in  trgInds \\ difInds
    where
     go acc []     = acc
     go acc (t:ts) = let inds = [] `fromMaybe` lookup t w
                     in go (intersect acc inds) ts
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

  lookupTagMap tags = fold intersect [] $ map (lookup tagMap) tags

  State monad?
  (oldw1:xs) <- get
  newss = [w1<a>, trgLit', w1<b>]:xs
  modify (const newss)
-}

--------------------------------------------------------------------------------

for = flip fmap

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

shTC :: [Tag] -> Int -> String 
shTC ts i = "w" ++ show i ++ (concatMap (\t -> '<':show t++">") ts)