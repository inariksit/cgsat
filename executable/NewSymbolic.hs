import CG_base hiding ( Sentence, showSentence )
import CG_parse
import CG_SAT
import SAT ( Solver(..), newSolver )
import SAT.Named

import Control.Monad
import Data.List hiding ( lookup )
import Data.Map ( Map(..), fromList, toAscList, member, adjust, lookup )
import Data.Maybe
import Debug.Trace
import Prelude hiding ( lookup )
import System.Environment

ex_abc1 = concat $ snd $ parseRules False
     ( "REMOVE:r1 (a) IF (-1 (b)) ; " ++
       "REMOVE:r2 (a) IF (-1 (*) - (b)) ; " ++
       "REMOVE:r3 (b) IF (-1 (a)) ; " ++
       "REMOVE:r4 (b) IF (-1 (*) - (a)) ; " ++
       "REMOVE:l (c) IF (-1 (c)) ; ")

ex_abc2 = concat $ snd $ parseRules False
     ( "SET CNotB = (c) - (b) ;" ++
       "REMOVE (a) IF (-1 (*) - (c)) ;" ++
       "REMOVE (c) IF (NOT 1 (a)-(b)) ;" ++
       "REMOVE (a) IF (-1C CNotB OR (a b)) ;" ++
       "REMOVE (a) IF (-1* (b) BARRIER (c)) ;" )

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
--(Ab)using Data.Map because lookup and updates go nicely with builtin functions
type Word     = Map WIndex Lit
type Sentence = Map SIndex Word
type TagMap   = Map Tag [WIndex]
type WIndex   = Int
type SIndex   = Int

printSentence :: Solver -> Sentence -> IO ()
printSentence s sent = do
  vals <- sequence 
           [ sequence  [ modelValue s lit | (wind,lit) <- toAscList word ] 
             | (sind,word) <- toAscList sent ]
  let trueAnas =
       [ "\"word" ++ show sind ++ "\"\n"
                ++ concat [ "\t"++show ana++"\n" | ((_,ana), True) <- zip (toAscList word) vs ]
         | ((sind,word), vs) <- zip (toAscList sent) vals ]
  mapM_ putStrLn trueAnas

--------------------------------------------------------------------------------

testRule :: [Tag] -> [[Tag]] -> (Rule, [Rule]) -> IO Bool
testRule ts tcs (x,xs) = do 
  print $ (x, width x)
  print $ (xs, map width xs)
  let w = width x
  let l = [1..length tcs]
  s <- newSolver
  symbwords <- fromList `fmap` sequence 
          [ (,) n `fmap` 
                  (fromList `fmap` sequence 
                    [ (,) m `fmap` newLit s (shTC t n) | (t,m) <- zip tcs [1..] ])
              | n <- [1..w] ] :: IO Sentence
  let taglookup = fromList $
                    ts `for` \t -> let getInds = map (1+) . findIndices (elem t)
                                   in (t, getInds tcs) 
  finalSent <- foldM (apply s taglookup l) symbwords (x:xs)
  print finalSent
  b <- solve s []
  if b 
   then do 
    printSentence s finalSent
    return b
   else return b

apply :: Solver -> TagMap -> [WIndex] -> Sentence -> Rule -> IO Sentence
apply s alltags taginds sentence rule = do

  let trg_difs = toTags $ target rule
  let conds    = toConds $ cond rule

      -- :: Sentence -> (SIndex, Map WIndex Lit) -> Sentence
  let applyToWord sentence (i,sw) = do
       let trgInds = concatMap (fst.lookupTag) trg_difs --[Int]; difs already included
       let conds_positions =
            [ (cs, map (i+) ps) | (cs, ps) <- conds `zip` (map.map) getPos conds
                                , all (inRange i) cs ]
       if null conds_positions
        then do 
          print ("out of scope:",i,sw)
          return sentence --out of scope, nothing changed in the sentence
        else do
          --for each condition, make a literal "condition holds"
          --final condition_holds is an OR of all those "this condition holds" literals
          disjConds <- mapM mkCond conds_positions
          condsHold <- if singleton disjConds
                         then return (head disjConds)
                         else orl s (show $ cond rule) disjConds
          print ("condsHold",disjConds,condsHold)
          newTrgLits <- sequence
            [ do il <- implies s implName condsHold (nt trgLit)
                 addClause s [il]
                 newLit s (show trgLit ++ "'")
               | trgInd <- trgInds
               , let Just trgLit = lookup trgInd sw
               , let nt = case rule of
                           (Remove _ _ _) -> neg
                           (Select _ _ _) -> id 
               , let implName = show condsHold ++ "=>" ++ show (nt trgLit) ]
          
          let newWord = foldl changeAna sw (zip trgInds newTrgLits)
          print newWord
          return $ changeWord sentence i newWord


  foldM applyToWord sentence (toAscList sentence)

  where
   inRange :: SIndex -> Condition -> Bool
   inRange i (C pos (b,ctags)) = not b -- `NOT -100 a' is always true
                                 || member (i+posToInt pos) sentence

   changeWord :: Sentence -> SIndex -> Word -> Sentence
   changeWord sent i newsw = adjust (const newsw) i sent

   changeAna :: Word -> (WIndex,Lit) -> Word
   changeAna word (i,newana) = adjust (const newana) i word
   
   mkCond :: ([Condition], --conjunction of conditions
              [WIndex])    --corresponding indices for each
           -> IO Lit       --all of that represented by just one literal   
   mkCond (conds,inds) = andl s (show conds) =<< 
     sequence 
      [ do case position of
             (Barrier  _ _ btags) 
                -> do let byes_bnos = map lookupTag (toTags btags)
                      let bi = if ind<0 then ind-1 else ind+1
                      addClause s [true]
                                         
             (CBarrier _ _ btags)
                -> do let byes_bnos = map lookupTag (toTags btags)
                      let bi = if ind<0 then ind-1 else ind+1
                      addClause s [true]
             _  -> return ()

           disjTags <- case (positive, cautious) of
                         (True, False) ->
                           sequence [ do y <- orl  s "" yesLits
                                         n <- andl s "" (map neg difLits)
                                         andl s "" [y,n]
                                       | (yi, di) <- yesInds_difInds
                                       , let yesLits = map (lookupLit ind) yi
                                       , let difLits  = map (lookupLit ind) di ]
                         (True, True)  ->
                           sequence [ do y <- orl  s "" yesLits
                                         n <- andl s "" (map neg other)
                                         andl s "" [y,n]
                                       | (yi, _) <- yesInds_difInds
                                       , let yesLits = map (lookupLit ind) yi
                                       , let other = map (lookupLit ind) (ti \\ yi) ]
                         (False, False) ->
                           sequence [ do n <- andl s "" (map neg noLits)
                                         y <- orl s "" other --need something positive
                                         andl s "" [y,n]
                                       | (yi, di) <- yesInds_difInds
                                       , let noLits = map (lookupLit ind) yi 
                                       , let other = map (lookupLit ind) (ti \\ yi) ]
                         (False, True)  ->
                           sequence [ do orl s "" other
                                       | (yi, di) <- yesInds_difInds
                                       , let other = map (lookupLit ind) (ti \\ yi) ]
           orl s (show c) disjTags
        | (c@(C position (positive,ctags)), ind) <- zip conds inds
        , let yesInds_difInds = map lookupTag $ toTags ctags
        , let cautious = isCareful position 
        , let ti = taginds ]

   lookupTag :: (Trg,Dif) -> ([WIndex],[WIndex])
   lookupTag (trg,dif) = 
      let trgInds = if trg==[[]] then taginds
                      else concatMap (go taginds) trg --trg::[[Tag]]
          difInds = if dif==[[]] then [] 
                      else concatMap (go taginds) dif --dif::[[Tag]]
          
      in  (trgInds\\difInds, difInds)
    where
     go acc []     = acc         --default is [] because intersect [] _ == []
     go acc (t:ts) = let inds = [] `fromMaybe` lookup t alltags
                     in go (intersect acc inds) ts

   lookupLit :: SIndex -> WIndex -> Lit
   lookupLit si wi = case lookup si sentence of
                       Just ts -> case lookup wi ts of
                                   Just tag -> tag
                                   Nothing  -> error "lookupLit: tag combination not found"
                       Nothing -> true  --we've been sent here by negated rule,
                                        --`NOT -1000 foo' is always true

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

getPos :: Condition -> SIndex
getPos (C (Barrier  _ i _) _) = i
getPos (C (CBarrier _ i _) _) = i
getPos (C  position        _) = posToInt position
getPos _ = error "trying to apply to complex condition"


shTC :: [Tag] -> Int -> String 
shTC ts i = "w" ++ show i ++ (concatMap (\t -> '<':show t++">") ts)

for :: (Functor f) => f a -> (a -> b) -> f b
for = flip fmap

singleton :: [a] -> Bool
singleton [x] = True
singleton _   = False


