import CG_base
import CG_parse
import CG_SAT
import SAT (newSolver)
import SAT.Named

import Data.List
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
  let tc = map ((:[]) . Tag . (:[])) "abc"
  case args of 
    [] -> do let abc1 = splits ex_abc1
             let abc2 = splits ex_abc2
             
             mapM (testRule tc) (abc1++abc2)

    _  -> mapM (testRule tc) (splits ex_abc2)

  where 
   splits :: (Eq a) => [a] -> [(a,[a])]
   splits xs = xs `for` \x -> let Just ind = elemIndex x xs
                              in  (x, take ind xs)

   for = flip fmap

--------------------------------------------------------------------------------

width :: Rule -> Int
width rule = case rule of
  (Select _ _ Always) -> 1
  (Remove _ _ Always) -> 1
  (Select _ _ conds) -> fill (toConds conds)
  (Remove _ _ conds) -> fill (toConds conds)

fill :: [[Condition]] -> Int
fill []   = 1 -- should not happen, toConds returns [[]] as the default option
fill [[]] = 1
fill css = length [minInd..maxInd]
 where
  minInd = 0 `min` minimum poss
  maxInd = 0 `max` maximum poss
  poss = map (\(C pos _) -> posToInt pos) (concat css)
  posToInt (Exactly _ i) = i
  posToInt (AtLeast _ i) = i
  posToInt (Barrier _ i _)  = if i<0 then i-1 else i+1
  posToInt (CBarrier _ i _) = if i<0 then i-1 else i+1

shTC :: [Tag] -> Int -> String 
shTC ts i = "w" ++ show i ++ (concatMap (\t -> '<':show t++">") ts)

--------------------------------------------------------------------------------

testRule :: [[Tag]] -> (Rule, [Rule]) -> IO Bool
testRule tcs (x,xs) = do 
  print $ (x, width x)
  print $ (xs, map width xs)
  let w = width x
  s <- newSolver
  sws <- sequence 
          [ sequence [ newLit s (shTC t n) | t <- tcs ] | n <- [1..w] ] :: IO [[Lit]]
  print sws
  
  return False



apply :: Rule -> Sentence -> IO Sentence
apply r ws = undefined
  
{-
 TODO:
  type Sentence = [SymbWord]
  and type SymbWord = [Lit]?

  We don't need such a heavy machinery as in normal CG_SAT for *finding* context.
  Unless run into a word boundary, then every word has the context to trigger a rule,
  and it will always be the same nth element of the mth word.
  So we don't need actually a getContext, we need just (!!).

  for rule@(Remove target conds):
    for sw in sentence:
       trgLits  = lookupTrg target sw

       *clause forming part*
       for trgLit in trgLits:
         condLits = lookupCnd conds trgLit sentence

         condsHold <- case conds of
                        notCautious,notNegative -> foo condLits
                        isCautious, notNegative -> bar condLits
                        ...
         trgLit'   <- andl s [ trgLit, or [ condsHold
                                          , trgLit_is_only_reading ]
                             ]
         addClause ......

  If the rule made new trgLit', make an update to sentence.
  Otherwise not.

  State monad?
  (oldw1:xs) <- get
  newss = [w1<a>, trgLit', w1<b>]:xs
  modify (const newss)
-}