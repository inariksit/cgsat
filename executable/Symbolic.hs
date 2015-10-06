import CG_base hiding ( Sentence, showSentence )
import CG_parse
import CG_SAT
import SAT ( Solver(..), newSolver, deleteSolver )
import SAT.Named

import Control.Monad
import Data.List hiding ( lookup )
import Data.Map ( Map(..), fromList, toAscList, keys, elems, member, adjust, lookup )
import Data.Maybe
import Debug.Trace
import Prelude hiding ( lookup, Word )
import System.Environment

ex_abc1 = concat $ snd $ parseRules False
     ( "REMOVE:r1 (a) IF (-1 (c)) ; " ++
       "REMOVE:l  (a) IF (-1C  (c)) ; ")

ex_abc2 = concat $ snd $ parseRules False
     ( "REMOVE:r1 (a) IF (-1 (*) - (b)) ;" ++
       "REMOVE:r2 (b) IF ( 1 (a)) ;" ++
       "REMOVE:r3 (a) IF (-1 (b)) ;" ++
       "REMOVE:l  (a) IF (-1 (c)) ;" )

ex_not = concat $ snd $ parseRules False
     ( "REMOVE:r1 (a) IF (NOT -1 (c)) ;" ++
       "REMOVE:r2 (a) IF (-1 (a)) ;" ++
       "REMOVE:l  (b) IF (-1 (c)) ;" )

main = do
  args <- getArgs
  let ts = map (Tag . (:[])) "abc"
  let tc = sequence [ts] -- ++ [[Tag "a",Tag "b"],[Tag "a",Tag "c"],[Tag "b",Tag "c"]]
  case args of 
    [] -> do let abc1 = splits ex_abc1
             let abc2 = splits ex_abc2
             let not_ = splits ex_not
             
             mapM (testRule (True,True) ts tc) (last abc1:[]) --last abc2:last not_:[])

    ("nld":r)
       -> do let verbose = "v" `elem` r || "d" `elem` r
             let debug = "d" `elem` r
             -- ts <- (concat . filter (not.null) . map parse . words) 
             --           `fmap` readFile "data/nld/nld_tags.txt"
             (tsets, rls) <- readRules' "data/nld/nld.rlx"
             let rules = concat rls
             let tcInGr = nub $ concatMap toTags' tsets
             print tcInGr
             tcInLex <- (map parse . words) `fmap` readFile "data/nld/nld_tagcombs.txt"
             let tc = nub $ tcInGr ++ tcInLex
             let ts = nub $ concat tc
             mapM (testRule (verbose,debug) ts tc) (splits rules)

  where 
   splits :: (Eq a) => [a] -> [(a,[a])]
   splits xs = xs `for` \x -> let Just ind = elemIndex x xs
                              in  (x, take ind xs)

   for = flip fmap

   toTags' :: TagSet -> [[Tag]]
   toTags' = concatMap (nub . (\(a,b) -> if all null b then a else a++b)) . toTags
--------------------------------------------------------------------------------

--Indices start at 1. More intuitive to talk about w1, w2 vs. w0, w1.
--(Ab)using Data.Map because lookup and updates go nicely with builtin functions
type Word     = Map WIndex Lit
type Sentence = Map SIndex Word
type TagMap   = Map Tag [WIndex]
type WIndex   = Int
type SIndex   = Int

solveAndPrintSentence :: Solver -> [Lit] -> Sentence -> IO ()
solveAndPrintSentence s ass sent = do
  let lits = concatMap elems (elems sent)
  --print lits
  --litsU <- count s lits
  --solveMaximize s ass litsU
  solve s ass
  vals <- sequence 
           [ sequence  [ modelValue s lit | lit <- elems word ] 
             | (sind,word) <- toAscList sent ]
  let trueAnas =
       [ "\"w" ++ show sind ++ "\"\n"
               ++ unlines [ "\t"++show ana | (ana, True) <- zip (elems word) vs ]
         | ((sind,word), vs) <- zip (toAscList sent) vals ]
  mapM_ putStrLn trueAnas
  putStrLn "----"

printSentence :: Sentence -> IO ()
printSentence sent = do
  let allAnas =
       [ "  \"w" ++ show sind ++ "\" ----> "
               ++ intercalate ", " [ show ana | ana <- elems word ]
         | (sind,word) <- toAscList sent ]
  mapM_ putStrLn allAnas

--------------------------------------------------------------------------------

testRule :: (Bool,Bool) -> [Tag] -> [[Tag]] -> (Rule, [Rule]) -> IO Bool
testRule (verbose,debug) ts tcs (lastrule,rules) = do 
  putStrLn "************* testRule ***************"
  putStrLn $ "Testing with " ++ show lastrule ++ " as the last rule"
  putStrLn "the rest of the rules: " >> mapM_ print rules
  let (w,trgSInd) = width lastrule
  let taginds = [1..length tcs]
  s <- newSolver
  initialSentence <- mkSentence s w tcs
  let taglookup = fromList $
                    ts `for` \t -> let getInds = map (1+) . findIndices (elem t)
                                   in (t, getInds tcs) 
  when verbose $ do
    putStrLn "Initial sentence:"
    printSentence initialSentence
    solveAndPrintSentence s [] initialSentence
    putStrLn "----"
  afterRules <- foldM (applyAndPrint s taglookup taginds) initialSentence rules
  --mapM_ (constrainBoundaries s taglookup) (elems afterRules)

  shouldTriggerLast <-  do
    let luTag = lookupTag taglookup taginds
    let trg_difs = toTags $ target lastrule
    let conds = toConds $ cond lastrule
    let conds_positions =
         [ (cs, map (trgSInd+) ps) | (cs, ps) <- conds `zip` (map.map) getPos conds]
    let trgWInds@((yes,no):_) = map luTag trg_difs --TODO
    let trgLits = map (lookupLit afterRules trgSInd) yes
    let otherLits = map (lookupLit afterRules trgSInd) (taginds \\ yes)
    let otherLitsName = if length otherLits > 3 
                           then show (take 3 otherLits) ++ "..."
                           else show otherLits

    mustHaveTrg  <- orl s ("must have: " ++ show trgLits) trgLits
    mustHaveOther <- orl s ("must have: " ++ otherLitsName) otherLits
    condLits <- mapM (mkCond s luTag (lookupLit afterRules) taginds) conds_positions
    allCondsHold <- andl s ("must hold: " ++ unwords (map show condLits)) condLits
    return [mustHaveTrg, mustHaveOther, allCondsHold]
    
  when debug $ print shouldTriggerLast
  b <- solve s shouldTriggerLast
  if b 
   then do 
      putStrLn $ "Following triggers last rule: " ++ show lastrule
      solveAndPrintSentence s shouldTriggerLast afterRules
   else do
      putStrLn "Conflict!"
      putStrLn $ "Cannot trigger the last rule: " ++ show lastrule
      putStrLn $ "The sentence should have these properties:"
      mapM_ (\x -> putStrLn ("* " ++ show x)) shouldTriggerLast
      putStrLn "This is the next best thing we can do:"
      shouldTriggerLast `forM_` \req -> solveAndPrintSentence s [req] afterRules


  deleteSolver s 
  return b

 where
  applyAndPrint :: Solver -> TagMap -> [WIndex] -> Sentence -> Rule -> IO Sentence
  applyAndPrint s tl ti sent rule = do
    let (w,_) = width rule
    if w > length (elems sent) then do
      putStrLn $ "Rule " ++ show rule ++ " out of scope, no effect"
      putStrLn "-----"
      return sent
     else do
      putStrLn $ "Applied rule " ++ show rule 
      newsent <- apply s tl ti sent rule
      when verbose $ do
        putStrLn "One possible new sentence:"
        when debug $ printSentence newsent
        solveAndPrintSentence s [] newsent
      return newsent

--------------------------------------------------------------------------------

apply :: Solver -> TagMap -> [WIndex] -> Sentence -> Rule -> IO Sentence
apply s alltags taginds sentence rule = do

  let trg_difs = toTags $ target rule
  let conds    = toConds $ cond rule

  sequence_ [ addClause s (elems word) | word <- elems sentence ]

      -- :: Sentence -> (SIndex, Map WIndex Lit) -> Sentence
  let applyToWord sentence (i,sw) = do
       let trgInds = concatMap (fst.luTag) trg_difs --[Int]; difs already included
       let otherInds = taginds \\ trgInds

       let conds_positions =
            [ (cs, map (i+) ps) | (cs, ps) <- conds `zip` (map.map) getPos conds
                                , all (inRange i) cs ]
       if null conds_positions
        then do 
          return sentence --out of scope, nothing changed in the sentence
        else do
          disjConds <- mapM mkCondition conds_positions --for disj. cond. templates
          condsHold <- orl' s disjConds
          onlyTrg <- orl' s  $ map (lu sw) trgInds
          let otherNeg = map (neg . lu sw) otherInds
          noOther <- andl' s otherNeg
          let noOtherName = if length otherNeg < 3 then show otherNeg else "~<everything else>"
          onlyTrgLeft <- andl s ("("++show onlyTrg ++ " & " ++ noOtherName ++ ")")
                                [onlyTrg, noOther]
          cannotApply <- orl' s [ neg condsHold, onlyTrgLeft ]
          
          newTrgLits <- sequence
             --wN<a>' is true if both of the following:
           [ andl s newTrgName [ oldTrgLit     --wN<a> was also true, and
                               , cannotApply ] --rule cannot apply 
               | trgInd <- trgInds
               , let Just oldTrgLit = lookup trgInd sw 
               , let newTrgName = show oldTrgLit ++ "'"]
          putStrLn $ "*** reasons why we couldn't apply: " ++ show cannotApply

          b <- solve s []
          let newsw = foldl changeAna sw (zip trgInds newTrgLits)
          constrainBoundaries s alltags newsw
          return $ changeWord sentence i newsw
          


  foldM applyToWord sentence (toAscList sentence)

  where
   luTag   = lookupTag alltags taginds
   luLit   = lookupLit sentence 
   lu xs x = fromMaybe false $ lookup x xs
   mkCondition = mkCond s luTag luLit taginds

   inRange :: SIndex -> Condition -> Bool
   inRange i (C pos (b,ctags)) = not b -- `NOT -100 a' is always true
                                 || member (i+posToInt pos) sentence

   changeWord :: Sentence -> SIndex -> Word -> Sentence
   changeWord sent i newsw = adjust (const newsw) i sent

   changeAna :: Word -> (WIndex,Lit) -> Word
   changeAna word (i,newana) = adjust (const newana) i word

--------------------------------------------------------------------------------
   
mkCond :: Solver                                -- ^ solver to use
       -> ((Trg,Dif) -> ([WIndex],[WIndex]))    -- ^ lookupTag function
       -> (SIndex -> WIndex -> Lit)             -- ^ lookupLit function
       -> [WIndex]                              -- ^ list of all tag indices
       -> ([Condition],                         -- ^ list of conditions (conjunction)
           [WIndex])                            -- ^ corresponding indices for each
       -> IO Lit                                -- ^ conjunction of all conditions in one literal 
mkCond s luTag luLit ti (conds,inds) = andl' s  =<< sequence 
  [ do case position of
             (Barrier  foo bar btags) 
               -> do let byes_bnos = map luTag (toTags btags)
                     let bi = if ind<0 then ind-1 else ind+1
                     addClause s [true] --TODO
                                         
             (CBarrier foo bar btags)
               -> do let byes_bnos = map luTag (toTags btags)
                     let bi = if ind<0 then ind-1 else ind+1
                     addClause s [true] --TODO
             foo -> return ()

       -- disjunction of *tags* in one condition
       orl s (show c ++ " in " ++ show ind) =<< sequence 
        ( case (positive, cautious) of
           (True, False)  -> [ do y <- orl  s "" yesLits
                                  n <- andl s "" (map neg difLits)
                                  andl s "" [y,n]
                                | (yi, di) <- yesInds_difInds
                                , let yesLits = map (luLit ind) yi
                                , let difLits  = map (luLit ind) di ]
           (True, True)   -> [ do y <- orl  s "" yesLits
                                  n <- andl s "" (map neg other)
                                  andl s "" [y,n]
                                | (yi, di) <- yesInds_difInds
                                , let yesLits = map (luLit ind) yi
                                , let other = map (luLit ind) (ti \\ yi) ]
           (False, False) -> [ do n <- andl s "" (map neg noLits)
                                  y <- orl s "" other --need something positive
                                  andl s "" [y,n]
                                | (yi, di) <- yesInds_difInds
                                , let noLits = map (luLit ind) yi 
                                , let other = map (luLit ind) (ti \\ yi) ]
           (False, True)  -> [ orl s "" other
                                | (yi, di) <- yesInds_difInds
                                , let other = map (luLit ind) (ti \\ yi) ] )
        | (c@(C position (positive,ctags)), ind) <- zip conds inds
        , let yesInds_difInds = map luTag (toTags ctags)
        , let cautious = isCareful position ]

--------------------------------------------------------------------------------

constrainBoundaries :: Solver -> TagMap -> Word -> IO ()
constrainBoundaries s alltags word = do
  let allInds = keys word 
  let bdTags = [EOS, BOS, Tag "sent", Tag "cm"]
  let bdInds = concat $ catMaybes $ map (\x -> lookup x alltags) bdTags
  let nonBdInds = allInds \\ bdInds
  print ("* constrainBoundaries",bdInds)

  let bds    = catMaybes $ map (\x -> lookup x word) bdInds
  let nonbds = catMaybes $ map (\x -> lookup x word) nonBdInds
  isBd  <- orl' s bds
  nonBd <- andl s "not boundary" (map neg nonbds)
  print [neg isBd, nonBd]
  addClause s [neg isBd, nonBd]

--------------------------------------------------------------------------------

mkSentence :: Solver -> Int -> [[Tag]] -> IO Sentence
mkSentence s w tcs = fromList `fmap` sequence 
                       [ ((,) n . fromList) `fmap` sequence 
                         [ (,) m `fmap` newLit s (shTC t n) | (m, t) <- zip [1..] tcs ]
                            | n <- [1..w] ] 
 where shTC ts i = "w" ++ show i ++ (concatMap (\t -> '<':show t++">") ts)

lookupTag :: TagMap -> [WIndex] -> (Trg,Dif) -> ([WIndex],[WIndex])
lookupTag alltags allinds (trg,dif) = 
  let trgInds = if trg==[[]] then allinds
                   else concatMap (go allinds) trg --trg::[[Tag]]
      difInds = if dif==[[]] then [] 
                      else concatMap (go allinds) dif --dif::[[Tag]]
          
  in  (trgInds\\difInds, difInds)
 where
  go acc []     = acc         --default is [] because intersect [] _ == []
  go acc (t:ts) = let inds = [] `fromMaybe` lookup t alltags
                  in go (intersect acc inds) ts

lookupLit :: Sentence -> SIndex -> WIndex -> Lit
lookupLit sentence si wi = 
  case lookup si sentence of
    Just ts -> case lookup wi ts of
                 Just tag -> tag
                 Nothing  -> error "lookupLit: tag combination not found"
    Nothing -> true --sent here by negated rule: `NOT -1000 foo' is always true

--------------------------------------------------------------------------------

width :: Rule -> (Int,SIndex) --width of rule + index of target
width = fill . toConds . cond

fill :: [[Condition]] -> (Int,SIndex)
fill []   = (1,1) -- should not happen, toConds returns [[]] as the default option
fill [[]] = (1,1)
fill css = (length [minInd..maxInd], 
            1+(fromJust $ elemIndex 0 [minInd..maxInd]))
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

for :: (Functor f) => f a -> (a -> b) -> f b
for = flip fmap

singleton :: [a] -> Bool
singleton [x] = True
singleton _   = False

--------------------------------------------------------------------------------

parse :: String -> [Tag]
parse str = map toTag $ filter (not.null) $ split isValid str
 where 
  isValid c = c=='<' || c=='+'
  toTag ">>>" = BOS
  toTag "<<<" = EOS
  toTag []    = error "empty tag"
  toTag str = if last str=='>' then Tag (init str) else Lem str

split :: (a -> Bool) -> [a] -> [[a]]
split p [] = []
split p xs = takeWhile (not . p) xs : split p (drop 1 (dropWhile (not . p) xs))