module CG_Symbolic where 

import CG_base hiding ( Sentence, showSentence )
import SAT ( Solver(..), newSolver, deleteSolver )
import SAT.Named

import Control.Monad
import qualified Data.IntSet as IS
import Data.List hiding ( lookup )
--import qualified Data.List.Ordered as O
import Data.Map ( Map(..), fromList, toAscList, keys, elems, member, adjust, lookup, filterWithKey )
import Data.Maybe
import Debug.Trace
import Prelude hiding ( lookup, Word )
import Text.Regex


--------------------------------------------------------------------------------
--Indices start at 1. More intuitive to talk about w1, w2 vs. w0, w1.
--(Ab)using Data.Map because lookup and updates go nicely with builtin functions
type Word     = Map WIndex Lit
type Sentence = Map SIndex Word
type TagMap   = Map Tag WIndSet
type WIndex   = Int
type SIndex   = Int
type WIndSet  = IS.IntSet
type SIndSet  = IS.IntSet

solveAndPrintSentence :: Bool -> Solver -> [Lit] -> Sentence -> IO ()
solveAndPrintSentence verbose s ass sent = do
  let lits = concatMap elems (elems sent)
  b <- solve s ass
  if b then do
          when verbose $ print ass
          vals <- sequence 
                   [ sequence [ modelValue s lit | lit <- elems word ] 
                      | (sind,word) <- toAscList sent ]
          let trueAnas =
               [ "\"w" ++ show sind ++ "\"\n"
                  ++ unlines [ "\t"++show ana | (ana, True) <- zip (elems word) vs ]
                 | ((sind,word), vs) <- zip (toAscList sent) vals ]
          mapM_ putStrLn trueAnas
          putStrLn "----"
      else do
        putStrLn $ "solveAndPrintSentence: Conflict with assumptions " ++ show ass

printSentence :: Sentence -> IO ()
printSentence sent = do
  let allAnas =
       [ "  \"w" ++ show sind ++ "\" ----> "
               ++ intercalate ", " [ show ana | ana <- elems word ]
         | (sind,word) <- toAscList sent ]
  mapM_ putStrLn allAnas

--------------------------------------------------------------------------------
{-
testRules :: (Bool,Bool) -> [Tag] -> [[Tag]] -> [Rule] -> IO ()
testRules (verbose,debug) ts tcs rules = do 
  s <- newSolver
  initialSent <- mkSentence s 5 tcs
  let taginds = [1..length tcs]
  let taglookup = mkTagMap ts tcs
  let luTag = lookupTag taglookup (IS.fromList taginds)

  let checkAndApply sentence rule = do
       let (w,tSInd) = width rule
       let luLit     = lookupLit sentence
       let trg_difs  = toTags $ target rule
       let conds     = toConds $ cond rule
       let conds_pos = [ (cs, map (tSInd+) ps) 
                          | (cs, ps) <- conds `zip` (map.map) getPos conds ]
       (mustHaveTrg, mustHaveOther, allCondsHold) <- do
         let trgWInds@((yes,no):_) = map luTag trg_difs --TODO
         let trgLits = map (luLit tSInd) yes
         let otherLits = map (luLit tSInd) (taginds \\ yes)
         mht <- orl s ("must have: " ++ mkVarName trgLits) trgLits
         mho <- orl s ("must have: " ++ mkVarName otherLits) otherLits
         condLits <- mapM (mkCond s luTag luLit taginds) conds_pos
         ach <- orl s ("must hold: " ++ unwords (map show condLits)) condLits
         return (mht, mho, ach)
       let shouldTriggerLast = [mustHaveTrg, mustHaveOther, allCondsHold]
       putStrLn "testRules.important solve..."
       b <- solve s shouldTriggerLast
       putStrLn "testRules.important solve done"
-- apply after checking if conditions can hold
       newsent <- apply s taglookup taginds sentence rule
       if b then return newsent
          else do putStrLn $ "conflict with rule " ++ show rule
                  return newsent
   
  foldM checkAndApply initialSent rules
  putStrLn "end!"
 where
  mkVarName :: [Lit] -> String
  mkVarName (x:y:z:_) = show [x,y,z] ++ "..."
  mkVarName lits      = show lits
  -}

--------------------------------------------------------------------------------

testRule :: (Bool,Bool) -> [Tag] -> [[Tag]] -> (Rule, [Rule]) -> IO Bool
testRule (verbose,debug) ts tcs (lastrule,rules) = do 
  putStrLn $ "Testing with " ++ show lastrule ++ " as the last rule"
  when verbose $ do
    putStrLn "************* testRule ***************"
    --putStrLn $ "Testing with " ++ show lastrule ++ " as the last rule"
    putStrLn "the rest of the rules: " >> mapM_ print rules
  let (w,trgSInd) = width lastrule
  let taginds = IS.fromList [1..length tcs]
  let trg_difs = toTags $ target lastrule
  let conds = toConds $ cond lastrule
  let conds_positions = [ (cs, map (trgSInd+) ps) | (cs, ps) <- conds `zip` (map.map) getPos conds ]

  s <- newSolver
  initialSentence <- mkSentence s w tcs
  let taglookup = mkTagMap ts tcs
  let luTag = lookupTag taglookup taginds

  when debug $ do
    putStrLn "Initial sentence:"
    printSentence initialSentence
    putStrLn "----"
  afterRules <- foldM (applyAndPrint s taglookup taginds) initialSentence rules

  --Add constraints: word can't be boundary & other word
  --TODO: find the constraints from lexicon
  let bdTags = [EOS, BOS, Tag "sent", Tag "cm"]
  let bdInds = IS.unions ( catMaybes $ map (\x -> lookup x taglookup) bdTags)
  let nonBdInds = taginds IS.\\ bdInds
  mapM_ (constrainBoundaries s bdInds nonBdInds) (elems afterRules)
  --

  (mustHaveTrg, mustHaveOther, allCondsHold) <- do
    let trgWInds@((yes,no):_) = map luTag trg_difs --TODO
    let trgLits = map (lookupLit afterRules trgSInd) (IS.toList yes)
    let otherLits = map (lookupLit afterRules trgSInd) (IS.toList $ taginds IS.\\ yes)
    let otherLitsName = if length otherLits > 3 
                           then show (take 3 otherLits) ++ "..."
                           else show otherLits
    let trgLitsName = if length trgLits > 3 
                           then show (take 3 trgLits) ++ "..."
                           else show trgLits
    mht <- orl s ("must have: " ++ trgLitsName) trgLits
    mho <- orl s ("must have: " ++ otherLitsName) otherLits
    condLits <- mapM (mkCond s luTag (lookupLit afterRules) taginds) conds_positions
           --disjunction of conjunctions of conditions (for template conditions)
    ach <- orl s ("must hold: " ++ unwords (map show condLits)) condLits
    return (mht, mho, ach)

  let shouldTriggerLast = [mustHaveTrg, mustHaveOther, allCondsHold]
  when debug $ print shouldTriggerLast

  --putStrLn "testRule.important solve..."
  b <- solve s shouldTriggerLast
  --putStrLn "testRule.important solve done"
  if b 
   then do 
      when verbose $ do 
           putStrLn $ "Following triggers last rule: " ++ show lastrule
           solveAndPrintSentence False s shouldTriggerLast afterRules
      return ()
   else do
      putStrLn "Conflict!"
      putStrLn $ "Cannot trigger the last rule: " ++ show lastrule
      putStrLn $ "with the previous rules:"
{-      when verbose $ mapM_ print rules --if verbose, old rules not visible on screen anymore
      putStrLn $ "The sentence should have these properties:"
      mapM_ (\x -> putStrLn ("* " ++ show x)) shouldTriggerLast
      vals <- filter (\x -> length x == 2) (subsequences shouldTriggerLast) 
               `forM` \req -> solve s req
      suspiciousRules <- case vals of
         (False:False:False:[])
           -> do putStr   "Problem appears with all combinations,"
                 putStrLn "trying out each individual requirement:"
                 shouldTriggerLast `forM_` \req -> solveAndPrintSentence True s [req] afterRules
                 return []
         (False:False:_)
           -> do putStrLn "Problem is with target"
                 putStrLn "Look for other rules with same target"
                 let rulesSameTrg = findSameTarget rules w (target lastrule) Nothing
                 mapM_ (\x -> putStrLn ("* " ++ show x)) rulesSameTrg
                 putStrLn "Is the target an existing tag combination?"
                 return rulesSameTrg

         (_:False:False:_)
           -> do putStrLn "Problem is with conditions."
                 let condsInAll = intersect1 conds --only conditions that are in all disjunctions
                 let new_cps = [ (cond, pos) | cond <- condsInAll
                                             , let pos = trgSInd + getPos cond ] ::  [(Condition, SIndex)]
                 let luTag = lookupTag taglookup taginds
                 let mkCond' = mkCond s luTag (lookupLit afterRules) taginds
                 offendingConds <- findSuspiciousConditions s [mustHaveTrg, mustHaveOther] new_cps conds_positions mkCond' :: IO [[(Condition,WIndex)]] --each list is a combination that conflicts
                 when verbose $ do
                   putStrLn "Candidates for offending conditions:"
                   sequence_ [ do putStrLn "  Combination of following:"
                                  mapM_ pr condList
                                  putStrLn ""
                               | condList <- offendingConds
                               , let pr = \(c,p) -> putStrLn ("  * " ++show c++" at "++show p) ]
                   putStrLn ""

                 --1) Are conditions conflicting with each other?
                 suspCondLits <- mapM (mkCond' . unzip) offendingConds
                 b <- solve s suspCondLits
                 if b then return ()
                  else do putStrLn "The following conditions conflict with each other:"
                          mapM_ (\x -> putStrLn $ "* " ++ show x) suspCondLits

                 --2) Do conditions contain valid tag sets? (Only relevant with strict-tags)
                 let tcNotInGr =
                      nub $ catMaybes [ if null (t++d) then Just cond else Nothing
                                       | (cond,_) <- concat offendingConds
                                       , ctags <- toTags $ getTagset cond 
                                       , let (t,d) = luTag ctags ]

                 when (not $ null tcNotInGr) $ do
                   putStrLn "Tag combinations not defined in grammar (--strict-tags):"
                   mapM_ (\x -> putStrLn $ "* " ++ show x) tcNotInGr
                 if length (concat offendingConds)==length tcNotInGr || not b
                   then return []
                   --3) Do other rules target the conditions?
                   else do putStrLn "Look for other rules that have the conditions as target"
                           let c_is = map (\(c,i) -> (getTagset c, Just i)) (concat offendingConds)
                           let sameCondsAsTrg = concatMap (uncurry $ findSameTarget rules w) c_is
                           mapM_ (\r -> putStrLn ("* " ++ show r)) sameCondsAsTrg
                           putStrLn ""
                           return $ sameCondsAsTrg
         
         (_:False:_)
           -> do putStrLn "Problem is target+conditions"
                 putStrLn "looking for other rules that have the same target"
                 let rulesSameTrg = findSameTarget rules w (target lastrule) Nothing
                 mapM_ (\x -> putStrLn ("* " ++ show x)) rulesSameTrg
                 return rulesSameTrg
         (False:_)
           -> do putStrLn "Problem is target+other"
                 putStrLn "looking for other rules that have the same target"
                 let rulesSameTrg = findSameTarget rules w (target lastrule) Nothing
                 mapM_ (\x -> putStrLn ("* " ++ show x)) rulesSameTrg
                 return rulesSameTrg

         _ -> do putStrLn "Problem is in combination of all three requirements"
                 putStrLn "Looking for all things:"
                 let rulesSameTrg = findSameTarget rules w (target lastrule) Nothing
                 putStrLn "Rules with same target:"
                 mapM_ (\x -> putStrLn ("* " ++ show x)) rulesSameTrg
                 
                 --let rules
                 return rulesSameTrg 

      if null suspiciousRules
        then do return ()
        else do putStrLn "Suspicious rules are here, TODO try out rules without them"
                mapM_ print suspiciousRules 
      putStrLn "----------\n"--}

  deleteSolver s 
  return b

 where
  findSuspiciousConditions s otherReqs new_cps old_cps mkCond' = do
    singleConds <- catMaybes `fmap` sequence
      [ do condLitsExcept <- mapM mkCond' cps_except
           allCondsHoldExcept <- if length condLitsExcept == 1 
                                   then return $ head condLitsExcept
                                   else orl' s condLitsExcept
           putStrLn "findSuspiciousConditions.solve..."
           b <- solve s (allCondsHoldExcept:otherReqs)
           putStrLn "findSuspiciousConditions.solve done"
           
           return $ if b then Just [cp] --skipping this condition makes it work
                         else Nothing
        | cp@(missingCond, pos) <- new_cps
        , let cps_except = deleteInAll [cp] old_cps ]
    if null singleConds then do 
      condPairs <- catMaybes `fmap` sequence
                   [ do cle <- mapM mkCond' cps_except2
                        allCondsHoldExcept2 <- orl' s cle
                        putStrLn "findSuspiciousConditions.solve..."
                        b <- solve s  (allCondsHoldExcept2:otherReqs)
                        putStrLn "findSuspiciousConditions.solve done"
                        return $ if b then Just condPair else Nothing
                     | condPair <- filter (\x->length x==2) (subsequences new_cps)
                     , let cps_except2 = deleteInAll condPair old_cps ]
      return condPairs
     else return singleConds
 

  applyAndPrint :: Solver -> TagMap -> WIndSet -> Sentence -> Rule -> IO Sentence
  applyAndPrint s tl ti sent rule = do
    let (w,_) = width rule
    if w > length (elems sent) then do
      when debug $ do
        putStrLn $ "Rule " ++ show rule ++ " out of scope, no effect"
        putStrLn "-----"
      return sent
     else do
      newsent <- apply s tl ti sent rule
      when debug $ do
        putStrLn $ "Applied rule " ++ show rule 
        putStrLn "One possible new sentence:"
        when debug $ printSentence newsent
        solveAndPrintSentence False s [] newsent
        return ()
      return newsent

findSameTarget :: [Rule] -> Int -> TagSet -> Maybe SIndex -> [Rule]
findSameTarget rules w trg trgSInd =
 case trgSInd of
   Just ind -> [ rule | (rule, tss) <- zip rules otherTrgs
                      , let (w',sInd) = width rule
                    --  , sInd == ind
                      , w>=w'
                      , any (\ts -> ts `elem` tss) lastTrg ]
   Nothing  -> [ rule | (rule, tss) <- zip rules otherTrgs
                      , let (w',_) = width rule
                      , w>=w'
                      , any (\ts -> ts `elem` tss) lastTrg ]
 where 
  lastTrg = justTS trg
  otherTrgs = map (justTS.target) rules
  

  justTS :: TagSet -> [[[Tag]]]
  justTS (TS ts)      = [ts]
  justTS (Or ts1 ts2) = justTS ts1 ++ justTS ts2
  justTS (Diff ts1 ts2) = justTS ts1 ++ justTS ts2
  justTS (Cart ts1 ts2) = justTS ts1 ++ justTS ts2
  justTS All            = []


  
--------------------------------------------------------------------------------

apply :: Solver -> TagMap -> WIndSet -> Sentence -> Rule -> IO Sentence
apply s alltags taginds sentence rule = do
  let trg_difs = toTags $ target rule --(Trg,Dif)
  let trgIndsRaw = IS.unions $ map (fst.luTag) trg_difs --[Int]; difs already included
  let otherIndsRaw = taginds IS.\\ trgIndsRaw
  let (trgInds,otherInds) = if isSelect rule
                              then (otherIndsRaw,trgIndsRaw)
                              else (trgIndsRaw,otherIndsRaw)
  let conds = toConds $ cond rule
  let trgIndsList = IS.toList trgInds
  --at least one reading per word
  sequence_ [ addClause s (elems word) | word <- elems sentence ]

  -- applyToWord :: Sentence -> (SIndex, Map WIndex Lit) -> Sentence
  let applyToWord sentence (i,sw) = do
       let conds_positions =
            [ (cs, map (i+) ps) | (cs, ps) <- conds `zip` (map.map) getPos conds
                                , all (inRange i) cs ]
       if null conds_positions
        then do 
          return sentence --out of scope, nothing changed in the sentence
        else do
          --print "mapM mkCondition conds_positions..."
          disjConds <- mapM mkCondition conds_positions --for disj. cond. templates
          --print "mapM mkCondition conds_positions done"
          condsHold <- orl' s disjConds
          let trgPos = map (lu sw) trgIndsList
          let otherNeg = map (neg . lu sw) (IS.toList otherInds)
          trgIsTrue <- orl' s trgPos
          noOther <- andl' s otherNeg
          let onlyTrgName = if length trgPos < 5 then show trgPos else show (take 3 trgPos) ++ "..."
          let noOtherName = if length otherNeg < 3 then show otherNeg else "~<everything else>"
          onlyTrgLeft <- andl s ("("++onlyTrgName ++ " & " ++ noOtherName ++ ")")
                                [trgIsTrue, noOther]
          cannotApply <- orl' s [ neg condsHold, onlyTrgLeft ]
          
          newTrgLits <- sequence
             --wN<a>' is true if both of the following:
           [ andl s newTrgName [ oldTrgLit     --wN<a> was also true, and
                               , cannotApply ] --rule cannot apply 
               | trgInd <- trgIndsList
               , let Just oldTrgLit = lookup trgInd sw 
               , let newTrgName = show oldTrgLit ++ "'" ]
          --putStrLn $ "*** reasons why we couldn't apply: " ++ show cannotApply
          --sequence_ [ putStr "#" | _ <- disjConds ++ trgPos ++ otherNeg ++ [condsHold, onlyTrg, noOther, onlyTrgLeft, cannotApply] ++ newTrgLits ] --size

          --b <- solve s []
          let newsw = foldl changeAna sw (zip trgIndsList newTrgLits)
          --constrainBoundaries s alltags newsw
          return $ changeWord sentence i newsw
          


  foldM applyToWord sentence (toAscList sentence)

  where
   luTag   = lookupTag alltags taginds 
   luLit   = lookupLit sentence 
   lu xs x = fromMaybe false $ lookup x xs
   mkCondition = mkCond s luTag luLit taginds

   inRange :: SIndex -> Condition -> Bool
   inRange i Always = True
   inRange i (C pos (b,ctags)) = not b -- `NOT -100 a' is always true
                                 || member (i+posToInt pos) sentence

   changeWord :: Sentence -> SIndex -> Word -> Sentence
   changeWord sent i newsw = adjust (const newsw) i sent

   changeAna :: Word -> (WIndex,Lit) -> Word
   changeAna word (i,newana) = adjust (const newana) i word

--------------------------------------------------------------------------------
   
mkCond :: Solver                                -- ^ solver to use
--       -> ((Trg,Dif) -> ([WIndex],[WIndex]))
       -> ((Trg,Dif) -> (WIndSet,WIndSet))      -- ^ lookupTag function
       -> (SIndex -> WIndex -> Lit)             -- ^ lookupLit function
--       -> [WIndex]                              -- ^ list of all tag indices
       -> WIndSet                               -- ^ IntSet of all tag indices
       -> ([Condition],                         -- ^ list of conditions (conjunction)
           [WIndex])                            -- ^ corresponding indices for each
       -> IO Lit                                -- ^ conjunction of all conditions in one literal 
mkCond s luTag luLit ti (conds,inds) = andl' s =<< sequence 
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
                                , let yesLits = map (luLit ind) (IS.toList yi)
                                , let difLits  = map (luLit ind) (IS.toList di) ]
           (True, True)   -> [ do y <- orl  s "" yesLits
                                  n <- andl s "" (map neg other)
                                  andl s "" [y,n]
                                | (yi, di) <- yesInds_difInds
                                , let only_di = IS.toList (ti IS.\\ yi)
                                , let yesLits = map (luLit ind) (IS.toList yi)
                                , let other = map (luLit ind) only_di ]
           (False, False) -> [ do n <- andl s "" (map neg noLits)
                                  y <- orl s "" other --some lit must be positive
                                  andl s "" [y,n]
                                | (yi, di) <- yesInds_difInds
                                , let only_di = IS.toList (ti IS.\\ yi) 
                                , let noLits = map (luLit ind) (IS.toList yi)
                                , let other = map (luLit ind) only_di ]
           (False, True)  -> [ orl s "" other
                                | (yi, di) <- yesInds_difInds
                                , let only_di = IS.toList (ti IS.\\ yi)
                                , let other = map (luLit ind) only_di ] )
        | (c@(C position (positive,ctags)), ind) <- zip conds inds
        , let yesInds_difInds = map luTag (toTags ctags)
        , let cautious = isCareful position ]

--------------------------------------------------------------------------------

constrainBoundaries :: Solver -> WIndSet -> WIndSet -> Word -> IO ()
constrainBoundaries s bdinds nonbdinds word = do
  let bds    = catMaybes $ map (\x -> lookup x word) (IS.toList bdinds)
  let nonbds = catMaybes $ map (\x -> lookup x word) (IS.toList nonbdinds)
  isBd  <- orl' s bds
  notNormal <- andl s "not normal word" (map neg nonbds)
  onlyBd <- andl s "boundary and nothing else" [isBd, notNormal]

  isNormal <- orl' s nonbds
  notBd <- andl s "not boundary" (map neg bds)
  onlyNonBd <- andl s "normal word and nothing else" [isNormal, notBd]

  notBoth <- xorl s [onlyBd, onlyNonBd]
  addClause s [notBoth]
  when False $ do
    putStr $ "constrainBoundaries: trying to solve with following anas both true: "
    print (head bds, head nonbds)
    putStrLn "constrainBoundaries.solve..."
    b <- solve s [head bds, head nonbds]
    putStrLn "constrainBoundaries.solve done"
    if b
       then putStrLn "constrainBoundaries: why can you be both boundary and not boundary"
       else putStrLn "constrainBoundaries: yay there is some sanity left in this world"

--------------------------------------------------------------------------------

mkSentence :: Solver -> Int -> [[Tag]] -> IO Sentence
mkSentence s w tcs = fromList `fmap` sequence 
                       [ ((,) n . fromList) `fmap` sequence 
                         [ (,) m `fmap` newLit s (shTC t n) | (m, t) <- zip [1..] tcs ]
                            | n <- [1..w] ] 
 where shTC ts i = "w" ++ show i ++ (concatMap (\t -> '<':show t++">") ts)

mkTagMap :: [Tag] -> [[Tag]] -> TagMap
mkTagMap ts tcs = fromList $
                    ts `for` \t -> let getInds = IS.fromList . map (1+) . findIndices (elem t)
                                   in (t, getInds tcs) 

--lookupTag :: TagMap -> [WIndex] -> (Trg,Dif) -> ([WIndex],[WIndex])
lookupTag :: TagMap -> WIndSet -> (Trg,Dif) -> (WIndSet,WIndSet)
lookupTag alltags allinds (trg,dif) = 
  let trgInds = if trg==[[]] then allinds --this means IF (-1 (*))
                   else IS.unions $ map (go allinds) trg --trg::[[Tag]]
      difInds = if dif==[[]] then IS.empty --this means IF (-1 something \\ nothing)
                      else IS.unions $ map (go allinds) dif --dif::[[Tag]]
          
  in  (trgInds IS.\\ difInds, difInds)
 where
  go acc []     = acc         --default is [] because intersect [] _ == []
  go acc (t:ts) = let inds = case t of
                              (Rgx r s) -> IS.empty `fromMaybe` lookupRegex t alltags
                              _         -> IS.empty `fromMaybe` lookup t alltags
                  in go (IS.intersection acc inds) ts
                  --in go (intersSorted acc inds) ts
                  --in go (O.isect acc inds) ts
                                

--lookupRegex :: Tag -> TagMap -> Maybe [WIndex]
lookupRegex :: Tag -> TagMap -> Maybe WIndSet
lookupRegex (Rgx r s) tagmap = trace ("lookupRegex: " ++ s) $
  Just $ IS.unions $ elems $ filterWithKey (\t _ -> matchTag r t) tagmap
 where
  matchTag regex (WF str)  = isJust $ matchRegex regex str
  matchTag regex (Lem str) = isJust $ matchRegex regex str
  matchTag regex _         = False
--lookupRegex tag = lookup tag tagmap shouldn't happen anyway


lookupLit :: Sentence -> SIndex -> WIndex -> Lit
lookupLit sentence si wi = 
  case lookup si sentence of
    Just ts -> case lookup wi ts of
                 Just tag -> tag
                 Nothing  -> error "lookupLit: tag combination not found"
    Nothing -> true --If index is out of bounds, we aresent here by negated rule.
                    --`NOT -1000 foo' is always true

--------------------------------------------------------------------------------

intersect1 :: (Eq a, Show a) => [[a]] -> [a]
intersect1 [xs]     = xs
intersect1 (xs:xss) = foldl1 intersect (xs:xss)
intersect1 xs       = error $ "intersect1: expected [[Condition]], got " ++ show xs

deleteInAll :: [(Condition, WIndex)] -> [([Condition], [WIndex])] -> [([Condition], [WIndex])]
deleteInAll cp cps = map (deleteInOne cp) cps

deleteInOne :: [(Condition, WIndex)] -> ([Condition], [WIndex]) -> ([Condition], [WIndex])
deleteInOne delCPs (cs,ps) = unzip [ (c, p) | (c, p) <- zip cs ps
                                            , (c, p) `notElem` delCPs ]

--------------------------------------------------------------------------------

width :: Rule -> (Int,SIndex) --width of rule + index of target
width = fill . toConds . cond

fill :: [[Condition]] -> (Int,SIndex)
fill []   = (1,1)
fill [[]] = (1,1)
fill cs   = (length [minInd..maxInd], 
             1+(fromJust $ elemIndex 0 [minInd..maxInd]))
 where
  minInd = 0 `min` minimum poss
  maxInd = 0 `max` maximum poss
  poss = [ i | c <- concat cs 
              , let i = case c of
                         C pos _ -> posToInt pos
                         Always  -> 0  
                         _       -> error $ "expected condition, got " ++ show cs]

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