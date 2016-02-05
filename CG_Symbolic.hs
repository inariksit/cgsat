module CG_Symbolic where 

import CG_base hiding ( Sentence, showSentence )
import SAT ( Solver(..), newSolver, deleteSolver )
import qualified SAT
import SAT.Named
import AmbiguityClass

import Control.Monad
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace
import Prelude hiding ( Word )
import Text.Regex


--------------------------------------------------------------------------------
--Indices start at 1. More intuitive to talk about w1, w2 vs. w0, w1.
--(Ab)using Data.IntMap because lookup and updates go nicely with builtin functions
type Word     = IM.IntMap Lit
type Sentence = IM.IntMap Word
type TagMap   = M.Map Tag WIndSet
type WIndex   = Int
type SIndex   = Int
type WIndSet  = IS.IntSet
type TrgIS    = IS.IntSet
type DifIS    = IS.IntSet



type TrgInds = (TrgIS,DifIS) --,WhoCares = all \ trg. 
                         -- Would be needed for cautious and onlyTrgLeft.
                         -- Question: is it better to store it or compute it for each rule?
type CondInds = (TrgIS,DifIS)

--like CG_base's Condition, but s/TagSet/CondInds/
data Condition' = C' Position (Bool,[CondInds]) deriving (Show,Eq)

data Rule' = R { trg :: TrgInds --Rule allows disjunction in targets, but Rule' does not. 
                                --We translate one such Rule into [Rule'].
               , cnd :: [[Condition']]
               , isSelect' :: Bool
               , show' :: String
               } deriving (Eq)

instance Show Rule' where
  show = show'

--We call this once for every rule, lookupTag does some expensive intersections and stuff
--Also many rules share targets and conditions--can we save the results?
ruleToRule' :: TagMap -> IS.IntSet -> Rule -> Rule'
ruleToRule' tagmap allinds rule = R trgInds conds isSel nm
 where
  nm = show rule
  lu = lookupTag tagmap allinds
  (trgs,_difs) = unzip $ toTags $ target rule
  --Difs are not important as a target.
  --TagSets include difs, because difs are important with *conditions*.
  --So we can just ignore whatever difs toTags gives us and go with
  trgInds = lu (concat trgs, [[]])
  isSel = isSelect rule
  conds = [  [ C' index (positive, yesInds_noInds)
              | C index (positive, ctags) <- conditions
              , let yesInds_noInds = map lu (toTags ctags) ]
           | conditions <- toConds (cond rule) ]
          
--------------------------------------------------------------------------------
testRule :: Bool -> FilePath -> [[Tag]] -> (Rule', [Rule']) -> IO Bool
testRule verbose ambcls readings (lastrule,rules) = do
  let tagInds = IS.fromList [1..length readings]
  let (w,trgSInd) = width $ cnd lastrule
  s <- newSolver
  initialSentence <- mkSentence s w readings
  afterRules <- foldM (apply s tagInds) initialSentence rules

  ambclasses <- readFile ambcls
  let als  = lines ambclasses
  let xss  = map read als :: [[Int]]
  let form = formula xss 

  mapM_ (constrainStuff s form) (IM.elems afterRules)

  (mustHaveTrg, mustHaveOther, allCondsHold) <- do
    let Just trgSWord = IM.lookup trgSInd afterRules
    let (yesTags,_) = trg lastrule
    let otherTags = tagInds IS.\\ yesTags
    let trgLits   = map (trgSWord IM.!) (IS.toList yesTags)
    let otherLits = map (trgSWord IM.!) (IS.toList otherTags)
    mht <- orl' s trgLits --must have >0 targetLits
    mho <- orl' s otherLits --must have >0 otherLits
    Just condLits <- mkConds s tagInds afterRules trgSInd (cnd lastrule) --we know that all conds are in range: sentence is generated wide enough to ensure that
    ach <- orl' s condLits --conditions must hold
    return (mht, mho, ach)
  let shouldTriggerLast = [mustHaveTrg, mustHaveOther, allCondsHold]
  
  b <- solve s shouldTriggerLast
  if b 
   then do 
      when verbose $ do
           putStrLn $ "Following triggers last rule: " ++ show lastrule
           solveAndPrintSentence False s shouldTriggerLast afterRules
   else do
      putStrLn "Conflict!"
      putStrLn $ "Cannot trigger the last rule: " ++ show lastrule
      when verbose $ do
           putStrLn $ "with the previous rules:"
           mapM_ print rules
  return b

 where 
   constrainStuff s form symbword = do
     let mp ind = fromMaybe true (IM.lookup ind symbword)
     putStrLn $ "constrainStuff: " ++ show symbword
     constraints s mp [] form
  
--------------------------------------------------------------------------------


apply :: Solver -> WIndSet -> Sentence -> Rule' -> IO Sentence
apply s allinds sentence rule = do
  --putStrLn ("apply " ++ show rule)
  let (trgIndsRaw,_) = trg rule --IntSet
  let otherIndsRaw   = allinds IS.\\ trgIndsRaw
  let (trgInds,otherInds) = if isSelect' rule
                              then (otherIndsRaw,trgIndsRaw)
                              else (trgIndsRaw,otherIndsRaw)

  --         :: Sentence -> (SIndex,Word) -> Sentence
  let applyToWord sentence (i,word) = do
       disjConds <- mkConds s allinds sentence i (cnd rule)
       case disjConds of
         Nothing -> return sentence --conditions out of scope, no changes in sentence
         Just cs -> do
           let trgIndsList = (IS.toList trgInds)
           condsHold <- orl' s cs
           let trgPos   = mapMaybe (lu' word) trgIndsList
           let otherNeg = map (neg . lu word) (IS.toList otherInds)
           someTrgIsTrue <- orl' s trgPos
           noOtherIsTrue <- andl' s otherNeg
           onlyTrgLeft <- andl' s [someTrgIsTrue, noOtherIsTrue]
           cannotApply <- orl' s [ neg condsHold, onlyTrgLeft ]
           newTrgLits <- sequence
             --wN<a>' is true if both of the following:
             [ andl s newTrgName [ oldTrgLit     --wN<a> was also true, and
                                 , cannotApply ] --rule cannot apply 
               | oldTrgLit <- trgPos
               , let newTrgName = show oldTrgLit ++ "'" ]
           let newsw = foldl changeAna word (zip trgIndsList newTrgLits)
           return $ changeWord sentence i newsw
           

  foldM applyToWord sentence (IM.assocs sentence)

 where

  changeAna :: Word -> (WIndex,Lit) -> Word
  changeAna word (i,newana) = IM.adjust (const newana) i word

  changeWord :: Sentence -> SIndex -> Word -> Sentence
  changeWord sent i newsw = IM.adjust (const newsw) i sent

  lu' xs x = IM.lookup x xs
  lu  xs x = IM.findWithDefault false x xs -- neg will be called, so false will turn into true. I imagine that this is faster than call map twice?
--------------------------------------------------------------------------------

mkConds :: Solver -> WIndSet -> Sentence -> SIndex -> [[Condition']] -> IO (Maybe [Lit])
mkConds s allinds sentence trgind disjconjconds = do
  let conds_absinds = [ [ (cond, absInd) | cond <- conjconds 
                                         , let absInd = absIndex trgind cond ]
                        | conjconds <- disjconjconds 
                        , all (inRange trgind) conjconds ]
  if null conds_absinds then return Nothing
   else Just `fmap` mapM (mkCond s allinds sentence) conds_absinds
  
 where
  absIndex :: SIndex -> Condition' -> SIndex
  absIndex i (C' pos (b,tags)) = i+posToInt pos

  inRange :: SIndex -> Condition' -> Bool
  inRange i (C' pos (b,ctags)) = not b -- `NOT -100 a' is always true
                                  || IM.member (i+posToInt pos) sentence


mkCond :: Solver -> WIndSet -> Sentence -> [(Condition',SIndex)] -> IO Lit
mkCond s allinds sentence conjconds_absinds = andl' s =<< sequence
 [ do case position of
             (Barrier  foo bar btags) 
               -> do --putStrLn "found a barrier!"
                     addClause s [true] --TODO
                                         
             (CBarrier foo bar btags)
               -> do --putStrLn "found a cbarrier!"
                     addClause s [true] --TODO
             _ -> return ()

      -- disjunction of *tags* in one condition
      orl s (show c ++ " in " ++ show absind) =<< sequence 
        ( case (positive, cautious) of
           (True, False)  -> [ do y <- orl  s "" yesLits
                                  n <- andl s "" (map neg difLits)
                                  andl s "" [y,n]
                                | (yi, di) <- yesInds_noInds --only case where we use noInds!
                                , let yesLits = lookup' yi
                                , let difLits = lookup' di ]
           (True, True)   -> [ do y <- orl  s "" yesLits
                                  n <- andl s "" (map neg otherLits)
                                  andl s "" [y,n]
                                | (yi, _) <- yesInds_noInds
                                , let oi = allinds IS.\\ yi
                                , let yesLits = lookup' yi
                                , let otherLits = lookup' oi ]
           (False, False) -> [ do n <- andl s "" (map neg noLits)
                                  y <- orl s "" otherLits --some lit must be positive
                                  andl s "" [y,n]
                                | (yi, _) <- yesInds_noInds
                                , let oi = allinds IS.\\ yi
                                , let noLits = lookup' yi
                                , let otherLits = lookup' oi ]
           (False, True)  -> [ orl s "" otherLits
                                | (yi, _) <- yesInds_noInds
                                , let oi = allinds IS.\\ yi
                                , let otherLits = lookup' oi ] )
        | (c@(C' position (positive,yesInds_noInds)), absind) <- conjconds_absinds
        , let lookup' is = case IM.lookup absind sentence of
                            Just ts -> mapMaybe (flip IM.lookup $ ts) (IS.toList is)
                            Nothing -> if positive then error "mkCond: index out of bounds"
                                        else [true] --if no -100, then `NOT -100 foo' is true.
        , let cautious = isCareful position ]


--------------------------------------------------------------------------------

mkSentence :: Solver -> Int -> [[Tag]] -> IO Sentence
mkSentence s w tcs = IM.fromList `fmap` sequence 
                       [ ((,) n . IM.fromList) `fmap` sequence 
                         [ (,) m `fmap` newLit s (shTC t n) | (m, t) <- zip [1..] tcs ]
                            | n <- [1..w] ] 
 where shTC ts i = "w" ++ show i ++ (concatMap (\t -> '<':show t++">") ts)

mkTagMap :: [Tag] -> [[Tag]] -> TagMap
mkTagMap ts tcs = M.fromList $
                   ts `for` \t -> let getInds = IS.fromList . map (1+) . findIndices (elem t)
                                  in (t, getInds tcs) 

lookupTag :: TagMap -> WIndSet -> (Trg,Dif) -> (WIndSet,WIndSet)
lookupTag alltags allinds (trg,dif) = 
  let trgInds = if trg==[[]] then allinds --this means IF (-1 (*))
                   else IS.unions $ map (go allinds) trg --trg::[[Tag]]
      difInds = if dif==[[]] then IS.empty --this means IF (-1 something \\ nothing)
                      else IS.unions $ map (go allinds) dif --dif::[[Tag]]
          
  in  (trgInds IS.\\ difInds, difInds)
 where
  go acc []     = acc         --default is empty set, because intersect [] _ == []
  go acc (t:ts) = let inds = case t of
                              (Rgx r s) -> IS.empty `fromMaybe` lookupRegex t alltags
                              _         -> IS.empty `fromMaybe` M.lookup t alltags
                  in go (IS.intersection acc inds) ts
                                
lookupRegex :: Tag -> TagMap -> Maybe WIndSet
lookupRegex (Rgx r s) tagmap = trace ("lookupRegex: " ++ s) $
  Just $ IS.unions $ M.elems $ M.filterWithKey (\t _ -> matchTag r t) tagmap
 where
  matchTag regex (WF str)  = isJust $ matchRegex regex str
  matchTag regex (Lem str) = isJust $ matchRegex regex str
  matchTag regex _         = False
--lookupRegex tag = lookup tag tagmap shouldn't happen anyway


lookupLit :: Sentence -> SIndex -> WIndex -> Lit
lookupLit sentence si wi = 
  case IM.lookup si sentence of
    Just ts -> case IM.lookup wi ts of
                 Just tag -> tag
                 Nothing  -> error "lookupLit: reading not found"
    Nothing -> true --If index is out of bounds, we are sent here by negated rule.
                    --If there is no -100, then `NOT -100 foo' is true.

--------------------------------------------------------------------------------

width :: [[Condition']] -> (Int,SIndex)
width []   = (1,1)
width [[]] = (1,1)
width cs   = (length [minInd..maxInd], 
             1+(fromJust $ elemIndex 0 [minInd..maxInd]))
 where
  minInd = 0 `min` minimum poss
  maxInd = 0 `max` maximum poss
  poss = [ i | C' pos _ <- concat cs 
             , let i = posToInt pos ]

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

getPos :: Condition' -> SIndex
getPos (C' (Barrier  _ i _) _) = i
getPos (C' (CBarrier _ i _) _) = i
getPos (C'  position        _) = posToInt position

for :: (Functor f) => f a -> (a -> b) -> f b
for = flip fmap

singleton :: [a] -> Bool
singleton [x] = True
singleton _   = False

--------------------------------------------------------------------------------

parse :: String -> [Tag]
parse str = maintags ++ concat subtags
 where
  (mainr:subrs) = split (=='+') str
  maintags = map toTag $ filter (not.null) $ split isValid mainr
  subrs_ns = (map FromStart [1..]) `zip` map (split isValid) subrs :: [(Subpos,[String])]
  subtags = map (\(n, strs) -> map (Subreading n . toTag) strs) subrs_ns
  isValid = (=='<') 


  toTag ">>>" = BOS
  toTag "<<<" = EOS
  toTag []    = error "empty tag"
  toTag str = if last str=='>' then Tag (init str) else Lem str

split :: (a -> Bool) -> [a] -> [[a]]
split p [] = []
split p xs = takeWhile (not . p) xs : split p (drop 1 (dropWhile (not . p) xs))

--------------------------------------------------------------------------------

solveAndPrintSentence :: Bool -> Solver -> [Lit] -> Sentence -> IO ()
solveAndPrintSentence verbose s ass sent = do
  b <- solve s ass
  if b then do
          when verbose $ print ass
          vals <- sequence 
                   [ sequence [ modelValue s lit | lit <- IM.elems word ] 
                      | (sind,word) <- IM.assocs sent ]
          let trueAnas =
               [ "\"w" ++ show sind ++ "\"\n"
                  ++ unlines [ "\t"++show ana | (ana, True) <- zip (IM.elems word) vs ]
                 | ((sind,word), vs) <- zip (IM.assocs sent) vals ]
          mapM_ putStrLn trueAnas
          putStrLn "----"
      else do
        putStrLn $ "solveAndPrintSentence: Conflict with assumptions " ++ show ass