module CG_Symbolic where 

import CG_base hiding ( Sentence, showSentence )
import SAT ( Solver(..), newSolver, deleteSolver )
import qualified SAT
import SAT.Named
import AmbiguityClass

import Control.Monad
import Data.Foldable ( asum )
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
data Condition' = C' Position (Bool,[CondInds]) | Always' deriving (Show,Eq)


data Conflict = NoConf | Internal | With [Rule'] deriving (Show,Eq)

--sometimes we just want to know if there is conflict, not what kind
confToBool :: Conflict -> Bool
confToBool NoConf = False
confToBool _      = True

confToMaybe :: Conflict -> Maybe Conflict
confToMaybe NoConf = Just NoConf
confToMaybe _      = Nothing


--------------------------------------------------------------------------------


data Rule' = R { trg :: TrgInds 
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
  conds = (map.map) condToCond' (toConds $ cond rule)
  --conds = [  [ C' index (positive, yesInds_noInds)
  --            | C index (positive, ctags) <- conditions
  --            , let yesInds_noInds = map lu (toTags ctags) ]
  --         | conditions <- toConds (cond rule) ]

  condToCond' (C index (positive, ctags)) = 
     let yesInds_noInds = map lu (toTags ctags) 
     in  C' index (positive, yesInds_noInds)
  condToCond' Always = Always' --C' (Exactly False 0) (True, [])
          
--------------------------------------------------------------------------------
testRule :: (Bool,Bool) -> Form -> [[Tag]] -> (Rule', [Rule']) -> IO Conflict
testRule (verbose,debug) form rds (lastrule,rules) = do

  let allwidths@((firstW,firstTrg):otherwidths) = width $ cnd lastrule
  
  --if the shortest reading conflicts, we want to keep that result
  resFst <- testRule' debug form rds (lastrule,rules) (firstW,firstTrg)

  --if any of the other lengths is fine, we keep that
  --if all the longer lengths conflict, we just return the first
  when debug $ putStrLn ("first result for " ++ show lastrule ++ ": " ++ show resFst)
  case (resFst,otherwidths) of
    (NoConf,_) -> return resFst
    (_,    []) -> return resFst
    (_,     _) -> do when debug $ do
                        putStrLn $ "rule with *, trying many combinations"
                     someLengthWorks <- asum `fmap` sequence 
                       [ confToMaybe `fmap` testRule' False form rds (lastrule,rules) w
                        | w <- otherwidths ]
                     return $ fromMaybe resFst someLengthWorks   



testRule' :: Bool -> Form -> [[Tag]] -> (Rule', [Rule']) 
          -> (Int,SIndex) -> IO Conflict
testRule' debug form readings (lastrule,rules) (w,trgSInd) = do
  --print (w,trgSInd)

  let tagInds = IS.fromList [1..length readings]


  s <- newSolver
  initialSentence <- mkSentence s w readings
  afterRules <- foldM (apply s tagInds) initialSentence rules


  defaultRules s afterRules


  let shouldTriggerLast s sentence = do
        let trgSWord = fromMaybe (error "shouldTriggerLast: no trg index found") (IM.lookup trgSInd sentence)
        let (yesTags,_) = trg lastrule
        let otherTags = tagInds IS.\\ yesTags
        let trgLits   = map (trgSWord IM.!) (IS.toList yesTags)
        let otherLits = map (trgSWord IM.!) (IS.toList otherTags)
        mht <- orl' s trgLits --must have >0 targetLits
        mho <- orl' s otherLits --must have >0 otherLits 
       --we know that all conds are in range: sentence is generated wide enough to ensure that
        cl <- liftM2 fromMaybe (return []) --(error $ "shouldTriggerLast: no cnd index found for rule\n\t" ++ 
        --                                show lastrule ++ "\n, sentence width " ++ show w ++ 
        --                                ", target index " ++ show trgSInd) 
                               (mkConds s tagInds sentence trgSInd (cnd lastrule) "testRule")
        ach <- orl' s cl --conditions must hold
        --print cl
        return (mht, mho, ach)

  (mustHaveTrg, mustHaveOther, allCondsHold) <- shouldTriggerLast s afterRules
  
  b <- solve s [mustHaveTrg, mustHaveOther, allCondsHold]
  if b 
    then do 
      when debug $ do
        putStrLn $ "Following triggers last rule WITH PREVIOUS: " ++ show lastrule
        solveAndPrintSentence False s [mustHaveTrg, mustHaveOther, allCondsHold] afterRules
      return NoConf

    else do
      when debug $ do 
        putStrLn "-------"
        putStrLn $ "Rule " ++ show lastrule ++":"
        putStrLn "could not solve with previous, trying to loosen requirements:"
        solveAndPrintSentence False s [mustHaveTrg, mustHaveOther] afterRules
        solveAndPrintSentence False s [mustHaveTrg, allCondsHold] afterRules
        solveAndPrintSentence False s [mustHaveOther, allCondsHold] afterRules
        
        putStrLn "...and now trying to solve the same rule with a fresh sentence:"

      s' <- newSolver
      initialSentence' <- mkSentence s' w readings
      defaultRules s' initialSentence'
      (mustHaveTrg', mustHaveOther', allCondsHold') <- shouldTriggerLast s' initialSentence'
      b' <- solve s' [mustHaveTrg', mustHaveOther', allCondsHold']
      when (debug && b') $ do
        putStrLn $ "Following triggers last rule ALONE: " ++ show lastrule
        solveAndPrintSentence False s' [mustHaveTrg', mustHaveOther', allCondsHold'] initialSentence'
      if b' then return $ With rules
              else return Internal

 where
  defaultRules s sentence = 
   sequence_ [ do addClause s lits          --Every word must have >=1 reading
                  --constraints s mp [] form  --Constraints based on lexicon
               | word <- IM.elems sentence 
               , let lits = IM.elems word 
               , let mp i = fromMaybe true (IM.lookup i word) ] --TODO why is true default?

--------------------------------------------------------------------------------


apply :: Solver -> WIndSet -> Sentence -> Rule' -> IO Sentence
apply s allinds sentence rule = do
  --putStrLn ("apply " ++ show rule)
  let (trgIndsRaw,_) = trg rule -- :: IntSet
  let otherIndsRaw   = allinds IS.\\ trgIndsRaw
  let (trgInds,otherInds) = if isSelect' rule
                              then (otherIndsRaw,trgIndsRaw)
                              else (trgIndsRaw,otherIndsRaw)

  --         :: Sentence -> (SIndex,Word) -> Sentence
  let applyToWord sentence (i,word) = do
       disjConds <- mkConds s allinds sentence i (cnd rule) "apply"
       case disjConds of
         Nothing -> return sentence --conditions out of scope, no changes in sentence
         Just cs -> do
           let trgIndsList = IS.toList trgInds
           condsHold <- orl' s cs
           let trgPos   = mapMaybe   (lu' word) trgIndsList
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

--OBS. it's also perfectly fine to have an empty condition, ie. remove/select always!
mkConds :: Solver -> WIndSet -> Sentence -> SIndex -> [[Condition']] -> String -> IO (Maybe [Lit])
mkConds s allinds sentence trgind disjconjconds str = do
  -- * conds_absinds = all possible (absolute, not relative) SIndices in range 
  -- * call mkCond with arguments of type (Condition, [SIndex])
  -- * in mkCond, sometimes we can get away with a big orl that contains literals
  --    from different symbolic words, but with cautious or negation, can't do that

  let conds_absinds = [ [ (cond, absinds) | cond <- nub conjconds 
                                          , let absinds = absIndices trgind cond 
                                          , not (null absinds) ] --TODO test with NOT!!!!
                        | conjconds <- disjconjconds ]
  when False $ do
    putStrLn $ "conds_absinds (calling from " ++ str ++ "):"
    print conds_absinds
    mapM_ (mapM_ print) conds_absinds
    putStrLn "-----"
  if null conds_absinds || all null conds_absinds
   then return Nothing --[true] --is there a meaningful difference between Nothing and []?
   else Just `fmap` mapM (mkCond s allinds sentence str) conds_absinds
  
 where
  absIndices :: SIndex -> Condition' -> [SIndex]
  absIndices i (C' pos (b,tags)) = let possibleInds = (i+) <$> posToInt pos
                                       member = flip IM.member
                                   in filter (member sentence) possibleInds
  absIndices i Always'           = [i]


mkCond :: Solver -> WIndSet -> Sentence -> String -> [(Condition',[SIndex])] -> IO Lit
mkCond s allinds sentence str conjconds_absinds = do 
  andl' s =<< sequence [ orl s "" =<< sequence
                         [ go cond absind --by absInd
                           | (cond,absinds) <- conjconds_absinds 
                           , absind <- absinds ] ]
--  lits_by_condition

 where
  lookup' :: Bool -> SIndex -> WIndSet -> [Lit]
  lookup' b ai is = case IM.lookup ai sentence of
                      Nothing -> if b then error "mkCond: index out of bounds"
                                  else [true] --if no -100, then `NOT -100 foo' is true
                      Just wd -> mapMaybe (\i -> IM.lookup i wd) (IS.toList is)

  go :: Condition' -> SIndex -> IO Lit 
  go Always' _  = return true
  go c@(C' position (positive,yesInds_difInds)) absind = do 

    --OBS. this IS.unions is for *tag disjunction*,
    --we keep literals from different symbolic words separate,
    --this function only touches stuff in one word
    let yi_ALLINONE = IS.unions $ fst $ unzip yesInds_difInds
    let oi_ALLINONE = allinds IS.\\ yi_ALLINONE

    let lu = lookup' positive absind

    case position of
      (Barrier  foo bar btags) 
        -> do --putStrLn "found a barrier!"
              addClause s [true] --TODO
                                         
      (CBarrier foo bar btags)
        -> do --putStrLn "found a cbarrier!"
              addClause s [true] --TODO
      _ -> return ()

    case (positive, isCareful position) of
      (True, False) -> orl s "" =<< sequence
                          [ do let yesLits = lu yi
                               let difLits = lu di
                               y <- orl  s "" yesLits
                               n <- andl s "" (map neg difLits)
                               andl s "" [y,n]
                            | (yi, di) <- yesInds_difInds ] --foreach TAG COMBINATION!
                        --only case where we really need difInds!
                           
      (True, True)  -> do let yesLits = lu yi_ALLINONE
                          let otherLits = lu oi_ALLINONE
                          y <- orl  s "" yesLits
                          n <- andl s "" (map neg otherLits)
                          andl s "" [y,n] 

                       --OBS. "yesInds" mean "noInds" for NOT
      (False,False) -> do let noLits = lu yi_ALLINONE
                          --let otherLits = lu oi_ALLINONE
                          andl s "" (map neg noLits)

      (False, True) -> do let otherLits = lu oi_ALLINONE
                          andl s "" otherLits


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

--each condition comes with a *list* of positions
--each list must have a corresponding trgInd place
--example: -1*,1,2* gives widths from 4 (-1 --> 2) to 8 (-3 --> 4)
--lists returned in poss : [-1,-2,-3],[1],[2,3,4]
--one thing must come from one list -> sequence into 
--   [[-1,1,2],[-1,1,3], ... [-3,1,3],[-3,1,4]]
--then get the min and max in each such list:
--   [(-1,2),  (-1,3),   ... (-3,3),  (-3,4)]
--and then just find the position of 0 in each, that must be the trgInd
width :: [[Condition']] -> [(Int,SIndex)]
width []   = [(1,1)]
width [[]] = [(1,1)]
width cs   = [ (len, tind) | (mi,ma) <- mins_maxs 
                           , let len = length [mi..ma]
                           , let tind = maybe 99999 (1+) (elemIndex 0 [mi..ma]) ]
 where
  mins_maxs = [ (0 `min` minimum pos, 0 `max` maximum pos) | pos <- poss ]
  poss = sequence $ map getPos (concat cs)

--for (C)BARRIER, count an extra place to place the barrier tag
posToInt :: Position -> [Int]
posToInt (Exactly _ i) = [i]
posToInt (AtLeast _ i) = case (i<0) of 
                          True  -> [i,i-1,i-2] 
                          False -> [i,i+1,i+2]
posToInt (Barrier _ i _)  = if i<0 then [i-1,i-2,i-3] else [i+1,i+2,i+3]
posToInt (CBarrier _ i _) = if i<0 then [i-1,i-2,i-3] else [i+1,i+2,i+3]

isCareful :: Position -> Bool
isCareful (Exactly b _) = b
isCareful (AtLeast b _) = b
isCareful (Barrier b _ _) = b
isCareful (CBarrier b _ _) = b

getPos :: Condition' -> [Int]
getPos Always'    = [1]
getPos (C' pos _) = posToInt pos


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