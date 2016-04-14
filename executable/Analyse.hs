module Main where

import CG_base hiding ( Sentence, showSentence )
import CG_data ( kimmo_implicit, kimmo_explicit )
import CG_parse
--import CG_Symbolic
import CG_SAT
import SAT ( Solver(..), newSolver, deleteSolver )
import SAT.Named
import AmbiguityClass

import Control.Monad
import Data.Foldable ( asum )
import Data.List
import Data.Maybe
import Debug.Trace
import qualified Data.Map as M
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import System.Environment ( getArgs ) 



--------------------------------------------------------------------------------

data Conflict = NoConf | Internal | With [Rule'] deriving (Show,Eq)

--sometimes we just want to know if there is conflict, not what kind
confToBool :: Conflict -> Bool
confToBool NoConf = False
confToBool _      = True

confToMaybe :: Conflict -> Maybe Conflict
confToMaybe NoConf = Just NoConf
confToMaybe _      = Nothing

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs

  --Dummy examples all work with tags "abcd"
  let tags = map (Tag . (:[])) "abcd"
  let readings = sequence [tags] -- ++ [[Tag "a",Tag "b"],[Tag "a",Tag "c"],[Tag "b",Tag "c"]]
  let tagmap = mkTagMap tags readings
  let allinds = IS.fromList [1..length readings]
  abcd_ambcls <- do ambclauses <- readFile "data/abcd-ambiguity-classes" 
                    let xss  = map read (lines ambclauses) :: [[Int]]
                    return $ formula xss
  let verbose = (True,True)

  case args of 
   ("kimmo":_) -> do

    let kimmo_e' = map (ruleToRule' tagmap allinds) kimmo_explicit
    putStrLn "testing with explicit kimmo"
    mapM_ (testRule verbose abcd_ambcls readings) (splits (reverse kimmo_e'))

    let kimmo_i' = map (ruleToRule' tagmap allinds) kimmo_implicit
    putStrLn "testing with implicit kimmo"
    mapM_ (testRule verbose abcd_ambcls readings) (splits (reverse kimmo_i'))


    let kimmo_i' = map (ruleToRule' tagmap allinds) kimmo_implicit
    putStrLn "testing with implicit kimmo WITHOUT ambiguity classes"
    mapM_ (testRule verbose (formula [[]]) readings) (splits (reverse kimmo_i'))


   (lang:fromStr:toStr:r)-> do 

    let verbose = ("v" `elem` r || "d" `elem` r, "d" `elem` r)

    let subr = if "nosub" `elem` r then ".nosub" else ".withsub"
    let lhack = if "lemmahack" `elem` r then ".lemmahack" else ""
    let rdsfromgrammar = "undersp" `elem` r || "rdsfromgrammar" `elem` r
 
    let dirname = "data/" ++ lang ++ "/" 
    let grfile  = dirname ++ lang ++ ".rlx"
    let tagfile = dirname ++ lang ++ ".tags"
    let rdsfile = dirname ++ lang ++ ".readings" ++ subr ++ lhack
    let acfile  = dirname ++ lang ++ ".ambiguity-classes" ++ lhack
    let frmfile = dirname ++ lang ++ ".formula" ++ lhack
    ambcls <- if "ambcls" `elem` r
                then do ambclauses <- readFile acfile
                        let xss  = map read (lines ambclauses) :: [[Int]]
                        return $ formula xss
                else if "formula"  `elem` r
                       then return $ formula [[]] --read `fmap` readFile frmfile
                       else return $ formula [[]]

    if "writeambcls" `elem` r
      then print ambcls
      else return ()

    let from = read fromStr
    let to   = read toStr

             
--------------------------------------------------------------------------------                           

    tagsInLex <- (concat . map parseReading . filter (not.null) . words) 
                   `fmap` readFile tagfile
    (tsets, rls) <- readRules' grfile
    let readingsInGr = if rdsfromgrammar --OBS. will mess up ambiguity class constraints
                        then nub $ concatMap toTags' tsets
                        else if "underspl" `elem` r
                          then nub $ filter containsLemmaOrWF $ concatMap toTags' tsets
                          else [] 
    print ("readingsInGr length:", length readingsInGr)
    readingsInLex <- (map parseReading . words) `fmap` readFile rdsfile
    print ("readingsInLex length:", length readingsInLex)
    let readings = nub $ readingsInGr ++ readingsInLex  :: [[Tag]]
    let tags = nub $ tagsInLex ++ concat readings

    --print (length readings, length (filter (not.null) readings))
    --mapM_ print tags

    let tagmap = mkTagMap tags readings
    let allinds = IS.fromList [1..length readings]

    let chosenRules = drop from $ take to $ concat (map reverse rls)
    let rulesToTest = splits $ map (ruleToRule' tagmap allinds) chosenRules

    --mapM_ print (zip [1..] rulesToTest)


-------------------------------------------------------------------------------- 

    rs_cs <- catMaybes `fmap` sequence
               [ do c <- testRule verbose ambcls readings rrs
                    if c==NoConf then return Nothing
                      else return $ Just (rrs, c)
                 | rrs <- rulesToTest ]

    let (internalConf,interactionConf) = partition (\(r,c) -> c==Internal) rs_cs
    when (not $ null internalConf) $  do
      putStrLn "\nThe following rules have an internal conflict:"
      mapM_ (print . fst . fst) internalConf


    
    let shrink (r,rs) = do
         putStrLn $ "\n-> " ++ show r ++ " <-"
         case rs of
               [r] -> putStrLn $ "\t" ++ show r
               _  -> do -- Going to be massively slow for >200-rule grammars

                        --find first conflicting rule
                        let allinits = (,) r `map` filter (not.null) (inits rs)
                        conflictsFromStart <- searchInits (testRule verbose ambcls readings) allinits

                        --find last conflicting rule
                        let alltails = (,) r `map` tails conflictsFromStart
                        conflictsFromEnd <- searchTails (testRule verbose ambcls readings) alltails
                        mapM_ (\s -> putStrLn $ "\t" ++ show s) conflictsFromEnd

    when (not $ null interactionConf) $ do
      putStrLn "\nThe following rules conflict with other rule(s):"
      mapM_ ({-shrink .-}print . fst . fst) interactionConf

    when (null rs_cs) $ putStrLn "no conflicts, hurra!"

--}

   _ -> print "usage: cabal analyse <3-letter language name> [v,d]"



 where 
  searchInits :: ((Rule', [Rule']) -> IO Conflict) -> [(Rule', [Rule'])] -> IO [Rule']
  searchInits = binarySearch True

  searchTails :: ((Rule', [Rule']) -> IO Conflict) -> [(Rule', [Rule'])] -> IO [Rule']
  searchTails = binarySearch False

  binarySearch :: Bool -> ((Rule', [Rule']) -> IO Conflict) -> [(Rule', [Rule'])] -> IO [Rule']
  binarySearch _ testFun []   = print "no conflict" >> return []
  binarySearch _ testFun [x]  = do b <- testFun x
                                   if b/=NoConf then return (snd x)
                                    else print "no conflict found" >> return []
  binarySearch _ testFun [x,y] = do b <- testFun x
                                    if b/=NoConf then return (snd x)
                                     else do b' <- testFun y
                                             if b'/=NoConf then return (snd y)
                                              else print "No conflict!" >> return []
  --is Inits
  binarySearch True testFun rrss = 
    do --print ("searchInits: ", length rrss)
       let (hd,tl) = splitAt (length rrss `div` 2) rrss
       b <- testFun (last hd)
       if b==NoConf then binarySearch True testFun tl 
         else binarySearch True testFun hd

  binarySearch False testFun rrss = 
    do --print ("searchTails: ", length rrss)
       let (hd,tl) = splitAt (length rrss `div` 2) rrss
       b <- testFun (head tl)
       if b==NoConf then binarySearch False testFun hd 
         else binarySearch False testFun tl



  splits :: (Eq a) => [a] -> [(a,[a])]
  splits xs = xs `for` \x -> let Just ind = elemIndex x xs
                             in  (x, take ind xs)
 
  for = flip fmap

  toTags' :: TagSet -> [[Tag]]
  toTags' foo = concatMap (nub . (\(a,b) -> if all null b then a else a++b)) . toTags $ foo 

  containsLemmaOrWF :: [Tag] -> Bool
  containsLemmaOrWF []     = False
  containsLemmaOrWF (x:xs) = case x of
                              Lem _ -> True
                              WF _  -> True
                              Subreading
                                _ y -> containsLemmaOrWF (y:xs)
                              _     -> containsLemmaOrWF xs    

--------------------------------------------------------------------------------

testRule :: (Bool,Bool) -> Form -> [[Tag]] -> (Rule', [Rule']) -> IO Conflict
testRule (verbose,debug) form rds (lastrule,rules) = do

  let allwidths@((firstW,firstTrg):otherwidths) = width $ cnd lastrule
  --print allwidths
  
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
                       [ confToMaybe `fmap` testRule' debug form rds (lastrule,rules) w
                        | w <- nub otherwidths ]
                     return $ fromMaybe resFst someLengthWorks   



testRule' :: Bool -> Form -> [[Tag]] -> (Rule', [Rule']) 
          -> (Int,SIndex) -> IO Conflict
testRule' debug form readings (lastrule,rules) (w,trgSInd) = do
  --print (w,trgSInd)

  let allInds = IS.fromList [1..length readings]

  s <- newSolver
  initialSentence <- symbSentence s readings w
  defaultRules s initialSentence

  afterRules <- foldM (apply s) initialSentence rules

--  defaultRules s afterRules

  let shouldTriggerLast s sentence = do
        let trgSWord = fromMaybe (error "shouldTriggerLast: no trg index found") (IM.lookup trgSInd sentence)
        let (trgIndsRaw,_difIndsRaw) = trg lastrule -- :: IntSet
                                                    -- we don't care about difs here anymore.
                                                    -- mkCond needs to still have access to difInds,
                                                    -- that's why the type includes it.
        let otherIndsRaw   = allInds IS.\\ trgIndsRaw

        let (trgInds,otherInds) = if isSelect' lastrule
                                   then (otherIndsRaw,trgIndsRaw)
                                   else (trgIndsRaw,otherIndsRaw)

        let trgLits   = map (trgSWord IM.!) (IS.toList trgInds)
        let otherLits = map (trgSWord IM.!) (IS.toList otherInds)

        mht <- orl' s trgLits -- 1) must have ≥1 targetLits
        mho <- orl' s otherLits -- 2) must have ≥1 otherLits 

        let alternative = error $ "shouldTriggerLast: no cnd index found for rule\n\t" ++ 
                                   show lastrule ++ "\n, sentence width " ++ show w ++ 
                                   ", target index " ++ show trgSInd
        --let alternative = [true]

        --TODO: investigate
        --symbConds doesn't give Nothing, but claims almost every rule has internal conflict.
        --mkConds doesn't claim that, but sometimes gives Nothing for totally reasonable rules.
        cl <- fromMaybe alternative `fmap` symbConds s sentence trgSInd (cnd lastrule)    

        ach <- orl' s cl -- 3) conditions must hold

        return (mht, mho, ach)


  (mustHaveTrg, mustHaveOther, allCondsHold) <- shouldTriggerLast s afterRules

  b <- solve s [mustHaveTrg, mustHaveOther, allCondsHold]
  if b 
    then do 
      when debug $ do
        putStrLn $ "Following triggers last rule WITH PREVIOUS: " ++ show lastrule
        solveAndPrintSentence False s [mustHaveTrg, mustHaveOther, allCondsHold] afterRules
      deleteSolver s
      return NoConf

    else do
      when debug $ do 
      --when False $ do 
        putStrLn "-------"
        putStrLn $ "Rule " ++ show lastrule ++":"
        putStrLn "could not solve with previous, trying to loosen requirements:"
        putStr "a) "
        solveAndPrintSentence False s [mustHaveTrg, mustHaveOther] afterRules
        putStr "b) "
        solveAndPrintSentence False s [mustHaveTrg, allCondsHold] afterRules
        putStr "c) "
        solveAndPrintSentence False s [mustHaveOther, allCondsHold] afterRules
        
        putStrLn "...and now trying to solve the same rule with a fresh sentence:"

      s' <- newSolver
      initialSentence' <- symbSentence s' readings w
      defaultRules s' initialSentence'
      (mustHaveTrg', mustHaveOther', allCondsHold') <- shouldTriggerLast s' initialSentence'
      b' <- solve s' [mustHaveTrg', mustHaveOther', allCondsHold']
      when (debug && b') $ do
        putStrLn $ "Following triggers last rule ALONE: " ++ show lastrule
        solveAndPrintSentence False s' [mustHaveTrg', mustHaveOther', allCondsHold'] initialSentence'
      deleteSolver s'
      deleteSolver s
      if b' then return $ With rules
              else return Internal
              

 where
  defaultRules s sentence = 
   sequence_ [ do addClause s lits          --Every word must have >=1 reading
                  constraints s mp [] form  --Constraints based on lexicon
               | word <- IM.elems sentence 
               , let lits = IM.elems word 
               , let mp i = fromMaybe (error $ "constraints: " ++ show i) (IM.lookup i word) ] 

--------------------------------------------------------------------------------

width :: [[Condition']] -> [(Int,SIndex)]
width []   = [(1,1)]
width [[]] = [(1,1)]
width cs   = [ (len, tind) | (mi,ma) <- mins_maxs 
                           , let len = length [mi..ma]
                           , let tind = maybe 99999 (1+) (elemIndex 0 [mi..ma]) ]
 where
  mins_maxs = [ (0 `min` minimum pos, 0 `max` maximum pos) | pos <- poss ]
  poss = sequence $ map getPos (concat cs)

  posToInts :: Position' -> [Int]
  posToInts (Exactly' _ i) = [i]
  posToInts (AtLeast' _ i) = if i<0 then [i,i-1,i-2,i-3] else [i,i+1,i+2,i+3]
  posToInts (Barrier' _ _ i _)  = if i<0 then [i,i-1,i-2] else [i,i+1,i+2]
  --TODO fix the LINK case
  posToInts (LINK' parent child _) = --trace (show (posToInts parent) ++ ", " ++ show child) $
                               [ pI + cI | (pI,cI) <- posToInts parent 
                                          `zip` (cycle $ posToInts child) ]

  getPos :: Condition' -> [Int]
  getPos Always'    = [1]
  getPos (C' pos _) = posToInts pos