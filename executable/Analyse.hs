module Main where

import CG_base hiding ( Sentence, showSentence )
import CG_parse
import CG_Symbolic
import SAT ( Solver(..), newSolver, deleteSolver )
import SAT.Named
import AmbiguityClass

import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.IntSet as IS
import System.Environment ( getArgs ) 

ex_abc1 = concat $ snd $ parseRules False
     ( "REMOVE:r1 (a) IF (-1 (c)) ; " ++
       "REMOVE:l  (a) IF (-1C  (c)) ; ")

ex_abc2 = concat $ snd $ parseRules False
     ( "REMOVE:r1 (a) IF (-1 (*) - (b)) ;" ++
       "REMOVE:r2 (b) IF ( 1 (a)) ;" ++
       "REMOVE:r3 (a) IF (-1 (b)) ;" ++
       "REMOVE:l  (a) IF (-1 (c)) ;" )

ex_tricky1 = concat $ snd $ parseRules False
     ( "REMOVE:r1 (a) IF (-1C (b)) ;" ++
       "REMOVE:r2 (b) IF ( 1 (a)) ;" ++    --should not remove
       "REMOVE:l  (a) IF (-1 (b)) ;" )

ex_tricky2 = concat $ snd $ parseRules False
     ( "REMOVE:r1 (a) IF (-1C (b)) ;" ++
       "SELECT:r2 (b) IF ( 1 (a)) ;" ++    --should select
       "REMOVE:l  (a) IF (-1 (b)) ;" )

ex_not = concat $ snd $ parseRules False
     ( "REMOVE:r1 (a) IF (NOT -1 (c)) ;" ++
       "REMOVE:r2 (a) IF (-1 (a)) ;" ++
       "REMOVE:l  (b) IF (-1 (c)) ;" )

kimmo_explicit = concat $ snd $ parseRules False
     ( "REMOVE:r1 (a) IF (0 (b)) (-1 (c)) ;" ++
       "REMOVE:r2 (b) IF (0 (a)) (-1 (c)) ;" ) -- ++
   --    "REMOVE:l  (c) IF (-1 (c)) ;" )

kimmo_implicit = concat $ snd $ parseRules False
     ( "REMOVE:r1 (a) IF  (-1 (c)) ;" ++
       "REMOVE:r2 (b) IF  (-1 (c)) ;" ) 
--       "REMOVE:l  (c) IF (-1 (c)) ;" )


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
    ambcls <- if "ambcls" `elem` r
                then do let acfile = dirname ++ lang ++ ".ambiguity-classes" ++ lhack
                        ambclauses <- readFile acfile
                        let xss  = map read (lines ambclauses) :: [[Int]]
                        return $ formula xss
                  else return $ formula [[]]

    let from = read fromStr
    let to   = read toStr

             
--------------------------------------------------------------------------------                           

    tagsInLex <- (concat . map parse . filter (not.null) . words) 
                   `fmap` readFile tagfile
    (tsets, rls) <- readRules' grfile
    let readingsInGr = if rdsfromgrammar --OBS. will mess up ambiguity class constraints
                        then nub $ concatMap toTags' tsets
                        else if "underspl" `elem` r
                          then nub $ filter containsLemmaOrWF $ concatMap toTags' tsets
                          else [] 
    --print ("readingsInGr length:", length readingsInGr)
    readingsInLex <- (map parse . words) `fmap` readFile rdsfile
    --print ("readingsInLex length:", length readingsInLex)
    let readings = nub $ readingsInGr ++ readingsInLex  :: [[Tag]]
    let tags = nub $ tagsInLex ++ concat readings

    --print (length readings, length (filter (not.null) readings))
    --mapM_ print tags

    let tagmap = mkTagMap tags readings
    let allinds = IS.fromList [1..length readings]

    let chosenRules = drop from $ take to $ concat (map reverse rls)
    let rulesToTest = splits $ map (ruleToRule' tagmap allinds) chosenRules

    --print rulesToTest


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
               _  -> do -- This is not going to work outside small grammars
                        let allinits = (,) r `map` filter (not.null) (inits rs)
                        brs <- searchInits (testRule verbose ambcls readings) allinits
                        let minimalConflict = brs --fromMaybe rs brs
                        --let moreMinimalConflict = []
                        let alltails = (,) r `map` tails minimalConflict
                        moreMinimalConflict <- searchTails (testRule verbose ambcls readings) alltails
                        mapM_ (\s -> putStrLn $ "\t" ++ show s) moreMinimalConflict

    when (not $ null interactionConf) $ do
      putStrLn "\nThe following rules conflict with other rule(s):"
      mapM_ (shrink . fst) interactionConf

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
  toTags' = concatMap (nub . (\(a,b) -> if all null b then a else a++b)) . toTags

  containsLemmaOrWF :: [Tag] -> Bool
  containsLemmaOrWF []     = False
  containsLemmaOrWF (x:xs) = case x of
                              Lem _ -> True
                              WF _  -> True
                              Subreading
                                _ y -> containsLemmaOrWF (y:xs)
                              _     -> containsLemmaOrWF xs    


