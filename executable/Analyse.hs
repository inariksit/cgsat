module Main where

import CG_base hiding ( Sentence, showSentence )
import CG_parse
import CG_Symbolic
import SAT ( Solver(..), newSolver, deleteSolver )
import SAT.Named

import Control.Monad
import Data.List
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
       "REMOVE:r2 (b) IF (0 (a)) (-1 (c)) ;" ++
       "REMOVE:l  (c) IF (-1 (c)) ;" )

kimmo_implicit = concat $ snd $ parseRules False
     ( "REMOVE:r1 (a) IF  (-1 (c)) ;" ++
       "REMOVE:r2 (b) IF  (-1 (c)) ;" ++
       "REMOVE:l  (c) IF (-1 (c)) ;" )


main = do
  args <- getArgs

  --Dummy examples all work with tags "abcd"
  let tags = map (Tag . (:[])) "abcd"
  let readings = sequence [tags] -- ++ [[Tag "a",Tag "b"],[Tag "a",Tag "c"],[Tag "b",Tag "c"]]
  let tagmap = mkTagMap tags readings
  let allinds = IS.fromList [1..length readings]
  let abcd_ambcls = "data/abcd-ambiguity-classes" 
  let verbose = (True,True)
  case args of 
    ("kimmo":_)
       -> do let kimmo_i' = map (ruleToRule' tagmap allinds) kimmo_implicit
             putStrLn "testing with implicit kimmo"
             mapM_ (testRule verbose abcd_ambcls readings) (splits (reverse kimmo_i'))

             let kimmo_e' = map (ruleToRule' tagmap allinds) kimmo_explicit
             putStrLn "testing with explicit kimmo"
             mapM_ (testRule verbose abcd_ambcls readings) (splits (reverse kimmo_e'))

    (lang:fromStr:toStr:r)
       -> do let verbose = ("v" `elem` r || "d" `elem` r, "d" `elem` r)

             let subr = if "nosub" `elem` r then "nosub" else "withsub"
             let withambcls = "ambcls" `elem` r
             let withunders = "undersp" `elem` r

             let dirname = "data/" ++ lang ++ "/" 
             let grfile  = dirname ++ lang ++ ".rlx"
             let tagfile = dirname ++ lang ++ ".tags"
             let rdsfile = dirname ++ lang ++ ".readings." ++ subr
             let ambcls  = if withambcls then dirname ++ lang ++ "-ambiguity-classes"
                            else "data/dummy-amb-cls" 

             let from = read fromStr
             let to   = read toStr
                           

             tagsInApe <- (concat . filter (not.null) . map parse . words) 
                         `fmap` readFile tagfile
             (tsets, rls) <- readRules' grfile
             let readingsInGr = if withunders then nub $ concatMap toTags' tsets
                            else [] 
             readingsInLex <- (map parse . words) `fmap` readFile rdsfile
             let readings = nub $ readingsInGr ++ readingsInLex  :: [[Tag]]
             let tags = nub $ tagsInApe ++ concat readings

             --mapM_ print readings
             --mapM_ print tags

             let tagmap = mkTagMap tags readings
             let allinds = IS.fromList [1..length readings]
             let rules = map (ruleToRule' tagmap allinds) (concat (map reverse rls))


             badrules <- filterM (testRule verbose ambcls readings) (drop from $ take to $ splits rules)

             putStrLn "\nThe following rules have an internal conflict:"
             rs_bsAlone <- badrules `forM` \rrs@(r,_) -> do b <- testRule (False,False) ambcls readings (r,[]) 
                                                            return (rrs, b)

             let (internalConf,interactionConf) = partition (\(r,b) -> b) rs_bsAlone
             mapM_ (print . fst . fst) internalConf


             putStrLn "\nFinding out the reason for conflict for the rest:"
             let shrink (r,rs) = do -- This is not going to work outside small grammars
                                    let allinits = (,) r `map` inits rs
                                    brs <- searchInits (testRule verbose ambcls readings) allinits
                                    let minimalConflict = case brs of 
                                                            [] -> rs --[last rs]
                                                            mc -> mc 
                                    --let moreMinimalConflict = []
                                    let alltails = (,) r `map` tails minimalConflict
                                    moreMinimalConflict <- searchTails (testRule verbose ambcls readings) alltails
                                    putStrLn $ "\n-> " ++ show r ++ " <-"
                                    mapM_ (\s -> putStrLn $ "\t" ++ show s) moreMinimalConflict

             mapM_ (shrink . fst) interactionConf




    _ -> print "usage: cabal analyse <3-letter language name> [v,d]"



 where 
  searchInits :: ((Rule', [Rule']) -> IO Bool) -> [(Rule', [Rule'])] -> IO [Rule']
  searchInits = binarySearch True

  searchTails :: ((Rule', [Rule']) -> IO Bool) -> [(Rule', [Rule'])] -> IO [Rule']
  searchTails = binarySearch False

  binarySearch :: Bool -> ((Rule', [Rule']) -> IO Bool) -> [(Rule', [Rule'])] -> IO [Rule']
  binarySearch _ testFun []   = print "no conflict" >> return []
  binarySearch _ testFun [x]  = do --print "binarySearch: 1"
                                   b <- testFun x
                                   if b then return (snd x)
                                    else print "no conflict found" >> return []
  binarySearch _ testFun [x,y] = do --print "binarySearch: 2"
                                    b <- testFun x
                                    if b then return (snd x)
                                     else do b' <- testFun y
                                             if b' then return (snd y)
                                              else print "No conflict!" >> return []
  --is Inits
  binarySearch True testFun rrss = 
    do --print ("searchInits: ", length rrss)
       let (hd,tl) = splitAt (length rrss `div` 2) rrss
       b <- testFun (last hd)
       if not b then binarySearch True testFun tl 
         else binarySearch True testFun hd

  binarySearch False testFun rrss = 
    do --print ("searchTails: ", length rrss)
       let (hd,tl) = splitAt (length rrss `div` 2) rrss
       b <- testFun (head tl)
       if not b then binarySearch False testFun hd 
         else binarySearch False testFun tl



  splits :: (Eq a) => [a] -> [(a,[a])]
  splits xs = xs `for` \x -> let Just ind = elemIndex x xs
                             in  (x, take ind xs)
 
  for = flip fmap

  toTags' :: TagSet -> [[Tag]]
  toTags' = concatMap (nub . (\(a,b) -> if all null b then a else a++b)) . toTags


