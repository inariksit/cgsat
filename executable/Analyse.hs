module Main where

import CG_base hiding ( Sentence, showSentence )
import CG_parse
import CG_Symbolic
import SAT ( Solver(..), newSolver, deleteSolver )
import SAT.Named

import Control.Monad
import Data.List
import qualified Data.Map as M
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

kimmo = concat $ snd $ parseRules False
     ( "REMOVE:r1 (a) IF (0 (b)) (-1 (c)) ;" ++
       "REMOVE:r2 (b) IF (0 (a)) (-1 (c)) ;" ++
       "REMOVE:l  (c) IF (-1 (c)) ;" )

main = do
  args <- getArgs
  let ts = map (Tag . (:[])) "abcd"
  let tc = sequence [ts] -- ++ [[Tag "a",Tag "b"],[Tag "a",Tag "c"],[Tag "b",Tag "c"]]
  case args of 
    [] -> do let tricky1 = last $ splits ex_tricky1
             let tricky2 = last $ splits ex_tricky2
             mapM_ (testRule (True,True) ts tc) [tricky1, tricky2]
    ("kimmo":_)
       -> do mapM_ (testRule (True,True) ts tc) (splits (reverse kimmo))

{- TODO update for IntSets
    ("play":gr:tagcombs:r)
       -> do (tsets, rls) <- readRules' gr 
             tcInLex <- (map parse . words) `fmap` readFile tagcombs
             let allConds = concatMap (toConds . cond) (concat rls)
             let unnamedTags = nub $ concatMap (map getTagset) allConds :: [TagSet]
             let tcInGr = nub $ concatMap toTags' $ tsets ++ unnamedTags
             let tc = nub $ tcInGr ++ tcInLex :: [[Tag]]
             let ts = concat tc

             s <- newSolver
             putStrLn "Give sentence length"
             slen <- readLn :: IO Int
             initialSent <- mkSentence s slen tc
             let tagmap = mkTagMap ts tc
             let taginds = [1..length tc]
             finalSent <- foldM (apply s tagmap taginds) initialSent (concat rls)
             M.toAscList finalSent `forM_`  \(sInd,_) -> do
               putStrLn $ "Do you want to decide the POS of w" ++ show sInd ++ "? y/n"
               yn <- getLine
               when ('y' `elem` yn) $ do
                   putStrLn "Which POS?"
                   newPos <- Tag `fmap` getLine
                   case M.lookup newPos tagmap of
                    Nothing -> putStrLn "Not a valid tag"
                    Just wInds 
                      -> do let tls = map (\wi -> lookupLit finalSent sInd wi) wInds
                            addClause s tls --at least one of them is true

             constrainBoundaries s tagmap `mapM_` M.elems finalSent
             solveAndPrintSentence True s [] finalSent
-}
             
    ("fin":r)
       -> do let verbose = "v" `elem` r || "d" `elem` r
             let debug = "d" `elem` r
             (tsets, rls) <- readRules' "data/fin.rlx"
             --tcInLex <- (map parse . words) `fmap` readFile "data/fin-tagcombs.txt"
             let rules = concat (map reverse rls)
             let allConds = concatMap (toConds . cond) rules
             let unnamedTags = nub $ concatMap (map getTagset) allConds
             let tcInGr = nub $ concatMap toTags' $ tsets ++ unnamedTags
             let tc = tcInGr -- ++ tcInLex
             let ts = concat tc
             print (length tc)
             -- print ("tag combinations from the grammar:", length (nub tsets))
             -- print ("tag combinations from the lexicon:", length tsets)
             --testRules (verbose,debug) ts tc rules
             --mapM_ print ( (splits rules))
             --mapM_ (testRule (verbose,debug) ts tc) (splits rules)
             testRule (verbose,debug) ts tc (last $ splits rules)
             putStrLn "end"

    ("nld":r)
       -> do let verbose = "v" `elem` r || "d" `elem` r
             let debug = "d" `elem` r
             tsInApe <- (concat . filter (not.null) . map parse . words) 
                         `fmap` readFile "data/nld/nld_tags.txt"
             (tsets, rls) <- readRules' "data/nld/nld.rlx"
             let rules = concat (map reverse rls)
             let tcInGr = nub $ concatMap toTags' tsets
             tcInLex <- (map parse . words) `fmap` readFile "data/nld/nld_tagcombs.txt"
             let tc = nub $ tcInGr ++ tcInLex  :: [[Tag]]
             let ts = nub $ tsInApe ++ concat tc
             --testRules (verbose,debug) ts tc (reverse rules)
             --testRule (verbose,debug) ts tc (last $ splits rules)
             mapM_ (testRule (verbose,debug) ts tc) (splits rules)


    ("spa":r)
       -> do let verbose = "v" `elem` r || "d" `elem` r
             let debug = "d" `elem` r
             let strict_tags = "only-named" `elem` r || "strict-tags" `elem` r
             tsInApe <- (concat . filter (not.null) . map parse . words) 
                         `fmap` readFile "data/spa/spa_tags.txt"
             (tsets, rls) <- readRules' "data/spa/apertium-spa.spa.rlx"
             let rules = concat (map reverse rls)
             let allConds = concatMap (toConds . cond) rules
             let unnamedTags = nub $ concatMap (map getTagset) allConds
             -- mapM_ print allConds 
             -- mapM_ print unnamedTags
             let tcInGr = nub $ if strict_tags 
                                 then map toTags' tsets
                                 else map toTags' tsets ++ map toTags' unnamedTags
             tcInLex <- (map parse . words) `fmap` readFile "data/spa/spa_tagcombs.txt"
             let tc = nub $ (concat tcInGr) ++ tcInLex 
             let ts = nub $ tsInApe ++ concat tc 
--             testRules (verbose,debug) ts tc rules            
             --testRule (verbose,debug) ts tc (last (splits rules))
             mapM_ (testRule (verbose,debug) ts tc) (splits rules)   
             print "foo"
  where 
   splits :: (Eq a) => [a] -> [(a,[a])]
   splits xs = xs `for` \x -> let Just ind = elemIndex x xs
                              in  (x, take ind xs)

   for = flip fmap

   toTags' :: TagSet -> [[Tag]]
   toTags' = concatMap (nub . (\(a,b) -> if all null b then a else a++b)) . toTags


