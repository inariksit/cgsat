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
  let ts = map (Tag . (:[])) "abcd"
  let tc = sequence [ts] -- ++ [[Tag "a",Tag "b"],[Tag "a",Tag "c"],[Tag "b",Tag "c"]]
  let tagmap = mkTagMap ts tc
  let allinds = IS.fromList [1..length tc]

  case args of 
    ("kimmo":_)
       -> do let kimmo_i' = map (ruleToRule' tagmap allinds) kimmo_implicit
             putStrLn "testing with implicit kimmo"
             mapM_ (testRule True "data/abcd-ambiguity-classes" tc) (splits (reverse kimmo_i'))

             let kimmo_e' = map (ruleToRule' tagmap allinds) kimmo_explicit
             putStrLn "testing with explicit kimmo"
             mapM_ (testRule True "data/abcd-ambiguity-classes" tc) (splits (reverse kimmo_e'))
    (lang:r)
       -> do let verbose = "v" `elem` r || "d" `elem` r
             let nosub = "nosub" `elem` r
             let dirname = "data/" ++ lang ++ "/" 
             let grfile  = dirname ++ lang ++ ".rlx"
             let tagfile = dirname ++ lang ++ ".tags"
             let rdsfile = dirname ++ lang ++ ".readings." ++ if nosub then "nosub" else "withsub"
             let ambcls  = dirname ++ lang ++ "-ambiguity-classes"
             tsInApe <- (concat . filter (not.null) . map parse . words) 
                         `fmap` readFile tagfile
             (tsets, rls) <- readRules' grfile
             let tcInGr = nub $ concatMap toTags' tsets
             tcInLex <- (map parse . words) `fmap` readFile rdsfile
             let tc = nub $ tcInGr ++ tcInLex  :: [[Tag]]
             let ts = nub $ tsInApe ++ concat tc
             let tagmap = mkTagMap ts tc
             let allinds = IS.fromList [1..length tc]
             let rules = map (ruleToRule' tagmap allinds) (concat (map reverse rls))
             mapM_ (testRule verbose ambcls tc) (splits rules)
{-             
    ("spa":r)
       -> do let verbose = "v" `elem` r || "d" `elem` r
             let debug = "d" `elem` r
             tsInApe <- (concat . filter (not.null) . map parse . words) 
                         `fmap` readFile "data/spa/spa_tags.txt"
--             (tsets, rls) <- readRules' "data/spa/apertium-spa.spa.rlx"
             (tsets, rls) <- readRules' "data/spa/spa_subreadings.rlx"
             let tcInGr = nub $ map toTags' tsets
             tcInLex <- (map parse . words) `fmap` readFile "data/spa/spa_tagcombs.txt"
             let tc = nub $ (concat tcInGr) ++ tcInLex 
             let ts = nub $ tsInApe ++ concat tc 
             let tagmap = mkTagMap ts tc
             let allinds = IS.fromList [1..length tc]
             let rules = map (ruleToRule' tagmap allinds) (concat (map reverse rls))
             print (length rules)
             --mapM_ print rules
             --mapM_ (testRule verbose tc) (splits rules)
             testRule verbose "spa-ambiguity-classes" tc (last $ splits rules)
             print "foo"

    ("por":r)
       -> do let verbose = "v" `elem` r || "d" `elem` r
             let debug = "d" `elem` r
             tsInApe <- (concat . filter (not.null) . map parse . words) 
                         `fmap` readFile "data/spa/spa_tags.txt"
             (tsets, rls) <- readRules' "data/apertium-por.por.rlx"
             let tcInGr = nub $ map toTags' tsets
             tcInLex <- (map parse . words) `fmap` readFile "data/spa/spa_tagcombs.txt"
             let tc = nub $ (concat tcInGr) ++ tcInLex 
             let ts = nub $ tsInApe ++ concat tc 
             let tagmap = mkTagMap ts tc
             let allinds = IS.fromList [1..length tc]
             let rules = map (ruleToRule' tagmap allinds) (concat (map reverse rls))
             print (length rules)
             --mapM_ print rules
             --mapM_ (testRule verbose tc) (splits rules)
             testRule verbose "spa-ambiguity-classes" tc (last $ splits rules)
             --testRule verbose "/tmp/foo" tc (last $ splits rules)
             putStrLn ""
    ("fin":r)
       -> do let verbose = "v" `elem` r || "d" `elem` r
             let debug = "d" `elem` r
             (tsets, rls) <- readRules' "data/fin.rlx"
             tcInLex <- (map parse . words) `fmap` readFile "data/fin-tagcombs.txt"
             let rules = concat (map reverse rls)
             let allConds = concatMap (toConds . cond) rules
             let unnamedTags = nub $ concatMap (map getTagset) allConds
             let tcInGr = nub $ concatMap toTags' $ tsets ++ unnamedTags
             let tc = tcInGr ++ tcInLex
             let ts = concat tc
             print (length tc)
             let tagmap = mkTagMap ts tc
             let allinds = IS.fromList [1..length tc]
             let rules' = map (ruleToRule' tagmap allinds) rules
             print (length rules)
             --mapM_ (testRule verbose tc) (splits rules')
             testRule verbose "" tc (last $ splits rules')
             putStrLn "end"
            -} 
    _ -> print "usage: cabal analyse [kimmo,nld,spa,fin] [v,d]"



  where 
   splits :: (Eq a) => [a] -> [(a,[a])]
   splits xs = xs `for` \x -> let Just ind = elemIndex x xs
                              in  (x, take ind xs)

   for = flip fmap

   toTags' :: TagSet -> [[Tag]]
   toTags' = concatMap (nub . (\(a,b) -> if all null b then a else a++b)) . toTags


