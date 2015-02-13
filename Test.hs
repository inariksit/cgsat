module Main where

import CG
import CG_SAT
import CG_data
import Control.Monad
import Data.Boolean.SatSolver
import Test.QuickCheck

main :: IO ()
main = do verboseCheck checkRule
          quickCheck checkGetContext

checkGetContext :: Literal -> [Literal] -> [Condition] -> Bool
checkGetContext lit allLits conds = length (getContext lit allLits conds) == length conds

checkRule :: Rule -> Bool
checkRule rule = True

instance Arbitrary Tag where
  arbitrary = elements [Art , Adj , Adv , Det , N , PN , V , V2 , VV 
                      , Particle , Prep , Pron , Punct
                      , CoordConj , SubordConj
                      , Sg , Pl , P1 , P2 , P3 
                      , Subj , Imper , Cond , Inf , Pres
                      , Nom , Acc , Dat ]

instance Arbitrary Boolean where
  arbitrary = elements [Var n | n <- [1..100]]

instance Arbitrary Position where
  arbitrary = elements $ [Exactly n | n <- [-5..5]] ++
                         [AtLeast n | n <- [-5..5]] ++
                         [Barrier n [t] | n <- [-5..5],
                                          t <- [Art, Adj, Adv, Det, N, PN, V, V2, VV 
                                                 , Particle, Prep, Pron, Punct
                                                 , CoordConj, SubordConj
                                                 , Sg, Pl, P1, P2, P3 
                                                 , Subj, Imper, Cond, Inf, Pres
                                                 , Nom, Acc, Dat ]]

instance Arbitrary Condition where
  arbitrary = do pos <- arbitrary
                 bool <- arbitrary
                 tags <- listOf arbitrary
                 return $ C pos (bool, tags)

instance Arbitrary Test where
  arbitrary = frequency [(1, liftM NEG arbitrary)
                        ,(4, liftM POS arbitrary)]

instance Arbitrary Rule where
  arbitrary = frequency [(1, liftM2 Select arbitrary arbitrary)
                        ,(1, liftM2 Remove arbitrary arbitrary)]
