module Main where

import CG_base
import MiniSat
import SAT
import Control.Monad
--import Data.Set (Set,fromList)
import Test.QuickCheck

main :: IO ()
main = do verboseCheck checkRule
          verboseCheck checkToConds


--check that getContext gives as many contexts as it is given conditions
--checkGetContext :: Token -> [Token] -> [Condition] -> Bool
--checkGetContext lit allLits conds = length (getContext lit allLits conds) == length conds

--roundtrip from Condition to [[Condition]] to Condition
--TODO some less silly test.
checkToConds :: Condition -> Bool
checkToConds cond = conds == toConds (toCond conds)
  where conds = toConds cond

--just for fun, to see automatically generated CG rules
checkRule :: Rule -> Bool
checkRule rule = True

--------------------------------------------------------------------------------

arbCondList :: Gen (Condition, Int)
arbCondList = 
  do num <- choose (2,10)
     cond <- arbSingleCond :: Gen Condition
     let numConds = replicate num cond
     return (foldr1 AND numConds, num)

--------------------------------------------------------------------------------
allTags :: [Tag]
allTags = map Tag ["vblex", "vbser", "vbmod", "n", "np", "det", "adv", "cnjcoo", "cnjsub", "prep", "sg", "pl", "cnjcoo"]

instance Arbitrary Tag where
  arbitrary = elements allTags

instance Arbitrary TagSet where
  arbitrary = do tags <- listOf arbitrary `suchThat` (\as -> length as < 7 && not (null as))
                 return $ TS [tags]

instance Arbitrary SAT.Lit where
  arbitrary = elements [SAT.Lit (MiniSat.MkLit n) | n <- [1..50]]


instance Arbitrary Position where
  arbitrary = elements $ [Exactly c n | n <- [-5..5], c <- cautious] ++
                         [AtLeast c n | n <- [-5..5], c <- cautious] ++
                         [Barrier c bc n (TS [[t]]) | n <- [-5..5]
                                               , c <- cautious
                                               , bc <- cautious
                                               , t <- allTags ]
    where cautious = [Careful,NotCareful]

instance Arbitrary Condition where
  arbitrary = frequency $ [ (3, return Always)
                          , (50, arbSingleCond)
                          , (20, liftM2 AND arbitrary arbitrary)
                          , (1, liftM2 AND arbitrary arbitrary) ]

arbSingleCond :: Gen Condition
arbSingleCond = do pos <- arbitrary
                   pol <- elements [Pos,Neg]
                   tags <- arbitrary
                   return $ C pos (pol, tags)


instance Arbitrary Rule where
  arbitrary = frequency [(1, liftM2 (Select NoName) arbitrary arbitrary)
                        ,(1, liftM2 (Remove NoName) arbitrary arbitrary)]
