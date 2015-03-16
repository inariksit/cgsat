module QCTest where

import CG_base
import CG_SAT (getContext,Token(..))
import MiniSat
import SAT.SAT (Bit(..))
import Control.Monad
import Test.QuickCheck

main' :: IO ()
main' = do --verboseCheck checkRule
           quickCheck checkGetContext
           quickCheck checkToLists


--check that getContext gives as many contexts as it is given conditions
checkGetContext :: Token -> [Token] -> [Condition] -> Bool
checkGetContext lit allLits conds = length (getContext lit allLits conds) == length conds

--conditions glued together with AND form a list as long as there were original conditions
checkToLists :: (Condition, Int) -> Bool
checkToLists (cond, num) = length (toLists cond) == num

--just for fun, to see automatically generated CG rules
checkRule :: Rule -> Bool
checkRule rule = True

--------------------------------------------------------------------------------

arbCondList :: Gen (Condition, Int)
arbCondList = 
  do num <- choose (2,10)
     cond <- arbitrary :: Gen Condition
     let numConds = replicate num cond
     return (foldr1 AND numConds, num)

--------------------------------------------------------------------------------
allTags :: [Tag]
allTags = concat $ verb ++ noun ++ det ++ adv ++ conj ++ prep ++ sg ++ pl ++ cnjcoo

instance Arbitrary Tag where
  arbitrary = elements allTags

instance Arbitrary Bit where
  arbitrary = elements [Lit (MkLit n) | n <- [1..50]]

instance Arbitrary Position where
  arbitrary = elements $ [Exactly n | n <- [-5..5]] ++
                         [AtLeast n | n <- [-5..5]] ++
                         [Barrier n [[t]] | n <- [-5..5],
                                            t <- allTags]

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
