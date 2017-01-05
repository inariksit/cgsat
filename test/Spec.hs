module Main where

import CG_SAT
import Rule
import qualified Data.Map as M
import Data.IntSet
import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = do runTestTT $ TestList [ testNormaliseAbs ]
          putStrLn "this was a unit test"


testNormaliseAbs :: Test
testNormaliseAbs = TestCase $ assertEqual "(*) = Nothing" (normaliseAbs All M.empty) Nothing

{- 
Unit tests: nontrivial combinations of Diffs, Unions, Carts etc.
(Also test with VISL CG-3!)


Prop. test: ensure that Seq TD is as long as Seq CtxInd 


            6 4 (-1  Foo LINK 1  Bar) = Or [ ( Or [[3,4]]
            6 4 (-1* Foo LINK 1  Bar) = Or [ ( Or [[1,2],[2,3][3,4]]
            6 4 (-1  Foo LINK 1* Bar) = Or [ ( Or [[3,4],[3,5],[3,6]]
            6 4 (-1* Foo LINK 1* Bar) = Or [ ( Or [ [1,2],[1,3],[1,4],[1,5],[1,6]
                                                  , [2,3],[2,4],[2,5],[2,6]
                                                  , [3,4],[3,5],[3,6] ]
            6 4 (1 (F - B) OR (B - F)) = Or [ ( Or [[5]],
            6 4 (-1* Foo) OR (1C Bar) = Or [ ( Or [[1],[2],[3]]
                                             , ... )
                                           , ( Or [[5]]
                                             , ... )
                                           ]
            6 4 (-1 NOT Foo)          = Or [ ( Or [[3]]
                                             , ... ]
-}