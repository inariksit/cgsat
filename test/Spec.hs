module Main where

import Test.QuickCheck

main :: IO ()
main = undefined



{- 
CG_SAT.tagset2TDs

Unit tests: nontrivial combinations of Diffs, Unions, Carts etc.
(Also test with VISL CG-3!)

CG_SAT.ctx2Pattern

Prop. test: ensure that Seq TD is as long as Seq CtxInd 

Some potential unit tests ?

 tdFooBar = Or[ Seq [(Trg Foo, Dif mempty), (Trg Bar, Dif mempty)] ]

            6 4 (-1  Foo LINK 1  Bar) = Or [ ( Or [[3,4]]
                                             , td_FooBar ) 
                                           ]
            6 4 (-1* Foo LINK 1  Bar) = Or [ ( Or [[1,2],[2,3][3,4]]
                                             , td_FooBar )
                                           ]
            6 4 (-1  Foo LINK 1* Bar) = Or [ ( Or [[3,4],[3,5],[3,6]]
                                             , td_FooBar )
                                           ]
            6 4 (-1* Foo LINK 1* Bar) = Or [ ( Or [ [1,2],[1,3],[1,4],[1,5],[1,6]
                                                  , [2,3],[2,4],[2,5],[2,6]
                                                  , [3,4],[3,5],[3,6] ]
                                              , td_FooBar )
                                           ]
            6 4 (1 (F - B) OR (B - F)) = Or [ ( Or [[5]],
                                              , td_F-B_B-F ) 
                                            ]
            td_F-B_B-F = Or [ Seq [()]
                            , Seq [()] 
                            ]

            6 4 (-1* Foo) OR (1C Bar) = Or [ ( Or [[1],[2],[3]]
                                             , (Trg Foo, Dif mempty) )
                                           , ( Or [[5]]
                                             , (Trg Bar, Complement) )
                                           ]
            6 4 (-1 NOT Foo)          = Or [ ( Or [[3]]
                                             , (Trg AllTags, Dif Foo )]

-}