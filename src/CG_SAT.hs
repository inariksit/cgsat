module CG_SAT where

import Rule
import SAT ( Solver(..), newSolver, deleteSolver )
import SAT.Named

import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM

--------------------------------------------------------------------------------

type Sentence = IntMap Cohort
type Cohort = IntMap Lit

type Pattern = OrList [Int]
type TrgInd = Int

ctx2Pattern :: Sentence -> Context -> TrgInd -> OrList (Pattern,[TD])
ctx2Pattern = undefined
{- Sentence = [1..6]
   tdFooBar = [(Trg Foo, Dif mempty), (Trg Bar, Dif mempty)]

				(-1 Foo LINK 1 Bar) 4 = Or [ ( Or [[3,4]]
											 , td_FooBar ) 
										   ]
				(-1* Foo LINK 1 Bar) 4 = Or [ ( Or [[1,2],[2,3][3,4]]
											  , td_FooBar )
											]
				(-1 Foo LINK 1* Bar) 4 = Or [ ( Or [[3,4],[3,5],[3,6]]
											  , td_FooBar )
											]
				(-1* Foo LINK 1* Bar) 4 = Or [ ( Or [ [1,2],[1,3][1,4],[1,5],[1,6]
											        , [2,3],[2,4],[2,5],[2,6]
											        , [3,4],[3,5],[3,6] ]
											   , td_FooBar )
											  ]
                ((-1* Foo) OR (1C Bar)) 4 = Or [ (Or [[1],[2],[3]], (Trg Foo, Dif mempty))
 								               , (Or [[5]], (Trg Bar, Complement)) ]
-}

type TD = (TrgLits,DifLits)

data TrgLits = Trg (OrList (AndList Lit)) | AllTags
data DifLits = Dif (OrList (AndList Lit)) | Complement

--------------------------------------------------------------------------------



apply :: Solver -> Sentence -> Rule -> IO Sentence
apply = undefined

applyParallel :: Solver -> Sentence -> Rule -> IO Sentence
applyParallel = apply


--------------------------------------------------------------------------------

--Checks if give cohort matches given condition.
--If condition has *:  IF (*1 pr)
-- 1     2      3         4     5      6
-- the   bear   sleeps    in    the    house
--       trgi=2  
matchCond = undefined


tagsMatchRule = undefined
{- Cohort' has [ (57,casa/casa<n><f><sg>),
              , (58,casa/casar<vblex><pri><p3><sg>)
              , (59,casa/casar<vblex><imp><p2><sg>) ]
 CondInds has [ (trg=[58,59,60], dif=[15,38,57])
              , (trg=[foo],      dif=[bar]) 
              , ...                         ] 

OBS. if "<casa>" had 57-59 from the beginning, it always matches 57-59,
     no matter if 57 has been negated!
For now, exclude dif from this function.
We can always access difs when we make SAT-clauses.

OBS. Rule' can have overlapping trg and dif;
     that's an internal conflict, and the rule should never apply anywhere.
Handling that too in the SAT-clauses.

-}