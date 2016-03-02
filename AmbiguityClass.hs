module AmbiguityClass where

import Data.List
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe( fromJust )
import SAT ( Solver(..), newSolver, deleteSolver )
import SAT.Named

--------------------------------------------------------------------------------
-- example use

-- main :: IO ()
-- main =
--   do s <- readFile "spa-ambiguity-classes"
--      let ls   = lines s
--          xss  = map read ls :: [[Int]]
--          univ = S.toList . S.fromList . concat $ xss
     
--      -- compute the formula once
--      putStrLn "Computing formula..."
--      let p = formula xss
--      (p == p) `seq` putStrLn "Done."

--      -- reuse it many times
--      putStrLn "Generating constraints..."
--      s  <- newSolver
--      qs <- sequence [ newLit s | y <- univ ]
--      let tab  = M.fromList (univ `zip` qs)
--          mp y = fromJust (M.lookup y tab)
--      constraints s mp [] p
--      deleteSolver s
--      putStrLn "Done.

test :: IO ()
test =
  do let xss  = [[1,2,3],[2,3,4],[1,4],[5],[6,7],[3,7]]
         univ = S.toList . S.fromList . concat $ xss

     print univ

     let acfile = "data/spa/spa-ambiguity-classes-hackylemmas"
     ambclauses <- readFile acfile
     let xss'  = map read (lines ambclauses) :: [[Int]]
         univ' = S.toList . S.fromList . concat $ xss'
     (univ'==univ') `seq` print univ'
     print (length univ')
     print $ [1..1638] \\ univ'

     -- compute the formula once
     putStrLn "Computing formula..."
     let p = formula xss
     (p == p) `seq` putStrLn "Done."

     -- reuse it many times
     putStrLn "Generating constraints..."
     s  <- newSolver
     qs <- sequence [ newLit s "" | y <- univ ] --remove "" for non-named SAT+
     let tab  = M.fromList (univ `zip` qs)
         mp y = fromJust (M.lookup y tab)
     constraints s mp [] p
     putStrLn "Done."

     -- asking questions
     let poss xs =
           do b <- solve s [ mp x | x <- xs ]
              if b then
                do bs <- sequence [ solve s (mp y:map mp xs) | y <- univ ]
                   putStrLn (show xs ++ " is possible, but not also: " ++ show [ y | (y,False) <- univ `zip` bs ])
               else
                do putStrLn (show xs ++ " is NOT possible")
     
     poss [4,5]
     poss [1,2]
     poss [2]
     poss [7]
     deleteSolver s

--------------------------------------------------------------------------------
-- constructing the formula

data Form
  = And [Form]
  | Or [Form]
  | NotTag Int
 deriving ( Eq, Ord, Show )

formula' :: [[Int]] -> Int ->  Form
formula' xss univsz = build yss
 where
  univ = [1..univsz]
  yss  = S.toList . S.fromList $ [ univ \\ xs | xs <- xss ]

formula :: [[Int]] -> Form
formula xss = build yss
 where
  univ = S.toList . S.fromList . concat $ xss
  yss  = S.toList . S.fromList $ [ univ \\ xs | xs <- xss ]

build :: [[Int]] -> Form
build []   = Or []
build [[]] = And []
build xss  =
  Or [ And ( build [ xs \\ zs | xs <- xss0 ]
           : [ NotTag z | z <- zs ]
           )
     , build [ xs | xs <- xss1 ]
     ]
 where
  zs = field xss
  (xss0,xss1) = partition (\xs -> all (`elem` xs) zs) xss

field :: [[Int]] -> [Int]
field xss = sweep [y] [ xs | xs <- xss, y `elem` xs ] ys
 where
  y:ys = map snd
       . reverse
       . sort
       $ [ (length [ xs | xs <- xss, y `elem` xs ], y)
         | y <- S.toList . S.fromList . concat $ xss
         ]
  
  sweep zs xss []                   = zs
  sweep zs xss (y:ys)
    | nzs * nxss <= (nzs+1) * nxss' = sweep (y:zs) xss' ys
    | otherwise                     = sweep zs xss ys
   where
    nzs   = length zs
    nxss  = length xss
    xss'  = [ xs | xs <- xss, y `elem` xs ]
    nxss' = length xss'

--------------------------------------------------------------------------------
-- generating the constraints

constraints :: Solver -> (Int -> Lit) -> [Lit] -> Form -> IO ()
constraints s mp pre (And ps) =
  do sequence_ [ constraints s mp pre p | p <- ps ]

constraints s mp pre (Or [p]) =
  do constraints s mp pre p

constraints s mp pre p =
  do qs <- asOr s mp p
     addClause s (pre ++ qs)
     --print (take 5 $ pre++qs)
     --(qs == qs) `seq` putStrLn "constraints: Done."
 where
  asOr s mp (And [])   = do return [true]
  asOr s mp (And [p])  = do asOr s mp p
  asOr s mp (And ps)   = do x <- newLit s "" --remove "" for non-named SAT+
                            constraints s mp [neg x] (And ps)
                            return [x]
  asOr s mp (Or ps)    = do qss <- sequence [ asOr s mp p | p <- ps ]
                            return (concat qss)
  asOr s mp (NotTag z) = do return [neg (mp z)]

--------------------------------------------------------------------------------

