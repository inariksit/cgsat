module SAT.SAT where

import CG (Tag(..))
import MiniSat
import Data.List ( nub )

--------------------------------------------------------------------------------

data Bit = Lit Lit | Bool Bool deriving ( Eq, Ord, Show )

nt :: Bit -> Bit
nt (Lit x)  = Lit (neg x)
nt (Bool b) = Bool (not b)

false, true :: Bit
false = Bool False
true  = Bool True

newBit :: Solver -> IO Bit
newBit s = Lit `fmap` newLit s

addClauseBit :: Solver -> [Bit] -> IO ()
addClauseBit s xs
  | true `elem` xs = do return ()
  | otherwise      = do addClause s [ x | Lit x <- xs ]
                        --putStrLn ("addClauseBit: " ++show xs)
                        return ()

solveBit :: Solver -> [Bit] -> IO Bool
solveBit s xs
  | false `elem` xs = do return False
  | otherwise       = do solve s [ x | Lit x <- xs ]

modelValueBit :: Solver -> Bit -> IO (Maybe Bool)
modelValueBit s (Lit x)  = modelValue s x
modelValueBit s (Bool b) = return (Just b)

--------------------------------------------------------------------------------

andl, orl :: Solver -> [Bit] -> IO Bit
andl s xs
  | false `elem` xs' = do return false
  | otherwise        =
      case xs' of
        []  -> do return true
        [x] -> do return x
        _   -> do y <- newBit s
                  addClauseBit s (y : map nt xs')
                  sequence_ [ addClauseBit s [nt y, x] | x <- xs' ]
                  return y
 where
  xs' = nub (filter (/= true) xs)

orl s xs = nt `fmap` andl s (map nt xs)

--------------------------------------------------------------------------------

atMostOne :: Solver -> [Bit] -> IO ()
atMostOne s xs = amo (length xs) xs
 where
  amo n xs | n <= 5 =
    do sequence_
         [ addClauseBit s [nt x, nt y]
         | (x,y) <- pairs xs
         ]

  amo n xs =
    do p <- newBit s
       amo (k+1)   (p     : take k xs)
       amo (n-k+1) (nt p : drop k xs)
   where
    k = n `div` 2

pairs :: [a] -> [(a,a)]
pairs (x:xs) = [ (x,y) | y <- xs ] ++ pairs xs
pairs []     = []

--------------------------------------------------------------------------------

data HowMany = None | One | Multiple deriving ( Eq, Ord, Show )

solveOne :: Solver -> [Bit] -> [Bit] -> IO HowMany
solveOne s as xs =
  do b <- solveBit s as
     if not b then
       do return None
      else
       do bs <- sequence [ modelValueBit s x | x <- xs ]
          a <- newBit s
          addClauseBit s (nt a : [ if b == Just True then nt x else x | (x,b) <- xs `zip` bs ])
          b <- solveBit s (a:as)
          addClauseBit s [nt a] -- just cleaning up
          if b then
            do return Multiple
           else
            do return One

--------------------------------------------------------------------------------

maximize :: Solver -> [Bit] -> [Bit] -> IO Bool
maximize s as xs =
  do b <- solveBit s as
     if not b then
       do return False
      else
       do a <- newBit s
          let opti xs =
                do bs <- sequence [ modelValueBit s x | x <- xs ]
                   sequence_
                     [ addClauseBit s [nt a, x]
                     | (x,b) <- xs `zip` bs
                     , b /= Just False
                     ]
                   let xs' = [ x | (x,Just False) <- xs `zip` bs ]
                   addClauseBit s (nt a : [ x | x <- xs' ])
                   b <- solveBit s (a:as)
                   if not b then
                     do return True
                    else
                     do opti xs'
           in opti xs

--------------------------------------------------------------------------------

{-
maximizeFromTop :: Solver -> [Bit] -> [Bit] -> IO Bool
maximizeFromTop s as rs =
  do b <- solveBit s (as ++ rs)
     if b then
       do return True
      else
       do ys <- conflict s
          let rs' = reverse rs
-}        

--------------------------------------------------------------------------------

testSolver =
  do s <- newSolver
     xs <- sequence [ newBit s | i <- [1..50] ]
     atMostOne s (take 20 xs)
     atMostOne s (take 20 (drop 20 xs))
     b <- maximize s [] xs
     print b
     if b then
       do bs <- sequence [ modelValueBit s x | x <- xs ]
          putStrLn [ if b == Just True then '1' else '0' | b <- bs ]
      else
       do return ()


--------------------------------------------------------------------------------

main' = 
  do s <- newSolver
     let names = [(1,Tag "det"),(2, Tag "noun"),(2, Tag "verb"),(3, Tag "noun"),(3,Tag "verb")]
     lits <- sequence [ newBit s | i <- [1..5] ]
     addClauseBit s [(lits !! 0)] --anchor unambiguous
     addClauseBit s [nt (lits !! 0), nt (lits !! 2)] --Remove Verb IF -1 Det
     b <- maximize s [] lits
     print b
     if b then
       do bs <- sequence [ modelValueBit s x | x <- lits ]
          putStrLn [ if b == Just True then '1' else '0' | b <- bs ]
      else
       do return ()
     
-- Literals:
-- 0det, 1n, 1v, 2n, 2v

-- Clauses:
-- -- If something is unambiguous to start with, anchor that:

-- 0det
-- 0det & 1n | 0det & 1v
-- 1n   & 2n | 1n   & 2v | 1v & 2n | 1v & 2v

-- rules:
--   Remove verb (C (-1) det)

--   ~(x-1 det) | ~(x verb)