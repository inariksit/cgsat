module Main where

import CG_base
import Data.List
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import System.Environment ( getArgs ) 


import Debug.Trace
--trace str a = a

type Key = Int
type ExampleWordForm = String

--type TagMap = M.Map Tag IS.IntSet
type AmbClass = M.Map ExampleWordForm IS.IntSet

--the IntSet from TagMap and AmbClass is the key set for LitMap
--for TagMap, the key is a single tag and value all readings where tag appears
--for AmbClass, the key is a word form and value all readings that the word has
--ie. AmbClass should have more zeroes for the average key than TagMap
--type LitMap = IM.IntMap Lit


main = do
 args <- getArgs
 case args of
  []    -> error "give a lexicon and tag combinations"
  (w:t:_) -> do ws <- words `fmap` readFile w
                tcs <- (map parseReadings . words) `fmap` readFile t
                mapM_ print tcs
                let ts = concat tcs
                let readings = map (toReadingMap tcs) ws 
                mapM_ (print) $ M.toList $ toGraph ( readings)
                --mapM_ print $ toGraph readings

--------------------------------------------------------------------------------

--toGraph :: 
toGraph readings = readingMap
 where
  group' acc (a,b) = case M.lookup a acc of
                       Just c  -> M.adjust (b:) a acc 
                       Nothing -> M.insert a [b] acc
  readingMap = invertMap 99999 $ foldl group' M.empty readings 
  --readingMap = onlyValues 99999 $ foldl group' M.empty readings 


--weet:weten<vblex><imp><sg> 
--weet:weten<vblex><imp><pl>  to 
--"weet":[ ["vblex", "imp", "sg"]
--       , ["vblex", "imp", "pl"] ]
--each of those tag combinations is a value in Word,
--and has a key, which is a number.
toReadingMap :: [[Tag]] -> String -> (ExampleWordForm, Key)
toReadingMap tcs str = trace (show (weet,key)) $ (weet, key)
 where
  (weet, ':':weten_vblex_imp_sg) = break (==':') str
  (_weten,         vblex_imp_sg) = break (=='<') weten_vblex_imp_sg
  tags = parseReadings vblex_imp_sg
  key = case elemIndex tags tcs of
          Just k' -> trace ("key found: " ++ show tags) k'+1
          Nothing -> trace ("key not found: " ++ show tags) 99999



onlyValues :: (Ord k, Ord v) => v -> M.Map k [v] -> [[v]]
onlyValues bad m = nub $ [vs' | (k, vs) <- M.toList m
                              , let vs' = filter (/=bad) vs
                              , not (null vs') ]

invertMap :: (Ord k, Ord v) => v -> M.Map k [v] -> M.Map [v] [k]
invertMap bad m = M.fromListWith (++) pairs
  where pairs = [(vs', [k]) | (k, vs) <- M.toList m
                           , let vs' = filter (/=bad) vs
                           , not (null vs') ]


parseReadings :: String -> [Tag]
parseReadings str = mainrP ++ concat subrsPI
 where
  (mainr:subrs) = split (=='+') str
  mainrP = parse' mainr :: [Tag]
  subrsP = map parse' subrs :: [[Tag]]
  subrsPI = map (\(n,subr) -> map (Subreading $ FromStart n) subr)
                (zip [1..] subrsP) :: [[Tag]] 


parse' :: String -> [Tag]
parse' str = map toTag $ filter (not.null) $ split (=='<') str
 where 
  toTag ">>>" = BOS
  toTag "<<<" = EOS
  toTag []    = error "empty tag"
  toTag str = if last str=='>' then Tag (init str) else Lem str 

split :: (a -> Bool) -> [a] -> [[a]]
split p [] = []
split p xs = takeWhile (not . p) xs : split p (drop 1 (dropWhile (not . p) xs))
