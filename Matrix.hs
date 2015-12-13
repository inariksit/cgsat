module Main where

import CG_base
import CG_Symbolic
import Data.List
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import System.Environment ( getArgs ) 

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
                tcs <- (map parseTC . words) `fmap` readFile t
                let ts = concat tcs
                let readings = map (toReadingMap tcs) ws 
                --mapM_ print (take 50 readings)
                --mapM_ print $ M.toList $ toGraph ( readings)
                mapM_ print $ toGraph readings

--------------------------------------------------------------------------------


parseTC :: String -> [Tag]
parseTC str = map toTag $ filter (not.null) $ split isValid str
 where 
  isValid c = c=='<' || c=='+'
  toTag ">>>" = BOS
  toTag "<<<" = EOS
  toTag []    = error "empty tag"
  toTag str = if last str=='>' then Tag (init str) else Lem str



--toGraph :: 
toGraph readings = readingMap
 where
  group' acc (a,b) = case M.lookup a acc of
                       Just c  -> M.adjust (b:) a acc 
                       Nothing -> M.insert a [b] acc
  --readingMap = invertMap $ foldl group' M.empty readings 
  readingMap = onlyValues 99999 $ foldl group' M.empty readings 


onlyValues :: (Ord k, Ord v) => v -> M.Map k [v] -> [[v]]
onlyValues bad m = nub $ [vs' | (k, vs) <- M.toList m
                              , let vs' = filter (/=bad) vs
                              , not (null vs') ]

invertMap :: (Ord k, Ord v) => M.Map k [v] -> M.Map [v] [k]
invertMap m = M.fromListWith (++) pairs
  where pairs = [(vs, [k]) | (k, vs) <- M.toList m]

group' acc (a,b) =
  case M.lookup a acc of
    Just c -> M.adjust (b:) a acc 
    Nothing -> M.insert a [b] acc
--weet:weten<vblex><imp><sg> 
--weet:weten<vblex><imp><pl>  to 
--weet :[ [Tag "vblex", Tag "imp", Tag "sg"]
--      , [Tag "vblex", Tag "imp", Tag "pl"] ]
--each of those tag combinations is a value in Word,
--and has a key, which is a number.
toReadingMap :: [[Tag]] -> String -> (ExampleWordForm, Key)
toReadingMap tcs str = (weet, key)
 where
  (weet, vblex_imp_sg) = break (==':') str
  tags = map Tag $ tail $ map (delete '>') $ split (=='<') vblex_imp_sg
  key = case elemIndex tags tcs of
          Just k' -> k'
          Nothing -> 99999