module Main where

import Prelude hiding ( nub )
import Data.Char
import Data.List hiding ( nub )
import qualified Data.Set as S
import Data.Set( Set )
import qualified Data.Map as M
import Data.Map( Map )
import System.IO

file, gold :: FilePath
file = "data/es.tagged.ambiguous"
gold = "data/es.tagged"

evens (x:y:xs) = x : evens xs
evens xs       = xs

main =
  do s1 <- readFile file
     s2 <- readFile gold
     let ls1 = parse s1
         ls2 = parse s2
     putStrLn ("Sentences:  " ++ show (length ls1))
     
     let ls12 = evens
                [ (l1,l2)
                | (l1,l2) <- ls1 `zip` ls2
                , length l1 == length l2
                ]
         
         (ls1',ls2') = unzip ls12
     putStrLn ("Sentences*: " ++ show (length ls12))
     
     let allWords = [ w | l <- ls1', w <- l ]
         total    = length allWords
     putStrLn ("Words:      " ++ show total)

     let sc = score 0.0 [] ls1' ls2'
     putStrLn ("Base score: " ++ show (round sc)
                      ++ " (" ++ showPercent (sc / fromIntegral total) ++ ")")
     putStrLn ""
     
     rs <- speculateRules total [] (ls1',ls2') (ls1',ls2')
     sequence_ [ putStrLn $ showRule r | r <- rs ]


nub :: Ord a => [a] -> [a]
nub xs = go S.empty xs
 where
  go seen []     = []
  go seen (x:xs)
    | x `S.member` seen = go seen xs
    | otherwise         = x : go (S.insert x seen) xs

type Rule = ([Tag],[Tag])

showRule :: Rule -> String
showRule (a,b) =
  if null a then ">" ++ showSide b ++ "<"
            else showSide a ++ " >< " ++ showSide b
 where
  showSide ts = intercalate "+" ts

showPercent :: Double -> String
showPercent d = (if null p then "0" else p) ++ "." ++ q ++ "%"
 where
  s = "0" ++ show (floor (d * 10000))
  q = reverse (take 2 (reverse s))
  p = reverse (takeWhile (/= '0') (drop 2 (reverse s)))

speculateRules :: Int -> [Rule] -> (Text,Text) -> (Text,Text) -> IO [Rule]
speculateRules total rs (ls0,gs0) (ls,gs) =
  if null rsSorted then
    do return rs
   else
    do putStr ("sents: " ++ rjust 5 (show (length ls')))
       putStr (", cands: " ++ rjust 5 (show (length rsSorted)))
       hFlush stdout
       putStrLn ( ", best: " ++ rjust 5 (show (round sc))
               ++ " (" ++ showPercent (sc / fromIntegral total) ++ ")"
               ++ "  -->  " ++ showRule r
                )
       speculateRules total (rs ++ [r]) (ls0,gs0) (ls',gs')
 where
  (ls',gs') = unzip
			   [ (l,g)
			   | ((l,g),l') <- (ls `zip` gs) `zip` apply rs ls
			   , not (null [ w | (w, _:_:_) <- l' ])
			   ]

  rs' = speculateRule rs ls' gs'
  rsSorted = map snd $ takeWhile (\(n,_) -> n >= 12) $ reverse $ sort [ (n,r) | (r,n) <- M.toList rs' ]
  r   = snd $ maximum [ (score 1000.0 (rs ++ [r]) ls' gs', r) | r <- rsSorted ]
  sc  = score 0.0 (rs ++ [r]) ls0 gs0

rjust :: Int -> String -> String
rjust n s = replicate (n-l) ' ' ++ s
 where
  l = length s

speculateRule :: [Rule] -> Text -> Text -> Map Rule Int
speculateRule rs ls gs =
  M.fromListWith (+)
  [ (r,1)
  | (l1,l2) <- ls' `zip` gs
  , let ws = l1 `zip` l2
  , (w1,w2) <- ws `zip` tail ws
  , r@(a,b) <- speculateRuleWord w1 w2
  --, length a <= 2
  --, length b <= 2
  , r `notElem` rs
  ]
 where
  ls' = apply rs ls

speculateRuleWord :: (Word,Word) -> (Word,Word) -> [Rule]
speculateRuleWord ww1 ww2 =
  nub $ filter okay $ spec ww1 ww2 ++ map (\(a,b) -> (b,a)) (spec ww2 ww1)
 where
  okay (_,[]) = False
  okay _      = True
 
  spec :: (Word,Word) -> (Word,Word) -> [Rule]
  spec ((_,is1),(_,[g])) ((_,is2),_) =
    [ (lhs, rhs)
    | g `elem` is1
    , let is1' = is1 \\ [g]
    , not (null is1')
    , lhs <- nub (map nub $ concatMap (subs . snd) is1')
    , not (all (`elem` snd g) lhs)
    , let ts = commonTags is2
    , rhs <- [] : subs ts
    ]
  
  commonTags ((_,i):is) =
    nub
    [ t
    | t <- i
    , all ((t `elem`) . snd) is
    ]
  
  subs []     = []
  subs (x:xs) = [x] : [ x:ys | ys <- xss ] ++ xss
   where
    xss = subs xs

score :: Double -> [Rule] -> Text -> Text -> Double
score punish rs ls gs =
  sum [ if g `elem` is1
          then 1.0 / fromIntegral (length is1)
          else (-punish)
      | (l1,l2) <- ls' `zip` gs
      , ((_,is1),(_,[g])) <- l1 `zip` l2
      ]
 where
  ls' = apply rs ls

apply :: [Rule] -> Text -> Text
apply rs0 ls = map (app rs0) ls
 where
  app :: [Rule] -> Sentence -> Sentence
  app []     s = s
  app (r:rs) s =
    case apps r s of
      Nothing -> app rs s
      Just s' | r == head rs0 -> app rs s'
              | otherwise     -> app rs0 s'
  
  apps :: Rule -> Sentence -> Maybe Sentence
  apps r (w1:w2:ws) =
    case appw r (w1,w2) of
      Nothing        -> fmap (w1:) (apps r (w2:ws))
      Just (w1',w2') -> case apps r (w2':ws) of
                          Just ws' -> Just (w1':ws')
                          Nothing  -> Just (w1':w2':ws)
  apps r ws         = Nothing
  
  appw :: Rule -> (Word, Word) -> Maybe (Word, Word)
  appw (a,b) (w1@(q1,is1), w2@(q2,is2))
    | not (single is2) && and ais1 && or bis2 && not (null fis2) = Just (w1, (q2,fis2))
    | not (single is1) && and bis2 && or ais1 && not (null fis1) = Just ((q1,fis1), w2)
    | otherwise                                                  = Nothing
   where
    ais1 = [ all (`elem` snd i) a  | i <- is1 ]
    bis2 = [ all (`elem` snd i) b  | i <- is2 ]
    fis1 = [ i | (i,False) <- is1 `zip` ais1 ]
    fis2 = [ i | (i,False) <- is2 `zip` bis2 ]
    
{-
apply :: [Rule] -> Text -> Text
apply rs ls = map (fix app) ls
 where
  app :: Sentence -> Sentence
  app (w1:w2:ws) = let (w1',w2') = appw w1 w2 in w1' : app (w2':ws)
  app ws         = ws

  appw :: Word -> Word -> (Word, Word)
  appw w1@(_,[_])  w2@(_,[_]) = (w1,w2)
  appw w1@(s1,is1) w2@(s2,is2) =
    case rs' of
      []            -> (w1,w2)
      (is1',is2'):_ -> ((s1,is1'),(s2,is2'))
   where
    rs' = [ (is1',is2')
          | (t1,t2) <- rs
          , (is1',is2') <-
              if not (single is2)
              && all ((\i -> all (`elem` i) t1) . snd) is1
              && any ((\i -> all (`elem` i) t2) . snd) is2
              then
                [(is1, [ i | i <- is2, any (`notElem` snd i) t2 ])]
              else if not (single is1)
                   && all ((\i -> all (`elem` i) t2) . snd) is2
                   && any ((\i -> all (`elem` i) t1) . snd) is1
                   then
                     [([ i | i <- is1, any (`notElem` snd i) t1 ], is2)]
                   else
                     []
          , not (null is1')
          , not (null is2')
          ]
-}

single :: [a] -> Bool
single [_] = True
single _   = False

fix :: Eq a => (a -> a) -> a -> a
fix f x = let fx = f x in if fx == x then x else fix f fx

type Text     = [Sentence]
type Sentence = [Word]
type Word     = (String, [Interp])
type Interp   = (String, [Tag])
type Tag      = String

parse :: String -> Text
parse = map sentence
      . filter (not . null)
      . split sent
      . map ( word
            . split (=='/')
            . takeWhile (/='$')
            )
      . filter (not . all isSpace)
      . split (=='^')
 where
  word :: [String] -> Word
  word (w:is)   = (w, map (interp . split (=='<')) is)
  
  interp :: [String] -> Interp
  interp (s:ts) = (map toLower s, map (takeWhile (/='>')) ts)
  
  sent (_,is)  = any ("sent" `elem`) (map snd is)

  sentence s = [("BEGIN", [("BEGIN", ["BEGIN"])])] ++ s ++ [("END", [("END", ["END"])])]

split :: (a -> Bool) -> [a] -> [[a]]
split p [] = []
split p xs = takeWhile (not . p) xs : split p (drop 1 (dropWhile (not . p) xs))
