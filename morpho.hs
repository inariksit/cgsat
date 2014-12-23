import PGF2
import CG 
import qualified Data.Map as Map
import Data.List
import Data.Either
import System.Environment

-- | Just a test to mimic morphological analyser.
-- | Takes sentences, uses GF lookupMorpho and outputs all analyses.

main = do
  args <- getArgs
  if length args /= 3 then putStrLn "Usage: ./morpho <sentences> <pgf> <lang>" else do
      sentsFile <- readFile (args !! 0)
      pgf <- readPGF (args !! 1)
      cnc <- getConcr pgf (args !! 2)
      let sents = lines sentsFile
          ws = map words sents -- :: [[String]]
          analyses = (map . map) (lookupMorpho cnc) ws
          onlyS = (map . map) (filter (\(name, f:field, _) -> length (findIndices ('_'==) name) <= 1 && f == 's')) analyses -- :: [[[MorphoAnalysis]]]


      mapM_ putStrLn (zipWith printWA (head ws) (head onlyS))



{- Output:
"<bear>"
      "bear_V2" VInf
      "bear_V" s VInf
      "bear_N" s Sg Nom
-}
printWA w as = "\"<" ++ w ++ ">\"\t" ++ (unwords $ zipWith hargle lemmas anals)
   where anals = map snd3 as
         lemmas = map fst3 as
         hargle l a = "\n      " ++ '"':l ++ '"':' ':a

fst3 (val,_,_) = val
snd3 (_,val,_) = val


getConcr :: (Monad m) => PGF -> String -> m Concr
getConcr pgf lang =                                  --b is m Concr
    maybe                         -- maybe :: b -> (a -> b) -> Maybe a -> b
          (fail $ "Concrete syntax not found: "++show lang)  -- :: m Concr
          return                                             -- :: a -> m Concr
          (Map.lookup lang (languages pgf))                  -- :: Maybe Concr
