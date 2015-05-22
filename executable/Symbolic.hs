import CG_base
import CG_SAT hiding (chunk)
import CG_parse
import Data.List
import SAT
import SAT.Optimize
import SAT.Unary hiding (modelValue)
n = 2

--t = map Tag ["n", "vblex", "pri", "p3", "sg", "pl"]
tags = map Tag ["b","c","d","e"]


rules = parseRules False  "REMOVE:r1 (d) IF (-1C (c)) ;\nREMOVE:r2 (b) ;\nREMOVE:r3 (e) IF (-1C (c)) ;"

sentence = parseData "^first/f<b>/f<c>$ ^second/s<d>/s<e>/s<f>$" 


{- 
all subsequences of t should be considered for the position of one reading

symbolic must have at least one word with >1 reading, because of the requirement that applying R must make a difference

First rule: we must find some input so that it applies, no other requirements.
Second rule: we must find some input so that it applies, and first rule doesn't apply.

-----

First task: find such input that a rule will have effect, or prove there is none
1) Target    of the rule must be in the analysis
2) Condition of the rule must be in the context
-}


main = mapM_ doSomething (concat rules)


doSomething :: Rule -> IO ()
doSomething rule = do

  putStrLn "---------------------"
  s <- newSolver
  t <- sequence [ newLit s | _ <- tags
                           , _ <- [1..n] ] -- # of words in sentence

  let chunkedSymbolicSent = chunk $ replicate n (map (:[]) tags)
      symbolicToks = zip chunkedSymbolicSent t :: [Token]
      allNotFalse = anchor symbolicToks

--  print chunkedSymbolicSent
--  putStrLn $ showSentence (dechunk symbolicToks)
  print allNotFalse

  sequence_ [addClause s cl | cl <- applyRule rule symbolicToks ]

  lt <- count s t
  b <- solveMaximize s [] lt


  as <- sequence [ modelValue s x | x <- t ]
  let alltoks = [ ((i,(WF t:((Tag (sc++l)):ts))),lit) 
                  | (b, ((i,(WF t:Tag l:ts)),lit)) <- zip as symbolicToks 
                  , let sc = if b then "" else "; " ]
--  let truetoks = [ t | (True, t) <- zip as symbolicToks ]

  print rule
  putStrLn $ showSentence (dechunk alltoks)


--  print b
--  print t


  
chunk :: Sentence -> [(Integer,[Tag])]
chunk sent = concat $ go sent 1
   where go []    _n = []
         go (x:xs) n = map ((,) n) (addWF x) : go xs (n+1)
         addWF = map (WF ("w" ++ show n) :)

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

