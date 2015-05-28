import CG_base
import CG_SAT hiding (chunk)
import CG_parse
import Control.Monad
import Data.List
import SAT
import SAT.Optimize
import SAT.Unary hiding (modelValue)

n = 2

--tags = map Tag ["det", "n", "v", "pri", "prs", "imp", "p3", "sg", "pl"]
tags = map Tag ["b","c","e"]


rules = parseRules False {- "REMOVE:r1 (d) IF (-1C (c)) ;\n -} "REMOVE:r2 (b) ;\nREMOVE:r3 (e) IF (-1C (c)) ;"
--rules = parseRules False "REMOVE:r1 (v) IF (-1C (det)) ;\nREMOVE:r2 (prs) ;\nREMOVE:r3 (imp) IF (0 (p3)) ;"

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


main = do
  s <- newSolver
  t <- sequence [ newLit s | _ <- tags
                           , _ <- [1..n] ] -- # of words in sentence

  {- If rules can only consist of one tag (not a list/set) in target and condition,
     is it enough that all readings in the symbolic sentence have just one tag?

     Number of readings is potentially |subsequences tags| (???), but can be smaller.
  -}

  let chunkedSymbSent = chunk $ replicate n (map (:[]) tags)
      symbToks = zip chunkedSymbSent t :: [Token]

      ndSymbToks = map concat $ sequence
         [ filter (not.null) $ subsequences cohort | cohort <- groupBy sameInd symbToks ] -- :: [[[Token]]]

       --concatMap (filter (not.null) . subsequences) $ groupBy sameInd symbToks :: [[Token]]
       

      ruleAndPrevs = loop (concat rules) [] :: [(Rule, [Rule])]

  sequence_ [print cl >> addClause s cl | cl <- anchor symbToks ]
  print ruleAndPrevs


  let foo s rule prev toks = do
          putStrLn ""
          putStrLn $ "Current rule:  " ++ show rule
          putStrLn $ "Previous rule: " ++ show prev
          possibletoks <- constrain toks prev s :: IO [[Token]]
          putStrLn "^-- possibletoks"
          newtoks <- sequence [ constrain ts rule s | ts <- possibletoks ]
          putStrLn "^-- newtoks"

  sequence_ [ foo s rule prev symbToks | (rule, prevs) <- ruleAndPrevs
                                       , prev <- prevs 
                                       , symbToks <- ndSymbToks 
                                       , not $ null symbToks ] 

  putStrLn "end"

  where loop []     _     = [] 
        loop (r:rs) prevs = (r, prevs) : loop rs (r:prevs)


constrain :: [Token] -> Rule -> Solver -> IO [[Token]]
constrain toks rule s = do

  let lits = map getLit toks
  putStrLn "---------------------"

  let applied = applyRule rule toks
  litsForClauses <- sequence [ newLit s | _ <- applied ]
  let clauses = [ neg b:cl | (cl, b) <- zip applied litsForClauses ]

  sequence_ [print cl >> addClause s cl | cl <- clauses ]

  lt <- count s lits
  b <- solveMaximize s litsForClauses lt 
  as <- sequence [ modelValue s x | x <- lits ]

  -- Get all possible maximal models.
  possiblebs <- sequence [ solveMore s litsForClauses lits | _ <- lits ]
{-solveMore maximises, so there can be a model for each lit in lits to be False. 
  (Not applicable if max model has >1 False value. Works for my ad hoc example.) -}
  let truecounts = reverse $ sort [ (length $ filter (==True) bs, bs) 
                                            | bs <- (as:possiblebs) ]
      maxbs = nub $ map snd $ head $ groupBy (\(a,_) (b,_) -> a==b) truecounts

  sequence_ [ putStrLn [ if b then '1' else '0' |  b <- bs ] | bs <- maxbs ]

  print rule
  mapM_ pr maxbs

  -- cleanup: all lits in litsForClauses are in negated form in the 
  -- clauses added by the rules; adding them as positive unit clauses
  -- should get rid of all those clauses ???
  sequence_ [print lit >> addClause s [lit] | lit <- litsForClauses ]

  return [ [ t | (True, t) <- zip bs toks ] | bs <- maxbs ]
  

  where pr bs = do 
          let alltoks = [ ((i, (WF w:((Tag (sc++t)):ts))), lit) 
                    | (b, ((i, (WF w:((Tag      t ):ts))), lit)) <- zip bs toks 
                    , let sc = if b then "" else "; " ]
          putStrLn $ showSentence (dechunk alltoks)
          putStrLn "-----"


solveMore :: Solver -> [Lit] -> [Lit] -> IO [Bool]
solveMore s as xs = do
  bs <- sequence [ modelValue s x | x <- xs ]
  a <- newLit s
  addClause s (neg a : [ if b == True then neg x else x | (x,b) <- xs `zip` bs ])
  b <- count s xs >>= solveMaximize s (a:as) -- give max True count in xs to solveMax
  addClause s [neg a] -- cleanup: remove (a:as) by adding ~a as unit clause
  if b then do
     sequence [ modelValue s x | x <- xs ]
   else return []

  
chunk :: Sentence -> [(Integer,[Tag])]
chunk sent = concat $ go sent 1
   where go []    _m = []
         go (x:xs) m = map ((,) m) (addWF m x) : go xs (m+1)
         addWF m = map (WF ("w" ++ show m) :)




------------------------------------------------


{- general design:

do
  tags_word1 <- choose from [ ["w1", a], ["w1", a, b], ["w1", b], ... ]
  tags_word2 <- choose from [ ["w2", c], ["w2", c, d], ["w2", c], ... ]
  ...
  tags_wordN <- choose from [ ... ]

  let sentence = tags_word1 ++ ... ++ tags_wordN
  
  --  for each pair of rules:
  newsent <- constrain sentence rule1
  newersent <- constrain newsent rule2
  guard $ all (==True) newsent && any (==False) newersent


TODO: why are you not working? ;____; e.g.

[~v430,~v3]
1010
REMOVE:r2 b  IF (0 (*))
"<w1>"
        c
"<w2>"
        ; b
        c
        ; e
-}

