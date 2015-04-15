{-# LANGUAGE DoAndIfThenElse #-} 

module CG_SAT where 


import CG_base
import CG_data
import Data.List
import Data.Maybe
import Data.Set (Set, fromList, isSubsetOf)
import Control.Monad
import Control.Exception
--import MiniSat
import SAT
import SAT.Optimize
import SAT.Unary hiding (modelValue)
import qualified SAT.Unary as U
import Debug.Trace


type Token = ((Integer, [Tag]), Lit)

getInd :: Token -> Integer
getInd ((i,_),_) = i

getTags :: Token -> [Tag]
getTags ((_,t),_) = t

getLit :: Token -> Lit
getLit (_,b) = b

isBoundary :: Token -> Bool
isBoundary tok = not $ null ([BOS,EOS] `intersect` getTags tok)

-- we don't need to apply rules to tokens that are already unambiguous
isAmbig :: Token -> [Token] -> Bool
isAmbig tok toks = atLeast 2 (filter (sameInd tok) toks) 
  where atLeast 0 _  = True
        atLeast _ [] = False
        atLeast k (x:xs) = atLeast (k-1) xs

--we want to often compare the indices
sameInd :: Token -> Token -> Bool
sameInd tok tok' = getInd tok == getInd tok'


--Rule     has [[Tag]].
--Analysis has [Tag].
--At least one complete sublist in the rule must be found in the analysis.
tagsMatchRule :: [[Tag]] -> Token -> Bool
tagsMatchRule tags tok = or $ map (\tagset -> isSubsetOf tagset tagsInAna) tagsInRule
  where tagsInAna  = fromList $ getTags tok :: Set Tag
        tagsInRule = map fromList tags :: [Set Tag]

tagsDontMatchRule :: [[Tag]] -> Token -> Bool
tagsDontMatchRule tags tok = and $ map (\tagset -> null $ tagset `intersect` tagsInAna) tagsInRule
  where tagsInAna  = getTags tok :: [Tag]
        tagsInRule = tags :: [[Tag]]


-- | Input: sentence, as in list of analyses.
-- | Output: for each word in sentence, 
-- | * its position in the sentence
-- | * one of the suggested tag sequences
-- | * id of that hypothesis in the SAT solver
-- ((1,["the",<det>]),Lit v0)
-- ((2,["bear",<n>,<sg>]),Lit v1)
-- ((2,["bear",<vblex>,<pl>]),Lit v2)
-- ((3,["sleep",<n>,<pl>]),Lit v4)
-- ((3,["sleep",<vblex>,<sg>,<p3>]),Lit v5)
chunk :: Sentence -> [(Integer,[Tag])]
chunk sent = concat $ go sent 1
   where go []    _n = []
         go (x:xs) n = map ((,) n) x : go xs (n+1)

dechunk :: [Token] -> Sentence
dechunk ts = (map.map) getTags (groupBy sameInd ts)
        
  
--------------------------------------------------------------------------------

-- | For each position, at least one analysis must be true.
--   Group tokens by index and return a lists of variables 
--   to be used in disjunction.
anchor :: [Token] -> [[Lit]]
anchor toks = (map.map) getLit (groupBy sameInd toks)


-- | Apply rules to tokens. 
applyRule :: Rule -> [Token] -> [[Lit]]
applyRule rule toks = --trace (show rule) $
  case rule of
    (Remove _name tags conds) -> applyRules rule (toLists conds) toks
    (Select _name tags conds) -> applyRules rule (toLists conds) toks


applyRules :: Rule -> [[Condition]] -> [Token] -> [[Lit]]
applyRules rule []         allToks = []
applyRules rule (conds:cs) allToks = applyRules rule cs allToks ++
  case rule of 
    (Remove _n tags _c) -> let chosen = remove tags in mkVars chosen neg 
    (Select _n tags _c) -> let (chosen,other) = select tags in mkVars chosen id ++ mkVars other neg


  where 
        -- check if getContext has returned non-empty context for each condition
        allCondsHold :: [[Token]] -> Bool
        allCondsHold ts | null conds = True 
                        | otherwise  = all (not.null) ts


     -- `foo => bar' translates into `nt foo || bar'
        mkVars :: [(Token,[[Token]])] -> (Lit -> Lit) -> [[Lit]]
        mkVars tctx nt = [ conseq:ants | (t, ctx) <- tctx -- (Token,[[Token]])
                                       , tCombs <- sequence ctx -- :: [[Token]]
                                       , let conseq = nt (getLit t)
                                       , let ants = map (neg . getLit) tCombs ] 
     -- tCombs <- sequence ctx: say we have rule REMOVE v IF (-1 det) (1 n)
     -- and we get [ [(1,det)], [(3,n pl), (3,n sg)] ]
     -- we can't just put all of them in the list of antecedents
     -- because that would require n pl and n sg be true at the same time.
     -- Instead we make combinations [(1,det), (3,n sg)] and [(1,det), (3,n pl)]

        remove :: TagSet -> [(Token,[[Token]])]
        remove tags = [ (tok, ctx) | tok <- allToks
                                   , isAmbig tok allToks --only apply rules to ambiguous tokens
                                   , tagsMatchRule tags tok
                                   , let ctx = getContext tok allToks conds
                                   , allCondsHold ctx]

        select :: TagSet -> ([(Token,[[Token]])], [(Token,[[Token]])]) --(chosen,other)
        select tags = (chosen, other)
             -- chosen: analyses that have the wanted readings and context
 
            -- other: analyses that don't have the wanted readings,
            -- but some word in the same location does have the wanted reading(s).
            --      allToks = [(1,[N,Pl]), (2,[V,Sg]), (2,[N,Pl])]
            --      tags    = [V]
            --      chosen  = (2,[V,Sg]), other = (2,[N,Pl])
          where chosen = remove tags
                other  = [ (tok, ctx) | tok <- allToks
                                      , (wantedTok, ctx) <- chosen
                                      , sameInd tok wantedTok
--                                      , tok /= wantedTok ]
                                      , tagsDontMatchRule tags tok ]
--                                      , not $ tagsMatchRule tags tok ]

        

 


getContext :: Token           -- ^ a single analysis
               -> [Token]     -- ^ list of all analyses
               -> [Condition] -- ^ list of conditions grouped by AND
               -> [[Token]]   -- ^ context for the first arg. If all conditions match for a token, there will be as many non-empty Token lists as Conditions.
getContext tok allToks []     = []
getContext tok allToks ((C position (bool,ctags)):cs) = getContext tok allToks cs ++
  case ctags of
    []     -> [[dummyTok]] --empty conds = holds always
    [[]]   -> [[dummyTok]] 
    (t:ts) -> case position of
                Exactly 0 -> if nt $ tagsMatchRule ctags tok 
                               then [[tok]] --if the condition at 0 is in the *same reading* -- important for things like REMOVE imp IF (0 imp) (0 vblex)
                               else [filter (nt . tagsMatchRule ctags) (exactly 0 tok)] --if the LINK 0 thing is in a different reading
                Exactly n -> [filter (nt . tagsMatchRule ctags) (exactly n tok)]
                AtLeast n -> [filter (nt . tagsMatchRule ctags) (atleast n tok)]
                Barrier n bs  -> [filter (nt . tagsMatchRule ctags) (barrier n bs tok)]

  where nt = if bool then id else not

        dummyTok = ((999,[]),true) 

        --given word and n, return list of words that are n places away in original sentence
        exactly :: Integer -> Token -> [Token]
        exactly n ((ind,_),_) = [ tok | tok@((ind',_),_) <- allToks, ind' == ind+n ]

        --same but list of tags that are at least n places away
        atleast n ((ind,_),_) = [ tok | tok@((ind',_),_) <- allToks, ind' >= ind+n ]

        --between m and n places away
        between m n ((ind,_),_) = [ tok | tok@((ind',_),_) <- allToks
                                        , ind+m <= ind' && ind' <= ind+n ]

        barrier n btags tok | barinds==[] = atleast n tok
                            | n < 0     = between mindist n tok
                            | otherwise = between n mindist tok
           where barinds = [ getInd tok | tok <- allToks
                                        , tagsMatchRule btags tok ]
                 dists   = map (\i -> i - getInd tok) barinds :: [Integer]
                 mindist = minimum dists
                 

---- Main stuff


-- how to emulate ordering:
-- Maximise all instances of applying rule one; then commit to those; then maximise instances of rule 2, and so on.

disambiguate :: Bool -> [Rule] -> Sentence -> IO Sentence
disambiguate verbose rules sentence = do
  let chunkedSent = chunk sentence :: [(Integer,[Tag])]
  if length chunkedSent == length sentence then return sentence -- not ambiguous
   else
   do s <- newSolver

      --literal for each analysis
      litsForTags  <- sequence [ newLit s | _ <- chunkedSent ]

      let toks = zip chunkedSent litsForTags
          allNotFalse = anchor toks :: [[Lit]]
          applied = [ (rl, cl) | rl  <- rules
                               , cl <- applyRule rl toks
                               , (not.null) cl ] 

      --literal for each instance when a rule is applied
      litsForClauses <- sequence [ newLit s | _ <- applied ]


      let clauses = [ neg b:cl | (_, cl) <- applied
                               , b <- litsForClauses ]

      when verbose $ do
        putStrLn "\ntokens:"
        mapM_ print toks
        putStrLn "\nfirst step, make sure all readings for a given word are not false:"
        mapM_ print allNotFalse
        --putStrLn "\nclauses gotten by applying rules"
        --mapM_ print applied

      sequence_ [ addClause s cl | cl <- allNotFalse ]
      sequence_ [ addClause s cl | cl <- clauses ]

      lc <- count s litsForClauses
      b <- solveMaximize s [] lc

      if b then
           do cs <- sequence [ modelValue s x | x <- litsForClauses ]
              k <- U.modelValue s lc --number of true lits in lits for clauses
              when verbose $ do
                let conf = [ show rl ++ "\n* " ++ show cl 
                              | (False, (rl,cl)) <- zip cs applied ]
                putStrLn "These clauses were omitted due to conflicts:"
                mapM_ putStrLn (conf) 
                putStrLn $ "+ " ++ show ((length conf) - 2) ++ " others"


              lt <- count s litsForTags
              b2 <- solveMaximize s [lc .>= k] lt
              when verbose $ do
                let trueClauses = [ show rl ++ "\n* " ++ show cl 
                                     | (True, (rl,cl)) <- zip cs applied ] 
                putStrLn "\nThe following clauses were used:"
                mapM_ putStrLn trueClauses
            
                print (length trueClauses) 
                print (length litsForClauses)

              if b2 then
                do bs <- sequence [ modelValue s x | x <- litsForTags ]
                   let truetoks = [ t | (True, t) <- zip bs toks ]
                   when verbose $ do
                     putStrLn "\nThe following tag sequence was chosen:"
                     putStrLn $ showSentence (dechunk truetoks)
                   return (dechunk truetoks)
                else
                  do putStrLn "No solution after trying to maximise tags"
                     return []
      else
        do putStrLn "No solution"
           conf <- conflict s
           print conf
           return []

test :: IO ()
test = mapM_ (disambiguate True rls) CG_data.exs

  where rls = [-- rmVerbIfDet
             -- , rmNounIfPron
             -- , slNounAfterConj
             -- , slCCifCC             
             -- , slPrepIfDet 
             -- , rmAdvIfDet 
             -- , rmPlIfSg
             -- , rmSgIfPl
             -- , slNounIfBear 
             -- , slVerbAlways --conflicts with anything that selects other than V 
             -- , negTest      --should conflict
             -- , negOrTest    --should conflict
             -- , rmParticle
             ]

