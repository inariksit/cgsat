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
isBoundary ((_,ts),_) = not $ null ([BOS,EOS] `intersect` ts)

-- we don't need to apply rules to tokens that are already unambiguous
isAmbig :: Token -> [Token] -> Bool
isAmbig tok toks = atLeast 2 (filter (sameInd tok) toks) 
  where atLeast 0 _  = True
        atLeast _ [] = False
        atLeast k (x:xs) = atLeast (k-1) xs


--we want to often compare the indices
sameInd :: Token -> Token -> Bool
sameInd ((i,_),_) ((i',_),_) = i == i'


--Rule     has [[Tag]].
--Analysis has [Tag].
--At least one complete sublist in the rule must be found in the analysis.
--If cautious mode, there must be only one sublist in analysis.
tagsMatchRule :: [[Tag]] -> Token -> Bool
tagsMatchRule tr ((_,ta),_) = go tr ta
  where go []       ta = False
        go (tr:trs) ta = if all (\t -> t `elem` ta) tr 
                           then True
                           else go trs ta 

tagsDontMatchRule :: [[Tag]] -> Token -> Bool
tagsDontMatchRule tagsInRule ((_, tagsInAna), _) = go tagsInRule tagsInAna
  where go []       ta = True
        go (tr:trs) ta = if any (\t -> t `elem` ta) tr 
                           then False
                           else go trs ta


-- | Input: sentence, as in list of analyses.
-- | Output: for each word in sentence, 
-- | * its position in the sentence
-- | * one of the suggested tag sequences
-- | * id of that hypothesis in the SAT solver
-- ((1,["the",<det>]),v0)
-- ((2,["bear",<n>,<sg>]),v1)
-- ((2,["bear",<vblex>,<pl>]),v2)
-- ((3,["sleep",<n>,<pl>]),v4)
-- ((3,["sleep",<vblex>,<sg>,<p3>]),v5)
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
    (Remove _name target conds) -> go False (toTags target) (toConds conds) toks
    (Select _name target conds) -> go True (toTags target) (toConds conds) toks

    
go :: Bool -> [[Tag]] -> [[Condition]] -> [Token] -> [[Lit]]
go isSelect tags []         allToks = []
go isSelect tags (conds:cs) allToks = --trace (show conds) $
  if isSelect 
    then let (ch,ot) = select tags in mkVars ch id ++ mkVars ot neg
    else let chosen  = remove tags in mkVars chosen neg 
  ++ go isSelect tags cs allToks

        -- check if getContext has returned non-empty context for each condition
  where allCondsHold :: [[Token]] -> Bool
        allCondsHold ts | null conds = True 
                        | otherwise  = (not.null) ts && all (not.null) ts


     -- `foo => bar' translates into `~foo || bar'
        mkVars :: [(Token,[[Token]])] -> (Lit -> Lit) -> [[Lit]]
        mkVars tctx nt = [ conseq:ants | (t, ctx) <- tctx -- (Token,[[Token]])
                                       , tCombs <- sequence ctx -- :: [[Token]]
                                       , let conseq = nt (getLit t)
                                       , let ants = [] ] --map (neg . getLit) tCombs ] 
     -- tCombs <- sequence ctx: say we have rule REMOVE v IF (-1 det) (1 n)
     -- and we get [ [(1,det)], [(3,n pl), (3,n sg)] ]
     -- we can't just put all of them in the list of antecedents
     -- because that would require n pl and n sg be true at the same time.
     -- Instead we make combinations [(1,det), (3,n sg)] and [(1,det), (3,n pl)]

        remove :: [[Tag]] -> [(Token,[[Token]])]
        remove tags = [ (tok, ctx) | tok <- allToks
                                   , isAmbig tok allToks --only apply rules to ambiguous tokens
                                   , tagsMatchRule tags tok
                                   , let ctx = getContext tok allToks conds
                                   , allCondsHold ctx]

        select :: [[Tag]] -> ([(Token,[[Token]])], [(Token,[[Token]])])
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
                                      , tagsDontMatchRule tags tok ]


getContext :: Token           -- ^ a single analysis
               -> [Token]     -- ^ list of all analyses
               -> [Condition] -- ^ list of conditions grouped by AND
               -> [[Token]]   -- ^ context for the first arg. If all conditions match for a token, there will be as many non-empty Token lists as Conditions.
getContext tok allToks []     = []
getContext tok allToks ((C position (bool,ctags)):cs) = getContext tok allToks cs ++
  case ctags' of
    []     -> [[dummyTok]] --empty conds = holds always
    [[]]   -> [[dummyTok]] 
    (t:ts) -> case position of
                Exactly _ 0 -> if match tok 
                                 then [[tok]] --if the feature at 0 is in the *same reading* -- important for things like REMOVE imp IF (0 imp) (0 vblex)
                                 else [filter match $ exactly 0 tok] --feature in  different reading

                -- for cautious mode. ONLY WORKS WITH SYMBOLIC!
                Exactly True n -> if length (exactly n tok) > 1 
                                   then [[]]
                                   else [filter match $ exactly n tok]
                AtLeast True n -> if length (atleast n tok) > 1
                                   then [[]]
                                   else [filter match $ atleast n tok]

                -- normal mode
                Exactly _ n -> [filter match $ exactly n tok]
                AtLeast _ n -> [filter match $ atleast n tok]
                Barrier n bs  -> [barrier n bs tok]

  where ctags' = toTags ctags
        match  = (if bool then id else not) . tagsMatchRule ctags'

        dummyTok = ((999,[]),true) 

        --given word and n, return list of words that are n places away in original sentence
        exactly :: Integer -> Token -> [Token]
        exactly n ((ind,_),_) = [ tok | tok@((ind',_),_) <- allToks
                                      , ind' == ind+n ]


        --same but list of tags that are at least n places away
        atleast n ((ind,_),_) = [ tok | tok@((ind',_),_) <- allToks
                                      , ind' >= ind+n ]


        --between m and n places away
        between m n ((ind,_),_) = [ tok | tok@((ind',_),_) <- allToks
                                        , ind+m <= ind' && ind' <= ind+n
                                        , match tok ]

        barrier n btags tok | barinds==[] = filter match $ atleast n tok
                            | n < 0     = between mindist n tok
                            | otherwise = between n mindist tok
           where barinds = [ ind | tok@((ind,_),_) <- allToks
                                 , tagsMatchRule (toTags btags) tok ]
                 dists   = map (\i -> i - getInd tok) barinds :: [Integer]
                 mindist = minimum dists
                 

---- Main stuff
disamSection ::  ([Rule] -> Sentence -> IO Sentence) -> [[Rule]] -> Sentence -> IO Sentence
disamSection disam []         sent = return sent
disamSection disam [rs]       sent = disam rs sent
disamSection disam (r1:r2:rs) sent = disam (take 10 (reverse r1)) sent
                                      >>= \s -> ({-print s >>-} disamSection disam 
                                                ((r1++r2):rs) s)
 

disambiguate :: Bool -> Bool -> [Rule] -> Sentence -> IO Sentence
disambiguate verbose debug rules sentence = do
  let chunkedSent = chunk sentence :: [(Integer,[Tag])]
  if length chunkedSent == length sentence then return sentence -- not ambiguous
   else
   do s <- newSolver

      --literal for each analysis
      litsForAnas  <- sequence [ newLit s | _ <- chunkedSent ]

      let toks = zip chunkedSent litsForAnas
          allNotFalse = anchor toks :: [[Lit]]
          applied = [ (rl, cl) | rl  <- rules
                               , cl <- applyRule rl toks 
                               , (not.null) cl ] :: [(Rule, [Lit])]

      --literal for each instance when a rule is applied
      litsForClauses <- sequence [ newLit s | _ <- applied ]


      let clauses = [ neg b:cl | (_, cl) <- applied
                               , b <- litsForClauses ]

      when debug $ do
        putStrLn "\ntokens:"
        mapM_ print toks
        -- putStrLn "\nfirst step, make sure all readings for a given word are not false:"
        -- print allNotFalse
        --putStrLn "\nclauses gotten by applying rules"
        --mapM_ print applied

      sequence_ [ addClause s cl | cl <- allNotFalse ]
      sequence_ [ addClause s cl | cl <- clauses ]

      lc <- count s litsForClauses
      b <- solveMaximize s [] lc

      if b then
           do cs <- sequence [ modelValue s x | x <- litsForClauses ]
              k <- U.modelValue s lc --number of true lits in lits for clauses
              when debug $ do
                let conf = [ show rl ++ "\n* " ++ show cl 
                              | (False, (rl,cl)) <- zip cs applied ]
                putStrLn "These clauses were omitted due to conflicts:"
                mapM_ putStrLn (conf) 
                putStrLn $ "+ " ++ show ((length conf) - 2) ++ " others"


              lt <- count s litsForAnas
              b2 <- solveMaximize s [lc .>= k] lt
              when debug $ do
                let trueClauses = [ show rl ++ "\n* " ++ show cl 
                                     | (True, (rl,cl)) <- zip cs applied ] 
                putStrLn "\nThe following clauses were used:"
                mapM_ putStrLn trueClauses
            
                print (length trueClauses) 
                print (length litsForClauses)

              if b2 then
                do bs <- sequence [ modelValue s x | x <- litsForAnas ]
                   let truetoks = [ t | (True, t) <- zip bs toks ]
                   when verbose $ do
                     --putStrLn "\nThe following tag sequence was chosen:"
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

disambiguateWithOrder :: Bool -> Bool -> [Rule] -> Sentence -> IO Sentence
disambiguateWithOrder verbose debug rules sentence = do
  let chunkedSent = chunk sentence :: [(Integer,[Tag])]
  if length chunkedSent == length sentence then return sentence -- not ambiguous
   else
   do s <- newSolver

      --literal for each analysis
      litsForAnas <- sequence [ newLit s | _ <- chunkedSent ]

      let toks = zip chunkedSent litsForAnas
          allNotFalse = anchor toks :: [[Lit]]

      sequence_ [ addClause s cl | cl <- allNotFalse ]

      let appliedRaw = [ (rl, cl) | rl <- rules
                                  , cl <- applyRule rl toks
                                  , not $ null cl
                             ] :: [(Rule, [Lit])]
     
      litsForInsts <- sequence [ newLit s | _ <- map snd appliedRaw ] :: IO [Lit]

      -- add literal to identify the instance
      let applied = [ (rl, neg lit:cl) 
                      | (lit, (rl,cl)) <- zip litsForInsts appliedRaw ]
      --mapM_ print applied
      let onlyClauses = (map.map) snd $ groupBy (\x y -> fst x==fst y) applied :: [[[Lit]]]

      --add clauses; in the "bs <- " loop when we add lits, some of those clauses will then be automatically removed
      --sequence_ [ addClause s cls | cls <- concat onlyClauses ]


      when debug $ do
        putStrLn "\ntokens:"
        mapM_ print toks
        -- putStrLn "\nfirst step, make sure all readings for a given word are not false:"
        -- print allNotFalse
        -- putStrLn "\nrules that are possibly triggered:"
        -- mapM_ putStrLn [ show rl ++ "\n* " ++ show cls | (rl, cls) <- zip rules applied ]

      bs <- sequence [ do k <- count s insts :: IO Unary
                          b <- solveMaximize s [] k
                          is <- sequence [ modelValue s x | x <- insts ] 
                          sequence_ [ addClause s [lit]
                                       | (True, lit, c) <- zip3 is insts cs]
                          return b
                  | cls <- onlyClauses
                  , let insts = map (neg . head) cls
                  , let cs = map (!! 1) cls ]
      --when debug $ print bs


      when debug $ do 
       cs <- sequence [ modelValue s x | x <- litsForInsts ]
       putStrLn "\nUsed clauses:"
       mapM_ putStrLn [ show rl ++ "\n* " ++ show cl 
                          | (True, (rl,cl)) <- zip cs applied 
                      ]
                         
       putStrLn "\nUnused clauses:"
       mapM_ putStrLn [ show rl ++ "\n* " ++ show cl 
                          | (False, (rl,cl)) <- zip cs applied 
                      ]

      --la <- count s litsForAnas
      --b <- solveMaximize s [] la
      --b <- solveMinimize s [] la --shouldn't do this, just add constraints that only one interpretation can be true
      --TODO check atMostOne in sat+
      --b <- solve s [] --just this doesn't make any difference!

      if and bs && not (null bs) then
                do as <- sequence [ modelValue s x | x <- litsForAnas ]
                   let truetoks = [ t | (True, t) <- zip as toks ]
                   let alltoks = [ ((i,(WF t:((Lem (sc++l)):ts))),lit) | (b, ((i,(WF t:Lem l:ts)),lit)) <- zip as toks 
                                           , let sc = if b then "" else "; " ]
                   when (verbose && debug) $ do
                     putStrLn "\nThe following tag sequence was chosen:"
                     putStrLn $ showSentence (dechunk alltoks)
                   when (verbose && (not debug)) $  
                     putStrLn $ showSentence (dechunk truetoks)
                   return (dechunk truetoks)
      else 
        if not (and bs) then 
                        do when debug $ do putStrLn "No solution"
                                           conf <- conflict s
                                           print conf
                           return sentence
          else do when debug $ putStrLn "No rules triggered"
                  return sentence

test :: IO ()
test = mapM_ (disambiguateWithOrder True True rls) CG_data.exs

  where rls = [rmVerbIfDet
             , rmNounIfPron
             , slNounAfterConj
             , slCCifCC             
             , slPrepIfDet 
             , rmAdvIfDet 
             , rmPlIfSg
             , rmSgIfPl
             , slNounIfBear 
             , slVerbAlways --conflicts with anything that selects other than V 
             , negTest      --should conflict
             , negOrTest    --should conflict
             , rmParticle
             ]

