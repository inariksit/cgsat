{-# LANGUAGE DoAndIfThenElse #-} 

module CG_simple where

import CG_base
import CG_SAT hiding ( tagsMatchRule, applyRule, go, disambiguate, getContext )
import Data.List
import Control.Monad ( liftM2, when )
import SAT
import SAT.Optimize
import SAT.Unary hiding ( modelValue )
import qualified SAT.Unary as U
import Debug.Trace

--------------------------------------------------------------------------------


-- Rule     has [(trg::[[Tag]], dif::[[Tag]])] , disjoint.
-- Analysis has [Tag].
-- In the analysis there must be at least one complete trg-sublist,
-- which does NOT include tags from any dif-sublist in its (Trg,Dif) pair.
tagsMatchCond :: [Token] -> Token -> [(Trg,Dif)] -> Bool
tagsMatchCond allToks t trg_difs = 
  case (isPositive t, isCautious t) of
    (True, False) -> any (bothMatch t) trg_difs --1
    (True, True) -> all (\tok -> any (bothMatch tok) trg_difs) toksAtSameInd --1C
    (False, False) -> not $ any (\tok -> any (bothMatch tok) trg_difs) toksAtSameInd --NOT 1
    (False, True) -> not $ all (\tok -> any (bothMatch tok) trg_difs) toksAtSameInd --NOT 1C

 where
  toksAtSameInd = filter (sameInd t) allToks
  bothMatch t (trg,dif) = let ta = getTags t in pos trg ta && neg dif ta
  pos []       ta = False
  pos (tr:trs) ta = if all (\t -> t `elem` ta) tr 
                       then True
                       else pos trs ta 
  neg []       ta = True
  neg (tr:trs) ta = if any (\t -> t `elem` ta) tr 
                       then False
                       else neg trs ta

tagsMatchTarget :: Token -> [(Trg,Dif)] -> Bool
tagsMatchTarget t trg_difs = any (bothMatch t) trg_difs
 where
  bothMatch t (trg,dif) = let ta = getTags t in pos trg ta && neg dif ta
  pos []       ta = False
  pos (tr:trs) ta = if all (\t -> t `elem` ta) tr 
                       then True
                       else pos trs ta 
  neg []       ta = True
  neg (tr:trs) ta = if any (\t -> t `elem` ta) tr 
                       then False
                       else neg trs ta

--------------------------------------------------------------------------------



-- | Apply rules to tokens. 
applyRule :: Solver -> [Token] -> Rule -> IO [Clause]
applyRule s toks rule = do
  case rule of 
    (Remove _name target conds) -> go s False False (toTags target) (toConds conds) toks
    (Select _name target conds) -> go s True False (toTags target) (toConds conds) toks

    
go :: Solver -> Bool -> Bool -> [(Trg,Dif)] -> [[Condition]] -> [Token] -> IO [Clause]
go s isSelect isGrAna trgs_diffs []         allToks = return []
go s isSelect isGrAna trgs_diffs (conds:cs) allToks = do
   let f = mkVars
   if isSelect 
     then let (trg,rm) = select trgs_diffs in f trg rm
     else let trg      = remove trgs_diffs in f [] trg

   ++> go s isSelect isGrAna trgs_diffs cs allToks

        
 where
  (++>) = liftM2 (++)

  -- check if getContext has returned non-empty context for each condition
  allCondsHold :: [Context] -> Bool
  allCondsHold ts | null conds = True
                  | otherwise  = (not.null) ts && all (not.null) ts

  remove :: [(Trg,Dif)] -> [(Token,[Context])]
  remove t_ds = [ (tok, ctx) | tok <- allToks
                             , isAmbig tok allToks --only apply rules to ambiguous words
                             , tagsMatchTarget tok t_ds
                             , let ctx = getContext tok allToks conds
                             , allCondsHold ctx ]

  select :: [(Trg,Dif)] -> ([(Token,[Context])], [(Token,[Context])])
  select t_ds = (chosen, other)
    -- chosen: analyses that have the wanted readings and context
    -- other: don't have wanted readings, but some other word in the same index has.
    where chosen = remove t_ds
          other  = [ (tok, ctx) | tok <- allToks
                                , (wantedTok, ctx) <- chosen
                                , sameInd tok wantedTok 
                                , not $ tagsMatchTarget tok t_ds ]


  --Simple version: we don't condition on conditions, just remove or select targets.
  mkVars :: [(Token,[Context])] -- ^ Tokens to be selected, each with its context tokens
         -> [(Token,[Context])] -- ^ Tokens to be removed, each with its context tokens
         -> IO [Clause]
  mkVars sl rm = do
    let rmClauses = [ [neg tl] | (trg, ctx) <- rm, let tl = getLit trg ] 
    let slClauses = [ [tl]     | (trg, ctx) <- sl, let tl = getLit trg ] 
    return $ rmClauses ++ slClauses



getContext :: Token       -- ^ a single analysis
           -> [Token]     -- ^ list of all analyses
           -> [Condition] -- ^ list of conditions grouped by AND
           -> [Context]   -- ^ context for the first arg. If all conditions match for a token, there will be as many non-empty Contexts ([Token]) as Conditions.
getContext ana allToks []          = []
getContext ana allToks (Always:cs) = [dummyTok] : getContext ana allToks cs 
getContext ana allToks (ctx@(C position ts@(positive,ctags)):cs) = 
 result : getContext ana allToks cs
 where 
  result = case head $ toTags ctags of
    ([]  ,_)   -> [dummyTok] --empty conds = holds always
    ([[]],_)   -> [dummyTok] 
    (t:ts,_) -> case position of
                Exactly False 0 -> 
                --TODO: rethink the next line, is this a good idea?
                 if match ana then [ana] --feature at 0 is in the *same reading*
                   else [ neg' t | t <- exactly 0 ana, match (neg' t)] --feature in other reading

                Exactly True 0 ->
                 if match ana then [ neg' $ mkCautious ana ]
                   else [ neg' cT | t <- exactly 0 ana
                               , let cT = mkCautious t
                               , match (neg' cT)] 

                Exactly True n -> [ neg' cT | t <- exactly n ana 
                                         , let cT = mkCautious t
                                         , match (neg' cT) ]
                AtLeast True n -> [ neg' cT | t <- atleast n ana
                                         , let cT = mkCautious t
                                         , match (neg' cT) ]

                Exactly _ n -> [ neg' t | t <- exactly n ana, match (neg' t) ]
                AtLeast _ n -> [ neg' t | t <- atleast n ana, match (neg' t) ]
                Barrier  _ n bs -> map neg' $ barrier n bs ana
                CBarrier _ n bs -> map neg' $ cbarrier n bs ana

  neg' :: Token -> Token
  neg' = if positive then id else mkNegative

  match :: Token -> Bool
  match tok = tagsMatchCond allToks tok (toTags ctags)

  --given word and n, return words that are n places away in the original sentence
  exactly :: Int -> Token -> [Token]
  exactly n token = [ tok' | tok' <- allToks
                           , getInd tok' == n + getInd token ]


  --same but list of tags that are at least n places away
  atleast n token = [ tok' | tok' <- allToks
                           , getInd tok' >= n + getInd token ]


  --between m and n places away
  between m n token =
    let toks = [ tok | tok <- allToks
                     , let ind' = getInd tok
                     , let ind = getInd token
                     , ind+m <= ind' && ind' <= ind+n ]
    in if positive
         then filter match toks
         else if all match toks 
                then toks else [] --NOT: all readings between m-n must [not match]


  --from n places away until one of btags is found. if not, same as atleast.
  --to mimic vislcg3 behaviour with negation, the following is fine:
  -- (NOT 1* foo BARRIER bar)  matches  ^target/lemma<ana>$ ^barrier/bar<ana>$
  -- ie. no need to have a ~foo word, just to *not* have foo for any reason.
  -- we mimic this behaviour by counting bar as a ~foo.
  barrier n btags token | barinds==[] = filter match $ atleast n token
                        | mindist < 0 = filter match $ atleast n token
                        | n < 0       = between mindist n token
                        | otherwise   = between n mindist token
     where barinds = [ getInd tok | tok <- allToks
                                  , tagsMatchCond allToks tok (toTags btags) ]
                      -- BARRIER = some token at given index must match with some btag
           dists   = map (\i -> i - getInd token) barinds :: [Int]
           mindist = minimum dists

  cbarrier n btags token | cbarinds==[] = filter match $ atleast n token
                         | mindist < 0  = filter match $ atleast n token
                         | n < 0        = between mindist n token
                         | otherwise    = between n mindist token
     where trgind = getInd token
           barinds = [ getInd tok | tok <- allToks
                                  , tagsMatchCond allToks tok (toTags btags)
                                  , let toksAtSameInd = filter (sameInd tok) allToks 
                                  , all (\tok -> (tagsMatchCond allToks tok) (toTags btags))
                                          toksAtSameInd
                                         ]
                      -- CBARRIER = all tokens at given index must match with some btag
           dists   = map (\i -> i - getInd token) barinds :: [Int]
           mindist = if null dists then -99 else minimum dists
           barindsAtMindist = [ n | n <- barinds, n == trgind + mindist ]
           allIndsAtMindist = [ n | tok <- allToks, let n = getInd tok
                                  , n == trgind + mindist ]
           cbarinds = if barindsAtMindist==allIndsAtMindist 
                        then barindsAtMindist 
                        else []
           

--------------------------------------------------------------------------------

disamSecRule :: [[Rule]] -> Sentence -> IO Sentence
disamSecRule []         sent = return sent
disamSecRule [rs]       sent = disamRule rs sent
disamSecRule (r1:r2:rs) sent = do 
  afterSec1 <- disamRule r1 sent
  disamSecRule ((r1++r2):rs) afterSec1

disamRule ::  [Rule] -> Sentence -> IO Sentence
disamRule []     sent = return sent
disamRule [r]    sent = disamV [r] sent
disamRule (r:rs) sent = disamNoV [r] sent >>= \s -> disamRule rs s

disamV = disambiguate True True
disamNoV = disambiguate False False

disambiguate :: Verbose -> Debug -> [Rule] -> Sentence -> IO Sentence
disambiguate verbose debug rules sentence = do
 let chunkedSent = chunk sentence

 -- CHECKPOINT 1: is sentence ambiguous
 if length chunkedSent == length sentence then return sentence 
  else do 
   s <- newSolver
   litsForAnas  <- sequence [ newLit s | _ <- chunkedSent ]
   let toks = zipWith (uncurry mkToken) chunkedSent litsForAnas
   applied' <- mapM (applyRule s toks) rules  -- :: IO [([Clause], [Lit])]
   

   -- CHECKPOINT 2: are rules triggered       
   if null applied' || all null applied'
    then do when debug $ putStr "No rules triggered: "
            when debug $ mapM_ print rules
            when verbose $ putStrLn (showSentence sentence)
            when verbose $ putStrLn "---------"
            return sentence 
    else do
      

      -- to force True for all literals that are not targets
      let targetlits = let safeLastLit = (\x -> if null x then true else last x) 
                       in concatMap (map safeLastLit) applied'
      let nonAffectedLits = litsForAnas \\ (map neg targetlits)



      litsForClauses <- sequence [ newLit s | _ <- concat applied' ]

      let applied = [ (rl, cl) | (rl, cl') <- zip rules applied'
                               , cl <- cl'
                               , (not.null) cl ] :: [(Rule, [Lit])]
      let rlsCls = [ (rl, neg b:cl) | (b, (rl, cl)) <- zip litsForClauses applied ]


      when debug $ do
        putStrLn "\ntokens:"
        mapM_ print toks

      sequence_ [ addClause s cl | cl <- anchor toks ]
      sequence_ [ addClause s [l] | l <- nonAffectedLits ]
      sequence_ [ addClause s cl  | (_, cl) <- rlsCls ]
      --- up to here identical
      
      b <- do
            let clsByRule = (map.map) snd $ groupBy fstEq rlsCls
            bs <- sequence [ do k <- count s insts :: IO Unary
                                b <- solveMaximize s [] k
                                is <- sequence [ modelValue s x | x <- insts ] 
                                sequence_ [ addClause s [lit]
                                       | (True, lit) <- zip is insts ]
                                return b
                              | cls <- clsByRule
                              , let insts = map (neg . head) cls ]

            return $ and bs && not (null bs)


      --- from here identical again, just output
      when debug $ do cs <- sequence [ modelValue s x | x <- litsForClauses ]
                      putStrLn "\nUsed clauses:"
                      mapM_ putStrLn [ show rl ++ "\n* " ++ show cl 
                                       | (True, (rl,cl)) <- zip cs applied  ]
                      putStrLn "\nUnused clauses:"
                      mapM_ putStrLn [ show rl ++ "\n* " ++ show cl 
                                       | (False, (rl,cl)) <- zip cs applied ]

      if b 
        then
         do as <- sequence [ modelValue s x | x <- litsForAnas ]
            let truetoks = [ t | (True, t) <- zip as toks ]
            let alltoks = [ insertStr sc tok | (b, tok) <- zip as toks 
                                                   , let sc = if b then "" else "; " ]
            when debug $ do putStrLn "\nThe following tag sequence was chosen:"
                            putStrLn $ showSentence (dechunk alltoks)
            when (verbose && (not debug)) $ 
                            putStrLn $ showSentence (dechunk alltoks)

            return (dechunk truetoks)
        else
         do putStrLn "No solution"
            conf <- conflict s
            print conf
            return []

 where 
  insertStr str (T i (WF t:ts) lit c p) = (T i (WF t:Tag str:ts) lit c p)
  insertStr str (T i        ts lit c p) = (T i (     Tag str:ts) lit c p)



--------------------------------------------------------------------------------

