{-# LANGUAGE DoAndIfThenElse #-} 

module CG_SAT
-- (
--     Clause
--   , Token
--   , Context
--   , getInd
--   , getTags
--   , getLit
--   , sameInd

--   , disambiguate
--   , disambiguateUnordered
--   , disamSection

-- --  , analyseGrammar
--   , test
--   )
where 


import CG_base hiding ( name )
import CG_data
import Data.List
import Data.Maybe
import Control.Monad ( liftM, liftM2, when )
import SAT.Named -- Added name for variables, for nicer looking output
import SAT (Solver,newSolver,conflict,modelValueMaybe)
import qualified SAT
import SAT.Unary ( Unary )
import Debug.Trace

--------------------------------------------------------------------------------

type Clause = [Lit]

data Token = T { getInd  :: Int
               , getTags :: [Tag]
               , getLit  :: Lit
               , isCautious :: Bool
               , isPositive :: Bool } deriving (Eq,Ord)

showToken :: Token -> String
showToken (T i ts lit False True) = show i ++ " " ++ prTags ts ++ ": " ++ show lit
showToken (T i ts lit True True)  = show i ++ "C " ++ prTags ts ++ ": " ++ show lit
showToken (T i ts lit False False)  = show i ++ " (negative match)" ++ prTags ts ++ ": " ++ show lit
showToken (T i ts lit True False)  = show i ++ "C  (negative match)" ++ prTags ts ++ ": " ++ show lit

show' :: Token -> String 
show' (T i (t:ts) lit _ _) = show (i, t, lit)

prTags :: [Tag] -> String
prTags = unwords . map show

instance Show Token where
  show = show . getLit 

type Context = (Condition, [Token])

-------------------------
-- Operations with tokens

mkToken :: Int -> [Tag] -> Lit -> Token
mkToken i ts lit = T { getInd = i , getTags = ts , getLit = lit
                     , isPositive = True, isCautious = False }

mkCautious :: Token -> Token
mkCautious (T i tags lit _ pos) = T i tags lit True pos

mkNegative :: Token -> Token
mkNegative (T i tags lit c _) = T i tags lit c False

dummyTok :: Token
dummyTok = T { getInd  = 999
             , getTags = []
             , getLit  = Lit "ALWAYS" SAT.true
             , isCautious = False
             , isPositive = True }


isBoundary :: Token -> Bool
isBoundary t = not $ null ([BOS,EOS] `intersect` getTags t)

-- we don't need to apply rules to tokens that are already unambiguous
isAmbig :: Token -> [Token] -> Bool
isAmbig tok toks = atLeast 2 (filter (sameInd tok) toks) 
  where atLeast 0 _  = True
        atLeast _ [] = False
        atLeast k (x:xs) = atLeast (k-1) xs


--we want to often compare the indices
sameInd :: Token -> Token -> Bool
sameInd t1 t2 = getInd t1 == getInd t2

--------------------------------------------------------------------------------

-- Rule     has [(trg::[[Tag]], dif::[[Tag]])] , disjoint.
-- Analysis has [Tag].
-- In the analysis there must be at least one complete trg-sublist,
-- which does NOT include tags from any dif-sublist in its (Trg,Dif) pair.
tagsMatchRule :: Token -> [(Trg,Dif)] -> Bool
tagsMatchRule t trg_difs = any (bothMatch t) trg_difs
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


-- | Input: sentence, as in list of analyses.
--   Output: for each word in sentence, 
--   * its position in the sentence
--   * one of the suggested tag sequences
-- (1,["the",<det>])
-- (2,["bear",<n>,<sg>])
-- (2,["bear",<vblex>,<pl>])
-- (3,["sleep",<n>,<pl>])
-- (3,["sleep",<vblex>,<sg>,<p3>])
chunk :: Sentence -> [(Int,[Tag])]
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

-- | Apply rules to a symbolic sentence used in grammar analysis. 
--   In addition to only applying rules, construct also clauses for the case where
--   a rule cannot be applied, because its target are the only remaining analyses.
analyseGrammar :: Solver -> [Token] -> Rule -> IO [([Clause], [Lit])]
analyseGrammar s toks rule = do
  case rule of 
    (Remove _name target conds) -> go s False True (toTags target) (toConds conds) toks
    (Select _name target conds) -> go s True True (toTags target) (toConds conds) toks


-- | Apply rules to tokens. 
-- Returns a list of triples instead of a triple in order to differentiate the cases
--    IF ( (-1 Foo) OR (1 Bar) )
--    IF (-1 Foo OR Bar)
-- Actually it could probably be done in just one triple, but baaaaah.
applyRule :: Solver 
          -> [Token]      -- ^ Text to disambiguate
          -> Rule         -- ^ Rule to apply 
                          -- returns a list of triples:
          -> IO [(Rule,   -- ^ * the rule with the clauses it has generated
                [Clause], -- ^ * list of clauses
                [Lit])]   -- ^ * debug help output; helper variables created for conditions
applyRule s toks rule = do
  case rule of 
    (Remove _name target conds) 
        -> map (insertRule rule) `fmap` go s False False (toTags target) (toConds conds) toks
    (Select _name target conds) 
        -> map (insertRule rule) `fmap` go s True False (toTags target) (toConds conds) toks


  where
   insertRule rule (cls, helps) = (rule, cls, helps)
    
go :: Solver -> Bool -> Bool -> [(Trg,Dif)] -> [[Condition]] -> [Token] -> IO [([Clause], [Lit])]
go s isSelect isGrAna trgs_diffs []         allToks = return []
go s isSelect isGrAna trgs_diffs (conds:cs) allToks = do
   -- putStrLn $ "\ngo with conds=" ++ show conds
   -- putStrLn "-----------------------------"
   let f = if isGrAna then mkVarsGrammarAnalysis else mkVars
   if isSelect 
     then let (trg,rm) = select trgs_diffs in f trg rm
     else let trg      = remove trgs_diffs in f [] trg

   ++> go s isSelect isGrAna trgs_diffs cs allToks

        
 where
  (++>) = liftM2 (:)

  -- check if getContext has returned non-empty context for each condition
  allCondsHold :: [Context] -> Bool
  allCondsHold ts | null conds = True
                  | otherwise  = (not.null) ts && all (not.null.snd) ts

  remove :: [(Trg,Dif)] -> [(Token,[Context])]
  remove t_ds = [ (tok, ctx) | tok <- allToks
                              , isAmbig tok allToks --only apply rules to ambiguous words
                              , tagsMatchRule tok t_ds --C doesn't matter at target
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
                               -- , tok /= wantedTok ]
                                , not $ tagsMatchRule tok t_ds ]


  --"Normal case":
  --At this point we know that conditions *without C* hold already, now just create clauses.
  --We collapse multiple conditions into one SAT variable and give that variable
  --as the antecedent to all of the target tokens.
  mkVars :: [(Token,[Context])] -- ^ Tokens to be selected, each with its context tokens
         -> [(Token,[Context])] -- ^ Tokens to be removed, each with its context tokens
         -> IO ([Clause], [Lit])
  mkVars sl rm = do 
    let onlyCtxs = [ ctx | tctx <- groupBy sndEq (sl++rm)
                         , let ctx = snd (head tctx) ]
    (rs, helps) <- unzip `fmap` sequence [ help 0 s ctx true []| ctx <- nub $ onlyCtxs ] --collapsed var for each ctx
    let ctxMap = zip onlyCtxs rs
 
    let rmClauses =  [ [neg r, neg tl] | (trg, ctx) <- rm
--    rmClauses <- sequence [ equiv s r (neg tl) | (trg, ctx) <- rm
                                       , let Just r = lookup ctx ctxMap
                                       , let tl = getLit trg ] 

    let slClauses = [ [neg r, tl] | (trg, ctx) <- sl
                                  , let Just r = lookup ctx ctxMap
                                  , let tl = getLit trg ] 


    return $ (rmClauses ++ slClauses, concat helps) 

  mapT :: (a -> b) -> (a, a) -> (b, b)
  mapT f (a1, a2) = (f a1, f a2)

  showHds :: (Show a) => [[a]] -> String
  showHds [[x]] = show x
  showHds xss   = intercalate "," ((map (show.head) xss) ++ ["etc."])

  mkVarsGrammarAnalysis :: [(Token,[Context])] -> [(Token,[Context])] -> IO ([Clause], [Lit])
  mkVarsGrammarAnalysis sl rm = do 


    (rmHelps,rmCls) <- unzip `fmap`sequence
                      [ do condLits <- findCondLits s ctx
                           if_ctx_holds <- andl s ctxName condLits
                           rm_trg <- implies s trgName if_ctx_holds (neg $ getLit trg)
                           return (if_ctx_holds, rm_trg)
                         | (trg, ctx) <- rm 
                         , let (conds,insts) = unzip ctx
                         , let condNames = unwords $ map show conds
                         , let ctxName = "("++condNames++")" 
                         , let thisInstanceName = "_if_(" ++ showHds insts ++ ")"
                         , let trgName = "rm_" ++ show trg ++ thisInstanceName ]
                         

    (slHelps,slCls) <- unzip `fmap` sequence
                      [ do condLits <- findCondLits s ctx
                           if_ctx_holds <- andl s ctxName condLits
                           sl_trg <- implies s trgName if_ctx_holds (getLit trg)
                           return (if_ctx_holds, sl_trg)
                         | (trg, ctx) <- sl 
                         , let (conds,insts) = unzip ctx
                         , let condNames = unwords $ map show conds
                         , let ctxName = "if_("++condNames++")" 
                         , let thisInstanceName = "_if_(" ++ showHds insts ++ ")"
                         , let trgName = "sl_" ++ show trg ++ thisInstanceName ]

    -- option 2: only target left, cannot apply REMOVE rule
    -- Only for REMOVE, because SELECT rule cannot lead into an empty cohort.
    -- SELECT in itself already means "only targets, nothing else".
    -- Actually if we do SELECT (*) - (*), would it then select nothing => remove everything?
    -- TODO investigate
    if null sl then do 
      let trgNames = [ ts ++ ds  | (trg,dif) <- trgs_diffs
                                 , let ts = showTagset trg
                                 , let ds = if all null dif then "" else "-" ++ showTagset dif ]
      let onlyTrgName = ("only_", intercalate "|" trgNames ++ "_left")


      onlyTrgLits <- sequence [ onlyTargetLeft s trg onlyTrgName | (trg, ctx) <- rm ]

      notBoth <- sequence [ xorl s notBothName [ruleApplied, onlyTrg] 
                            | (onlyTrg, ruleApplied) <- zip onlyTrgLits rmCls
                            , let notBothName = name onlyTrg ++ "_XOR_" ++ name ruleApplied ]

      return $ (map (:[]) notBoth,
              notBoth++onlyTrgLits++rmHelps++rmCls++slCls++slHelps) 
     else do
      return (map (:[]) (rmCls++slCls),
              rmHelps++rmCls++slCls++slHelps) 
   where
    onlyTargetLeft :: Solver -> Token -> (String,String) -> IO Lit
    onlyTargetLeft s trg (only,left) = do
      let sameIndToks = filter (sameInd trg) allToks
      let (hasTargets,noTarget) = partition (\t -> tagsMatchRule t trgs_diffs) sameIndToks
      let tname = only ++ (show.head.getTags) trg ++ left
      noOthers <- andl s ""  (map (neg.getLit) noTarget)
      someTargets <- orl s "" (map getLit hasTargets)
      andl s tname [someTargets, noOthers]




  --OBS. ctx::[Context] includes things that match to the tags in the cond,
  -- *taking into account NOT*. 
  --OBS2. Ignoring BARRIER case for now; assuming that in a list of [Token] in Context
  -- there are only tokens of one index
  findCondLits :: Solver -> [Context] -> IO [Lit]
  findCondLits s ctx = do
    let (allConds,allCtxs) = unzip ctx
    concat `fmap` sequence
          [ do let condLits = map getLit cToks
               let sameIndToks = filter (sameInd (head cToks)) allToks
               let nomatch = [ getLit tok | tok <- sameIndToks, noMatch tok ]
               case (positive, any isCautious cToks) of
                      (True, False) -> (:[]) `liftM` orl s "--1" condLits --1
                      (True,  True) -> do oneCondHolds <- orl s "--1C" condLits
                                          return $ oneCondHolds:map neg nomatch --1C

                      --NOT 1 --- obs. condLits contains tokens that do NOT match ctags!
                      -- all NOT-anas are false, and >=1 non-NOT ana must be true.
                      -- is it even necessary to specify? doesn't anchor do that?
                      (False,False) -> do c_NOT1 <- orl s "--NOT_1" condLits 
                                          return $ c_NOT1:map neg nomatch 
                      --NOT 1C
                      (False, True) -> (:[]) `liftM` orl s "--NOT_1C" condLits
            | (c@(C _pos (positive,ctags)), cToks) <- ctx 
            , let noMatch t = (if positive then not else id) $ tagsMatchRule t (toTags ctags)]

  --  return ls

                                 

  help :: Int -> Solver -> [Context] -> Lit -> [Lit] -> IO (Lit, [Lit])
  help i s []     r2 helpers = do
    -- putStrLn $ "the final lit: " ++ show r2 
    -- putStrLn $ "conditions: " ++ show conds ++ " " 
    --putStrLn $ show (map (toTags . getTagset) conds)
    return (r2, helpers)
  help i s ((cond,toks):cs) r1 helpers = do --trace ("help" ++ (show i) ++ ": " ++ show toks ++ " " ++ show helpers) $do 
    r <- newLit s ""
    r' <- newLit s "" --if C or NOT
    let matchlits   = map getLit toks
    let nomatchlits = (nub $ map getLit $ concat
                      [ filter (sameInd tok) allToks | tok <- toks ]) \\ matchlits

    putStr $ "help" ++ show i ++ ": matchlits="
    print matchlits
    putStr "nomatchlits="
    print nomatchlits

                    --IF (1C Foo OR Bar), you get both foos and bars at 1C
    clauses <- case (any isPositive toks, any isCautious toks) of
                                          --if it was neg r1:map neg matchlits:r, then
                                          --it would be enough for any matchlit to be false
                                          --to remove the whole clause.
                 (True, False)  -> return [ [neg r1, neg l, r] | l <- matchlits ] --1
                 (False, True)  -> return [ [neg r1, neg l, r] | l <- nomatchlits ] --NOT 1C
                 (_,     _   )  -> return $                                     --1C or NOT 1
                                    [ [neg r1, neg l, r'] | l <- matchlits ] ++
                                    if null nomatchlits then []
                                      else [ neg r':nomatchlits++[r] ]
                                        -- ++ [neg r:map neg nomatchlits]
                                        -- ++ [neg r':matchlits]
    mapM_ (addClause s >> print) clauses
    let helpers' = case (any isPositive toks, any isCautious toks) of
                     (True, True)   -> r':r:helpers
                     (False, False) -> r':r:helpers
                     _              -> r:helpers

    help (i+1) s cs r helpers'




getContext :: Token       -- ^ a single analysis
           -> [Token]     -- ^ list of all analyses
           -> [Condition] -- ^ list of conditions grouped by AND
           -> [Context]   -- ^ context for the first arg. If all conditions match for a token, there will be as many non-empty Contexts ([Token]) as Conditions.
getContext ana allToks []          = []
getContext ana allToks (Always:cs) = (Always, [dummyTok]) : getContext ana allToks cs 
getContext ana allToks (cond@(C position ts@(positive,ctags)):cs) = --trace ("getContext.ana: " ++ show ana ++ ", cond:" ++ show cond ++ ", result:" ++ show result) $ 
 (cond, result) : getContext ana allToks cs
 where 
  result = case head $ toTags ctags of
    ([]  ,_)   -> [dummyTok] --empty conds = holds always
    ([[]],_)   -> [dummyTok] 
    (t:ts,_) -> case position of
                 Exactly False 0 -> 
                  [ f t | t <- exactly 0 ana, match t]

                 Exactly True 0 ->
                  if match ana then [ f $ mkCautious ana ]
                   else [ fc t | t <- exactly 0 ana, match t] 

                 Exactly True n -> [ fc t | t <- exactly n ana, match t ]
                 AtLeast True n -> [ fc t | t <- atleast n ana, match t ]

                 Exactly _ n -> [ f t | t <- exactly n ana, match t ]
                 AtLeast _ n -> [ f t | t <- atleast n ana, match t ]
                 Barrier  _ n bs -> map f $ barrier n bs ana
                 CBarrier _ n bs -> map f $ cbarrier n bs ana

  f :: Token -> Token
  f = if positive then id else mkNegative

  fc :: Token -> Token 
  fc = f . mkCautious

  match :: Token -> Bool
  match tok = (if positive then id else not) $ tagsMatchRule tok (toTags ctags)

  --given word and n, return words that are n places away in the original sentence
  --If n is out of bounds AND the condition is negative, then return [dummyTok],
  --it will signal that a condition /is/ met:
  --there is no <tag> in a word n places away if there is no word n places away!
  exactly :: Int -> Token -> [Token]
  exactly n token = let potentialToks = 
                         [ tok' | tok' <- allToks
                                , getInd tok' == n + getInd token ]
                    in if null potentialToks && not positive 
                         then [dummyTok]
                         else potentialToks

  --same but list of tags that are at least n places away
  atleast n token = let potentialToks = 
                         [ tok' | tok' <- allToks
                                , getInd tok' >= n + getInd token ]
                    in if null potentialToks && not positive 
                         then [dummyTok]
                         else potentialToks

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
                                  , tagsMatchRule tok (toTags btags) ]
                      -- BARRIER = some token at given index must match with some btag
           dists   = map (\i -> i - getInd token) barinds :: [Int]
           mindist = minimum dists

  cbarrier n btags token | cbarinds==[] = trace "cbarrier.bi==[]" $ filter match $ atleast n token
                         | mindist < 0  = trace "cbarrier.md<0" $ filter match $ atleast n token
                         | n < 0        = between mindist n token
                         | otherwise    = between n mindist token
     where trgind = getInd token
           barinds = [ getInd tok | tok <- allToks
                                  , tagsMatchRule tok (toTags btags)
                                  , let toksAtSameInd = filter (sameInd tok) allToks 
                                  , all (\tok -> (tagsMatchRule tok) (toTags btags))
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

disamSection ::  ([Rule] -> Sentence -> IO Sentence) -> [[Rule]] -> Sentence -> IO Sentence
disamSection disam []         sent = return sent
disamSection disam [rs]       sent = disam rs sent
disamSection disam (r1:r2:rs) sent = disam r1 sent
                                      >>= disamSection disam ((r1++r2):rs)


type WithOrder = Bool
type Verbose = Bool
type Debug = Bool 

disambiguate = disambiguate' True
disambiguateUnordered = disambiguate' False

disambiguate' :: WithOrder -> Verbose -> Debug -> [Rule] -> Sentence -> IO Sentence
disambiguate' withOrder verbose debug rules sentence = do
 let chunkedSent = chunk sentence

 -- CHECKPOINT 1: is sentence ambiguous
 if length chunkedSent == length sentence then return sentence 
  else do 
   s <- newSolver
   litsForAnas  <- sequence [ newLit s (show chunk) | chunk <- chunkedSent ]
   let toks = zipWith (uncurry mkToken) chunkedSent litsForAnas
   rls_applied_helps <- concat `fmap` mapM (applyRule s toks) rules :: IO [(Rule, [Clause], [Lit])]
   let helps = concatMap (\(_,_,c) -> c) rls_applied_helps :: [Lit]
   let applied = concatMap (\(_,b,_) -> b) rls_applied_helps :: [Clause]
   -- CHECKPOINT 2: are rules triggered       
   if null applied || all null applied
    then do when debug $ putStrLn "No rules triggered"
            when verbose $ putStrLn (showSentence sentence)
            return sentence 
    else do

      -- to force True for all literals that are not to be removed/selected by any rule
      let targetlits = let safeLastLit = (\x -> if null x then true else last x) 
                       in map safeLastLit applied
      let nonAffectedLits = litsForAnas  \\ (map neg targetlits)


      (litsForClauses,
        rls_cls) <- unzip `fmap` sequence 
                      [ do b <- newLit s (show rl)
                           return (b, (rl, neg b:cl))
                        | (rl, cls, _) <- rls_applied_helps , cl <- cls ] :: IO ([Lit], [(Rule, Clause)])
                                                      
      when debug $ do
        putStrLn "\ntokens:"
        mapM_ print toks

      sequence_ [ addClause s cl | cl <- anchor toks ]
      sequence_ [ addClause s [l] | l <- nonAffectedLits ]
      sequence_ [ addClause s cl >> when debug (print cl) | (_, cl) <- rls_cls ]
      --- up to here identical
      

      b <- 
       if withOrder 
         then 
          do let clsByRule = (map.map) snd $ groupBy fstEq rls_cls
             bs_ls <- sequence [ do k <- SAT.Named.count s insts :: IO Unary
                                    b <- solveMaximize s [] k
                                    is <- sequence [ modelValue s x | x <- insts ] 
                                    sequence_ [ addClause s [lit]
                                       | (True, lit) <- zip is insts ]
                                    let ls = [ lit | (True, lit) <- zip is insts ]
                                    return (b,ls)
                              | cls <- clsByRule
                              , let insts = map (neg . head) cls ]
             let (bs,ls) = unzip bs_ls
             --n <- count s litsForAnas
             b' <- return True -- solveMaximize s (concat ls) n
             return $ (and bs && not (null bs)) && b'

        else
         do print "No order!"
            lc <- count s litsForClauses
            solveMaximize s [] lc


      --- from here identical again, just output
      when debug $ do cs <- sequence [ modelValue s x | x <- litsForClauses ]
                      putStrLn "\nUsed clauses:"
                      mapM_ putStrLn [ show rl ++ "\n* " ++ show cl 
                                       | (True, (rl,l:cl)) <- zip cs rls_cls  ]
                      putStrLn "\nUnused clauses:"
                      mapM_ putStrLn [ show rl ++ "\n* " ++ show cl 
                                       | (False, (rl,l:cl)) <- zip cs rls_cls ]

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

            
            putStrLn "helper lits after solving:"
            safePrValues (helps++litsForClauses) s

            return (dechunk truetoks)
        else
         do putStrLn "No solution"
            conf <- conflict s
            print conf
            return []

 where 
  insertStr str (T i (WF t:ts) lit c p) = (T i (WF t:Tag str:ts) lit c p)
  insertStr str (T i        ts lit c p) = (T i (     Tag str:ts) lit c p)



test :: IO ()
test = mapM_ (disambiguate True True rls) CG_data.exs

  where rls = [rmVerbIfDet
             --, slVerbAlways --conflicts with anything that selects other than V 
             , rmNounIfPron
             , slNounAfterConj
             , slCCifCC             
             , slPrepIfDet 
             , rmAdvIfDet 
             , rmPlIfSg
             , rmSgIfPl
             , slNounIfBear 
             --, negTest      --should conflict
             --, negOrTest    --should conflict
             , rmParticle
             ]


--------------------------------------------------------------------------------

fstEq (a,_) (b,_) = a==b
sndEq (_,a) (_,b) = a==b

pr' :: Bool ->  String
pr' True  = "1"
pr' False = "0"

unsafePrValues :: [Lit] -> Solver -> [Lit] -> IO ()
unsafePrValues lits s ass = do
  mapM_ (\x -> putStr (show x ++ (if length (show x) == 2 then "  " else " "))) lits
  putStrLn ""
  --k <- count s lits
  --solveMaximize s ass k
  solve s ass
  hs <- sequence [ modelValue s x | x <- lits ]
  sequence_ [ putStr (pr' h) | h <- hs ]
  putStrLn ""

safePrValues :: [Lit] -> Solver -> IO ()
safePrValues lits s = do
  hs <- sequence [ modelValue s x | x <- lits ]
  sequence_ [ putStrLn (show x ++ "\t" ++ show h) | (x,h) <-zip lits hs ]
  putStrLn ""
