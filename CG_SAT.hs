{-# LANGUAGE DoAndIfThenElse #-} 

module CG_SAT(
    Clause
  , Token
  , getInd
  , getTags
  , getLit
  , sameInd

  , disambiguate
  , disambiguateUnordered
  , disamSection

  , analyseGrammar
  , test
  )
where 


import CG_base
import CG_data
import Data.List
import Data.Maybe
import Control.Monad ( liftM2, when )
import SAT
import SAT.Optimize
import SAT.Unary hiding ( modelValue )
import qualified SAT.Unary as U
import Debug.Trace

--------------------------------------------------------------------------------

type Clause = [Lit]
type Token = (((Int,Cautious), [Tag]), Lit)
type Trg = [[Tag]]
type Dif = [[Tag]]

-------------------------
-- Operations with tokens

mkCautious :: Token -> Token
mkCautious (((i,_),tags),lit) = (((i,True),tags),lit)

dummyTok :: Token
dummyTok = (((999,False),[]),true) 

getInd :: Token -> Int
getInd (((i,_),_),_) = i

getTags :: Token -> [Tag]
getTags ((_,t),_) = t

getLit :: Token -> Lit
getLit (_,l) = l

isBoundary :: Token -> Bool
isBoundary ((_,ts),_) = not $ null ([BOS,EOS] `intersect` ts)

isCautious :: Token -> Bool
isCautious (((_,b),_),_) = b

-- we don't need to apply rules to tokens that are already unambiguous
isAmbig :: Token -> [Token] -> Bool
isAmbig tok toks = atLeast 2 (filter (sameInd tok) toks) 
  where atLeast 0 _  = True
        atLeast _ [] = False
        atLeast k (x:xs) = atLeast (k-1) xs


--we want to often compare the indices
sameInd :: Token -> Token -> Bool
sameInd (((i,_),_),_) (((i',_),_),_) = i == i'

--------------------------------------------------------------------------------

--Rule     has target::[[Tag]], diff::[[Tag]].
--Analysis has [Tag].
--At least one complete sublist in the rule must be found in the analysis.
--And none of the sublists in diff can be found in the analysis.
--tagsMatchRule :: [Token] -> Token -> ([[Tag]],[[Tag]]) -> Bool
tagsMatchRule :: [Token] -> Token -> [(Trg,Dif)] -> Bool
tagsMatchRule allToks t@(((ind,isC),ta),_) trg_difs = --trace ("tagsMatchRule: " ++ if isC then (show toksAtSameInd) ++ " " ++ (show trg_difs) ++ " " ++ (show (map (\tok -> any (bothMatch tok) trg_difs) toksAtSameInd)) else "") $
  if isC 
    then let foo = True  --toksAtSameInd = filter (sameInd t) allToks
         in  all (\tok -> any (bothMatch tok) trg_difs) toksAtSameInd
    else any (bothMatch t) trg_difs
 where
  toksAtSameInd = filter (sameInd t) allToks
  bothMatch t@((_,ta),_) (trg,dif) = pos trg ta && neg dif ta
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
--   * id of that hypothesis in the SAT solver
-- ((1,["the",<det>]),v0)
-- ((2,["bear",<n>,<sg>]),v1)
-- ((2,["bear",<vblex>,<pl>]),v2)
-- ((3,["sleep",<n>,<pl>]),v4)
-- ((3,["sleep",<vblex>,<sg>,<p3>]),v5)
chunk :: Sentence -> [((Int,Cautious), [Tag])]
chunk sent = concat $ go sent 1
   where go []    _n = []
         go (x:xs) n = map ((,) (n,False)) x : go xs (n+1)

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
analyseGrammar :: Solver -> [Token] -> Rule -> IO [Clause]
analyseGrammar s toks rule = do
  case rule of 
    (Remove _name target conds) -> go s False True (toTags target) (toConds conds) toks
    (Select _name target conds) -> go s True True (toTags target) (toConds conds) toks


-- | Apply rules to tokens. 
applyRule :: Solver -> [Token] -> Rule -> IO [Clause]
applyRule s toks rule = do
  case rule of 
    (Remove _name target conds) -> go s False False (toTags target) (toConds conds) toks
    (Select _name target conds) -> go s True False (toTags target) (toConds conds) toks

    
go :: Solver -> Bool -> Bool -> [(Trg,Dif)] -> [[Condition]] -> [Token] -> IO [Clause]
go s isSelect isGrAna trgs_diffs []         allToks = return []
go s isSelect isGrAna trgs_diffs (conds:cs) allToks = do
   let f = if isGrAna then mkVarsGrammarAnalysis else mkVars
   if isSelect 
     then let (trg,rm) = select trgs_diffs in f trg rm conds 
     else let trg      = remove trgs_diffs in f [] trg conds

   ++> go s isSelect isGrAna trgs_diffs cs allToks

        
 where
  (++>) = liftM2 (++)

  -- check if getContext has returned non-empty context for each condition
  allCondsHold :: [[Token]] -> Bool
  allCondsHold ts | null conds = True
                  | otherwise  = (not.null) ts && all (not.null) ts

  remove :: [(Trg,Dif)] -> [(Token,[[Token]])]
  remove t_ds = [ (tok, ctx) | tok <- allToks
                              , isAmbig tok allToks --only apply rules to ambiguous words
                              , tagsMatchRule allToks tok t_ds --C doesn't matter at target
                              , let ctx = getContext tok allToks conds
                              , allCondsHold ctx ]

  select :: [(Trg,Dif)] -> ([(Token,[[Token]])], [(Token,[[Token]])])
  select t_ds = (chosen, other)
    -- chosen: analyses that have the wanted readings and context
    -- other: don't have wanted readings, but some other word in the same index has.
    where chosen = remove t_ds
          other  = [ (tok, ctx) | tok <- allToks
                                , (wantedTok, ctx) <- chosen
                                , sameInd tok wantedTok 
                               -- , tok /= wantedTok ]
                                , not $ tagsMatchRule allToks tok t_ds ]


  --"Normal case":
  --At this point we know that conditions hold already, now we just create clauses.
  --We collapse multiple conditions into one SAT variable and give that variable
  --as the antecedent to all of the target tokens.
  mkVars :: [(Token,[[Token]])] -- ^ Tokens to be selected, each with its context tokens
         -> [(Token,[[Token]])] -- ^ Tokens to be removed, each with its context tokens
         -> [Condition]         -- ^ All conds for the rule (used only for debug printout) 
         -> IO [Clause]
  mkVars sl rm conds = do
    let onlyCtxs = [ ctx | tctx <- groupBy sndEq (sl++rm)
                         , let ctx = snd (head tctx) ]
    rs <- sequence [ help s ctx true conds | ctx <- onlyCtxs ] --collapsed var for each ctx
    let ctxMap = zip onlyCtxs rs
 
    let rmClauses =  [ [neg r, neg tl] | (trg, ctx) <- rm
                                       , let Just r = lookup ctx ctxMap
                                       , let tl = getLit trg ] 
    let slClauses = [ [neg r, tl] | (trg, ctx) <- sl
                                  , let Just r = lookup ctx ctxMap
                                  , let tl = getLit trg ] 

    return $ rmClauses ++ slClauses


  --Creating clauses for grammar analysis
  --As before, we collapse multiple conditions into one SAT variable and create clauses.
  --In addition, stuff only for grammar analysis:
  --We want to find a sentence for which a rule /does not/ fire.
  --It can be because of there is no target left, or because there is /only/ target left.
  --TODO: why this happens?
     -- REMOVE target  IF (1C tag  - dummy ): [v19,v18]
     -- REMOVE target  IF (1C tag  - dummy ): [~v19] <----- ??????
  mkVarsGrammarAnalysis :: [(Token,[[Token]])] -> [(Token,[[Token]])] -> [Condition] ->  IO [Clause]
  mkVarsGrammarAnalysis sl rm conds = do
    let onlyCtxs = [ ctx | tctx <- groupBy sndEq (sl++rm)
                         , let ctx = snd (head tctx) ]
    rs <- sequence [ help s ctx true conds | ctx <- onlyCtxs ]
    let ctxMap = zip onlyCtxs rs


    --should onlyTrgCls only apply to the original target of the rule that is analysed?
    --no  : a rule cannot apply selectively if context is met
    --yes : if ctx word isn't analysed as <target>, no need for rule in the first place
    let target = if null sl then rm else sl 
    trgLits <- sequence [ newLit s | _ <- target ]
    let onlyTrgCls = concat [ onlyTargetLeft lit trg conds
                              | (lit, (trg, ctx)) <- zip trgLits target ] 
                     
            
    -- print target
    -- print onlyTrgCls

    -- this lit will indicate that rules are applied
    rlit <- newLit s

    --we apply one, but not both, of:
    --    rmClauses and slClauses 
    --    onlyTrg
    let rmClauses = [ [neg rlit, neg r, neg tl]
                        | (trg, ctx) <- rm
                        , let Just r = lookup ctx ctxMap
                        , let tl = getLit trg 
                        ]
    let slClauses = [ [neg rlit, neg r, tl] 
                        | (trg, ctx) <-  sl
                        , let Just r = lookup ctx ctxMap
                        , let tl = getLit trg 
                        ]  
    -- putStr "rmClauses: " 
    -- print rmClauses
    -- putStr "slClauses: " 
    -- print slClauses
    -- putStrLn ""

    a <- newLit s
    let ruleApplied = [a, rlit]
    let onlyTrg = neg a : trgLits
    -- putStr "ruleApplied: "
    -- print ruleApplied
    -- putStr "onlyTrg: "
    -- print onlyTrg
    return $ ruleApplied : onlyTrg : onlyTrgCls ++ rmClauses ++ slClauses
    
   where
    --TODO: pass information about the target from Symbolic
    --intersect (getTags tok) (getTags trg)  won't work in general case
    onlyTargetLeft :: Lit -> Token -> [Condition] -> [Clause]
    onlyTargetLeft newlit trg conds = -- trace ("onlyTargetLeft: " ++ (show trg) ++ (show conds)) $ 
      [ neg newlit:[getLit tok | tok <- allToks
                               , sameInd tok trg
                               , not $ null $ sameTags tok trg ] ] ++
      [ [neg newlit, neg $ getLit tok] | tok <- allToks 
                                       , sameInd tok trg
                                       , null $ sameTags tok trg] 

    sameTags tok trg = 
      intersect [ tag | tag@(Tag foo) <- getTags tok ]
                [ tag | tag@(Tag foo) <- getTags trg ]


{-
Examples:

analyses:
  v6 ... (7,["<la>","el",det,def,f,sg])
  v7 ... (7,["<la>","lo",prn,pro,p3,f,sg])
  v8 ... (8,["<casa>","casa",n,f,sg])
  v9 ... (8,["<casa>","casar",vblex,pri,p3,sg])
  v10 .. (8,["<casa>","casar",vblex,imp,p2,sg])
  v11 .. (9,["<,>", ",", cm])
rule: 
  SELECT:i08_s_pro PrnIndep IF (1 VerbFin)

both v9 and v10 are VerbFin, so we create these clauses:
  [False,~v9,v15]
  [False,~v10,v15]

the final lit to be used in the rule is v15:
  [~v15,~v6], [~v15,v7]

the old way would be 
  [~v9,~v6], [~v9,v7], [~v10,~v6], [~v10,v7]

Example with two conditions:

rule:
  SELECT:i02_s_n_adj n  IF (-1C Det) (NOT 1 N)

v11 is not N and v6 is a det, so we create these clauses:
 [False,~v11,v16] -- first round:   dummy && NOT 1 N => v16
 [~v16,~v6,v17]   -- second round:  v16   && -1 Det  => v17 

the final lit is v17:
 [~v17,v8], [~v17,~v9], [~v17,~v10]  -}

  help :: Solver -> [[Token]] -> Lit -> [Condition] -> IO Lit
  help s []     r2 conds = do
    -- putStr "the final lit: " 
    -- print r2 
    -- putStr $ "conditions: " ++ show conds ++ " " 
    -- putStrLn $ show (map (toTags . getTagset) conds)
    -- putStrLn "\n"
    return r2
  help s (c:cs) r1 conds = do 
    r' <- newLit s --r' is the variable that indicates that *some condition* holds
    --putStrLn $ "r': " ++ show r'
    sequence_ [ do addClause s clause
                   -- putStr "mkVars.help.clause: "
                   -- print clause
                   | tok@(_,lit) <- c --context 
                   , let clause = if isCautious tok --1 w<ana> at a time
                                   then [neg r1, neg lit, r'] --TODO
                                   else [neg r1, neg lit, r'] ] 
    help s cs r' conds
    --TODO cautious




getContext :: Token       -- ^ a single analysis
           -> [Token]     -- ^ list of all analyses
           -> [Condition] -- ^ list of conditions grouped by AND
           -> [[Token]]   -- ^ context for the first arg. If all conditions match for a token, there will be as many non-empty Token lists as Conditions.
getContext ana allToks []          = []
getContext ana allToks (Always:cs) = [dummyTok] : getContext ana allToks cs 
getContext ana allToks ((C position c@(positive,ctags)):cs) = {-trace ("getContext.c: " ++ show c ++ "\n" ++ "result: " ++ show result) $ -} result : getContext ana allToks cs
 where 
  result = case head $ toTags ctags of
    ([]  ,_)   -> [dummyTok] --empty conds = holds always
    ([[]],_)   -> [dummyTok] 
    (t:ts,_) -> case position of
                Exactly _ 0 -> 
                 if match ana then [ana] --feature at 0 is in the *same reading*
                   else [ t | t <- exactly 0 ana, match t] --feature in other reading

                Exactly True n -> [ mkCautious t | t <- exactly n ana 
                                                 , match $ mkCautious t ]
                AtLeast True n -> [ mkCautious t | t <- atleast n ana
                                                 , match $ mkCautious t ]

                Exactly _ n -> [ t | t <- exactly n ana, match t ]
                AtLeast _ n -> [ t | t <- atleast n ana, match t ]
                Barrier  _ n bs -> barrier n bs ana
                CBarrier _ n bs -> cbarrier n bs ana

  match :: Token -> Bool
  match tok = (if positive then id else not) $ tagsMatchRule allToks tok (toTags ctags)

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
                then toks else [] --NOT: all readings between m-n must *not* match


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
                                  , tagsMatchRule allToks tok (toTags btags) ]
                      -- BARRIER = some token at given index must match with some btag
           dists   = map (\i -> i - getInd token) barinds :: [Int]
           mindist = minimum dists

  cbarrier n btags token | cbarinds==[] = filter match $ atleast n token
                         | mindist < 0  = filter match $ atleast n token
                         | n < 0        = between mindist n token
                         | otherwise    = between n mindist token
     where trgind = getInd token
           barinds = [ getInd tok | tok <- allToks
                                  , let tokC = mkCautious tok
                                  , tagsMatchRule allToks tokC (toTags btags)
                                  -- , any (tagsMatchRule allToks tok) (toTags btags) 
                                  -- , let toksAtSameInd = filter (sameInd tok) allToks 
                                  -- , all (\tok -> any (tagsMatchRule allToks tok) (toTags btags))
                                  --        toksAtSameInd
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
                                      >>= \s -> ({-print s >>-} disamSection disam 
                                                ((r1++r2):rs) s)


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
   litsForAnas  <- sequence [ newLit s | _ <- chunkedSent ]
   let toks = zip chunkedSent litsForAnas
   applied' <- mapM (applyRule s toks) rules

   -- CHECKPOINT 2: are rules triggered       
   if null applied' || all null applied'
    then do when debug $ putStrLn "No rules triggered"
            when verbose $ putStrLn (showSentence sentence)
            return sentence 
    else do
      

      -- to force True for all literals that are not to be removed/selected
      let targetlits = if all null applied' then []
                          else concatMap (map last) applied'
      let nonAffectedLits = litsForAnas \\ (map neg targetlits)
      ---------------------------------- ^ comment out here if old behaviour desired


      litsForClauses <- sequence [ newLit s | _ <- concat applied' ]

      let applied = [ (rl, cl) | (rl, cl') <- zip rules applied'
                               , cl <- cl'
                               , (not.null) cl ] :: [(Rule, [Lit])]

      let rlsCls = [ (rl, neg b:cl) | (b, (rl, cl)) <- zip litsForClauses applied ]


      when debug $ do
        putStrLn "\ntokens:"
        mapM_ print toks
        -- putStrLn "\nfirst step, make sure all readings for a given word are not false:"
        -- print (anchor toks)
        -- putStrLn "\nclauses gotten by applying rules"
        -- mapM_ print applied

      sequence_ [ addClause s cl | cl <- anchor toks ]
      sequence_ [ addClause s cl | (_, cl) <- rlsCls ]
      --- up to here identical
      
      b <- 
       if withOrder 
         then 
          do let clsByRule = (map.map) snd $ groupBy fstEq rlsCls
             bs <- sequence [ do k <- count s insts :: IO Unary
                                 b <- solveMaximize s [] k
                                 is <- sequence [ modelValue s x | x <- insts ] 
                                 sequence_ [ addClause s [lit]
                                       | (True, lit) <- zip is insts ]
                                 return b
                              | cls <- clsByRule
                              , let insts = map (neg . head) cls ]
             b' <- solve s nonAffectedLits
             return $ b' && (and bs && not (null bs))

        else
         do lc <- count s litsForClauses
            solveMaximize s [] lc
            solve s nonAffectedLits


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
  insertStr str ((i, (WF t:ts)), lit) = ((i,  WF t:Tag str:ts), lit)
  insertStr str ((i, ts), lit) = ((i, Tag str:ts), lit)



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
