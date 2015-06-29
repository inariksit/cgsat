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

  , applyRule
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


type Clause = [Lit]
type Token = (((Int,Cautious), [Tag]), Lit)


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

--Rule     has [[Tag]].
--Analysis has [Tag].
--At least one complete sublist in the rule must be found in the analysis.
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


-- | Apply rules to tokens. 
applyRule :: Solver -> [Token] -> Rule -> IO [[Lit]]
applyRule s toks rule = do
  case rule of 
    (Remove _name target conds) -> go s False (toTags target) (toConds conds) toks
    (Select _name target conds) -> go s True (toTags target) (toConds conds) toks

    
go :: Solver -> Bool -> [[Tag]] -> [[Condition]] -> [Token] -> IO [[Lit]]
go s isSelect target []         allToks = return []
go s isSelect target (conds:cs) allToks =
   if isSelect 
     then let (trg,rm) = select target in mkVars trg rm conds 
     else let trg      = remove target in mkVars [] trg conds

   ++> go s isSelect target cs allToks

        
 where
  (++>) = liftM2 (++)

  -- check if getContext has returned non-empty context for each condition
  allCondsHold :: [[Token]] -> Bool
  allCondsHold ts | null conds = True
                  | otherwise  = (not.null) ts && all (not.null) ts
                                         

  mkVars :: [(Token,[[Token]])] -> [(Token,[[Token]])] -> [Condition] ->  IO [[Lit]]
  mkVars sl rm conds = do

    let onlyCtxs = [ ctx | tctx <- groupBy sndEq (sl++rm)
                         , let ctx = snd (head tctx) ]

    rs <- sequence [ help s ctx true conds | ctx <- onlyCtxs ]
    let ctxMap = zip onlyCtxs rs
 
    let rmClauses =  [ [neg r, neg tl] | (trg, ctx) <- rm
                                       , let r = fromJust $ lookup ctx ctxMap
                                       , let tl = getLit trg ] 

    let slClauses = [ [neg r, tl] | (trg, ctx) <- sl
                                  , let r = fromJust $ lookup ctx ctxMap
                                  , let tl = getLit trg ] 

    return $ rmClauses ++ slClauses


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
  help s []     r2 conds = 
                     do putStr "the final lit: " 
                        print r2 
                        putStrLn $ "conditions: " ++ show conds ++ "\n"
                        return r2
  help s (c:cs) r1 conds = 
                     do litsForConds <- sequence [ newLit s | _ <- c] --for each analysis that matches condition
                        r' <- --r' is the variable that indicates that *some condition* holds
                         if length litsForConds > 1 
                          then do
                            veryCautiousLit <- newLit s
                            let vcl = [neg veryCautiousLit:litsForConds]
                            putStrLn $ "mkVars.help |litsForConds| > 1: " ++ show vcl
                            mapM_ (addClause s) vcl
                            return veryCautiousLit
                          else do
                            putStrLn $ "mkVars.help.litsForConds: " ++ show litsForConds
                            addClause s litsForConds
                            return $ head litsForConds
                        putStrLn $ "r' :" ++ show r'
                        sequence_ [ do --mapM_ (addClause s) cautiousCls
                                       mapM_ (addClause s) clauses
                                       
                                       putStr "mkVars.help.cautiousCls: "
                                       print cautiousCls
                                       putStr "mkVars.help.clauses: "
                                       print clauses
                                     | (lc, tok@(_,lit)) <- zip litsForConds c 
                                     , let cautiousCls = if isCautious tok 
                                                           then disjC lc tok
                                                           else disj lc tok
                                     , let clauses = [[neg r1, neg lit, r']] 
                                                          ]
                                                           --else [[neg r1, neg lit, r']] ]
                        help s cs r' conds
   where

    disj :: Lit -> Token -> [Clause]
    disj cautiouslit yestok = 
       let yeslit = getLit yestok           
       in [[neg cautiouslit, yeslit]]

    disjC cautiouslit yestok = 
       let yeslit = getLit yestok
           nolits = [ getLit tok | tok <- allToks
                                 , sameInd tok yestok 
                                 , getLit tok /= getLit yestok ]
       in [neg cautiouslit, yeslit] :
          [ [ neg cautiouslit, neg nolit ] | nolit <- nolits ]



  remove :: [[Tag]] -> [(Token,[[Token]])]
  remove target = [ (tok, ctx) | tok <- allToks
                               , isAmbig tok allToks --only apply rules to ambiguous words
                               , tagsMatchRule target tok
                               , let ctx = getContext tok allToks conds
                               , allCondsHold ctx]

  select :: [[Tag]] -> ([(Token,[[Token]])], [(Token,[[Token]])])
  select target = (chosen, other)
    -- chosen: analyses that have the wanted readings and context
 
    -- other: analyses that don't have the wanted readings,
    -- but some word in the same location does have the wanted reading(s).
    --      allToks = [(1,[N,Pl]), (2,[V,Sg]), (2,[N,Pl])]
    --      tags    = [V]
    --      chosen  = (2,[V,Sg]), other = (2,[N,Pl])
    where chosen = remove target
          other  = [ (tok, ctx) | tok <- allToks
                                , (wantedTok, ctx) <- chosen
                                , sameInd tok wantedTok
                                , tagsDontMatchRule target tok ]


getContext :: Token           -- ^ a single analysis
               -> [Token]     -- ^ list of all analyses
               -> [Condition] -- ^ list of conditions grouped by AND
               -> [[Token]]   -- ^ context for the first arg. If all conditions match for a token, there will be as many non-empty Token lists as Conditions.
getContext tok allToks []          = []
getContext tok allToks (Always:cs) = getContext tok allToks cs ++
  [[(((999,False),[]),true)]]
getContext tok allToks ((C position (positive,ctags)):cs) = getContext tok allToks cs ++
  case ctags' of
    []     -> [[dummyTok]] --empty conds = holds always
    [[]]   -> [[dummyTok]] 
    (t:ts) -> case position of
                Exactly _ 0 -> if match tok 
                                 then [[tok]] --if the feature at 0 is in the *same reading* -- important for things like REMOVE imp IF (0 imp) (0 vblex)
                                 else [filter match $ exactly 0 tok] --feature in  different reading

                Exactly True n -> [map makeCautious $ filter match $ exactly n tok]
                AtLeast True n -> [map makeCautious $ filter match $ atleast n tok]

                -- normal mode
                Exactly _ n -> [filter match $ exactly n tok]
                AtLeast _ n -> [filter match $ atleast n tok]
                Barrier n bs  -> [barrier n bs tok]

  where ctags' = toTags ctags
        match  = (if positive then id else not) . tagsMatchRule ctags'

        dummyTok = (((999,False),[]),true) 

        makeCautious :: Token -> Token
        makeCautious (((i,_),tags),lit) = (((i,True),tags),lit)

        --given word and n, return words that are n places away in the original sentence
        exactly :: Int -> Token -> [Token]
        exactly n token = [ tok' | tok' <- allToks
                                 , getInd tok' == n + getInd token ]


        --same but list of tags that are at least n places away
        atleast n token = [ tok' | tok' <- allToks
                                 , getInd tok' >= n + getInd token ]


        --between m and n places away
        between m n token = [ tok' | tok' <- allToks
                                   , let ind' = getInd tok'
                                   , let ind = getInd token
                                   , ind+m <= ind' && ind' <= ind+n
                                   , match tok ]

        barrier n btags tok | barinds==[] = filter match $ atleast n tok
                            | n < 0     = between mindist n tok
                            | otherwise = between n mindist tok
           where barinds = [ getInd tok | tok <- allToks
                                 , tagsMatchRule (toTags btags) tok ]
                 dists   = map (\i -> i - getInd tok) barinds :: [Int]
                 mindist = minimum dists
                 

--------------------------------------------------------------------------------

disamSection ::  ([Rule] -> Sentence -> IO Sentence) -> [[Rule]] -> Sentence -> IO Sentence
disamSection disam []         sent = return sent
disamSection disam [rs]       sent = disam rs sent
disamSection disam (r1:r2:rs) sent = disam (take 10 (reverse r1)) sent
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
   applied' <- applyRule s toks `mapM` rules

   -- CHECKPOINT 2: are rules triggered       
   if null applied' || all null applied'
    then do when debug $ putStrLn "No rules triggered"
            when verbose $ putStrLn (showSentence sentence)
            return sentence 
    else do
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
             return $ and bs && not (null bs)

        else
         do lc <- count s litsForClauses
            solveMaximize s [] lc


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
                            putStrLn $ showSentence (dechunk truetoks)
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
