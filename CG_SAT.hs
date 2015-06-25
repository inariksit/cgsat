{-# LANGUAGE DoAndIfThenElse #-} 

module CG_SAT(
    Token
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
import Debug.Trace



type Token = ((Int, [Tag]), Lit)

getInd :: Token -> Int
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


-- | Apply rules to tokens. 
applyRule :: Solver -> Rule -> [Token] -> IO [[Lit]]
applyRule s rule toks = do
  case rule of 
    (Remove _name target conds) -> go s False (toTags target) (toConds conds) toks
    (Select _name target conds) -> go s True (toTags target) (toConds conds) toks

    
go :: Solver -> Bool -> [[Tag]] -> [[Condition]] -> [Token] -> IO [[Lit]]
go s isSelect tags []         allToks = return []
go s isSelect tags (conds:cs) allToks =
   if isSelect 
     then let (trg,rm) = select tags in mkVars trg rm 
     else let trg     = remove tags in mkVars [] trg

   ++> go s isSelect tags cs allToks

  -- if isSelect 
  --   then let (ch,ot) = select tags in (mkVars ch id) ++ (mkVars ot neg)
  --   else let chosen  = remove tags in mkVars chosen neg 
  -- ++ go s isSelect tags cs allToks

        
 where
  (++>) = liftM2 (++)
  sndEq (_,a) (_,b) = a==b

  -- check if getContext has returned non-empty context for each condition
  allCondsHold :: [[Token]] -> Bool
  allCondsHold ts | null conds = True 
                  | otherwise  = (not.null) ts && all (not.null) ts
                                         

  -- TODO this is a mess
  mkVars :: [(Token,[[Token]])] -> [(Token,[[Token]])] -> IO [[Lit]]
  mkVars sl rm = do --group all tokens that have the same context

    let onlyCtxs = [ ctx | tctx <- groupBy sndEq (sl++rm)
                         , let ctx = snd (head tctx) ]

    let trgsByCtx = [ (trgs, ctx) | tctx <- groupBy sndEq (sl++rm)
                                  , let trgs = map fst tctx
                                  , let ctx = snd (head tctx) ]
--    mapM_ print trgsByCtx

    rs <- sequence [ help s ctx r1 | ctx <- onlyCtxs
                                   , let r1 = true ]
    let ctxMap = zip onlyCtxs rs
 
    let rmClauses =  [ [neg r, neg tl] | (trg, ctx) <- rm
                                       , let r = fromJust $ lookup ctx ctxMap
                                       , let tl = getLit trg ] 

    let slClauses = [ [neg r, tl] | (trg, ctx) <- sl
                                  , let r = fromJust $ lookup ctx ctxMap
                                  , let tl = getLit trg ] 

    return $ rmClauses ++ slClauses


  help :: Solver -> [[Token]] -> Lit -> IO Lit
  help s []     r2 = do --putStr "the final lit: " 
                        --print r2 
                        --addClause s [r2] ---- ???
                        return r2
  help s (c:cs) r1 = do r' <- newLit s
                        sequence_ [ do addClause s [neg r1, neg vc, r'] 
--                                       putStr "mkVars.help: "
--                                       print [neg r1, neg vc, r']
                                     | (_,vc) <- c ]
                        help s cs r'


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
getContext tok allToks []          = []
getContext tok allToks (Always:cs) = getContext tok allToks cs ++
  [[((999,[]),true)]]
getContext tok allToks ((C position (bool,ctags)):cs) = getContext tok allToks cs ++
  case ctags' of
    []     -> [[dummyTok]] --empty conds = holds always
    [[]]   -> [[dummyTok]] 
    (t:ts) -> case position of
                Exactly _ 0 -> if match tok 
                                 then [[tok]] --if the feature at 0 is in the *same reading* -- important for things like REMOVE imp IF (0 imp) (0 vblex)
                                 else [filter match $ exactly 0 tok] --feature in  different reading

                -- for cautious mode. You probably shouldn't use this;
                -- it doesn't check about the variables in the position,
                -- just the length of list.
                -- And I think checking the value of variables might just not work,
                -- depends on the state of the SAT solver. Maybe with 
                -- disambiguateWithOrder but definitely not with disambiguate.
                -- Exactly True n -> if length (exactly n tok) > 1 
                --                    then [[]]
                --                    else [filter match $ exactly n tok]
                -- AtLeast True n -> if length (atleast n tok) > 1
                --                    then [[]]
                --                    else [filter match $ atleast n tok]

                -- normal mode
                Exactly _ n -> [filter match $ exactly n tok]
                AtLeast _ n -> [filter match $ atleast n tok]
                Barrier n bs  -> [barrier n bs tok]

  where ctags' = toTags ctags
        match  = (if bool then id else not) . tagsMatchRule ctags'

        dummyTok = ((999,[]),true) 

        --given word and n, return words that are n places away in the original sentence
        exactly :: Int -> Token -> [Token]
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
                 dists   = map (\i -> i - getInd tok) barinds :: [Int]
                 mindist = minimum dists
                 

---- Main stuff
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
 let chunkedSent = chunk sentence :: [(Int,[Tag])]

 -- CHECKPOINT 1: is sentence ambiguous
 if length chunkedSent == length sentence then return sentence 


  else do 
   s <- newSolver
   litsForAnas  <- sequence [ newLit s | _ <- chunkedSent ]
   let toks = zip chunkedSent litsForAnas
   applied' <- filter (not.null) `fmap` sequence [ applyRule s rl toks | rl <- rules ]

   -- CHECKPOINT 2: are rules triggered       
   if null applied' || all null applied'
    then do when debug $ putStrLn "No rules triggered"
            return sentence 

    else do
      litsForClauses <- sequence [ newLit s | _ <- applied' ]

      let applied = [ (rl, cl) | (rl, cl') <- zip rules applied'
                               , cl <- cl'
                               , (not.null) cl ] :: [(Rule, [Lit])]

      let clauses = [ neg b:cl | (_, cl) <- applied
                               , b <- litsForClauses ]

      let clsByRule = [ [neg b:cl] | cls <- applied'
                                   , (b, cl) <- zip litsForClauses cls 
                                   , (not.null) cl ]

      when debug $ do
        putStrLn "\ntokens:"
        mapM_ print toks
        putStrLn "\nfirst step, make sure all readings for a given word are not false:"
        print (anchor toks)
        putStrLn "\nclauses gotten by applying rules"
        mapM_ print applied

      sequence_ [ addClause s cl | cl <- anchor toks ]
      --- up to here identical
      
      b <- 
       if withOrder 
         then -- TODO there's something wrong here
          do bs <- sequence [ do k <- count s insts :: IO Unary
                                 b <- solveMaximize s [] k
                                 is <- sequence [ modelValue s x | x <- insts ] 
                                 sequence_ [ addClause s [lit]
                                       | (True, lit, c) <- zip3 is insts cs]
                                 return b
                              | cls <- clsByRule
                              , let insts = map (neg . head) cls
                              , let cs = map (!! 1) cls ]
             --if null bs then print "bs is empty >:(" else print bs
             return $ and bs && not (null bs)

        else
         do sequence_ [ addClause s cl | cl <- clauses ]
            lc <- count s litsForClauses
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
         do lt <- count s litsForAnas
            --b2 <- solveMaximize s [lc .>= k] lt
            --b2 <- solveMaximize s [] lt
            if True
              then
               do as <- sequence [ modelValue s x | x <- litsForAnas ]
                  let truetoks = [ t | (True, t) <- zip as toks ]
                  let alltoks = [ insertStr sc tok | (b, tok) <- zip as toks 
                                                   , let sc = if b then "" else "; " ]
                  when (verbose && debug) $ do
                      putStrLn "\nThe following tag sequence was chosen:"
                      putStrLn $ showSentence (dechunk alltoks)
                  when (verbose && (not debug)) $  
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

 where 
  insertStr str ((i, (Lem l:ts)), lit) = ((i, ((Lem (str++l)):ts)), lit)
  insertStr str ((i, (Tag t:ts)), lit) =  ((i, ((Tag (str++t)):ts)), lit)
  insertStr str ((i, (WF t:Lem l:ts)), lit) = ((i, newts), lit)
   where newts = WF t:(Lem (str++l)):ts
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

