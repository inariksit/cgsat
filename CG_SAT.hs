module CG_SAT where 


import CG_base
import CG_data
import Data.List
import Data.Maybe
import Data.Set (Set, fromList, isSubsetOf)
import Control.Monad
import Control.Exception
import MiniSat
import SAT.SAT
import Debug.Trace


type Token = ((Integer, [Tag]), Bit)

getInd :: Token -> Integer
getInd ((i,_),_) = i

getTags :: Token -> [Tag]
getTags ((_,t),_) = t

getBit :: Token -> Bit
getBit (_,b) = b

isBoundary :: Token -> Bool
isBoundary tok = not $ null ([Lem "<<<", Lem ">>>"] `intersect` getTags tok)

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
  where toksByInd = groupBy sameInd ts :: [[Token]]
        
  
--------------------------------------------------------------------------------

-- | For each position, at least one analysis must be true.
--   Group tokens by index and return a lists of variables 
--   to be used in disjunction.
anchor :: [Token] -> [[Bit]]
anchor toks = (map.map) getBit (groupBy sameInd toks)


-- | Apply rules to tokens. 
applyRule :: [Token] -> Rule -> [[Bit]]
applyRule toks rule = --trace (show rule) $
  case rule of
-- a) condition(s) must not hold 
--for each token, if getContext is empty, remove/select it
    (Remove tags (NEG conds)) -> [nt (getBit tok) | tok <- toks
                                                    , tagsMatchRule tags tok
                                                    , null (ctxt tok conds)]:[]

    (Select tags (NEG conds)) -> [getBit tok | tok <- toks
                                              , tagsMatchRule tags tok
                                              , null (ctxt tok conds)]:[]
                              ++ [nt (getBit tok) | tok <- toks
                                                    , tagsMatchRule tags tok
                                                    , (not.null) (ctxt tok conds)]:[]

-- b) general case, there can be nested ANDs, ORs or just plain rules
    (Remove tags (POS conds)) -> applyRules rule (toLists conds) toks
    (Select tags (POS conds)) -> applyRules rule (toLists conds) toks

  where ctxt tok conds = concatMap (getContext tok toks) (toLists conds)


applyRules :: Rule -> [[Condition]] -> [Token] -> [[Bit]]
applyRules rule []         allToks = []
applyRules rule (conds:cs) allToks = --trace (show rule ++ "\n") $ {-++ (show (map (\tok -> getContext tok allToks conds) allToks))) $ -}
  applyRules rule cs allToks ++
  case rule of 
    (Remove tags c) -> mkVars (chosen tags) nt 
    (Select tags c) -> mkVars (chosen tags) id ++ mkVars (other tags) nt


  where
        --getContext must return something for mkVars to work;
        -- if the TagSet part in Condition is null,
        -- getContext returns just the word itself as the context.
        allCondsHold :: [[Token]] -> Bool
        allCondsHold ts | null conds = True 
                        | otherwise  = all (not.null) ts


     -- cause => consequence    translates into     Not cause || consequence.
     -- cause is e.g. "-1 is noun" and consequence is "remove verb"
     -- needed because the word at -1 could have many tags, and they could conflict.
        mkVars :: [(Token,[Token])] -> (Bit -> Bit) -> [[Bit]]
        mkVars tctx neg' = [ neg' conseq:(map nt causes) | (l, ls) <- tctx
                                                         , let conseq = getBit l
                                                         , let causes = map getBit ls ]


        -- chosen: analyses that have the wanted readings and context
        chosen :: TagSet -> [(Token,[Token])]
        chosen tags = [(tok, concat context) | tok <- allToks
                                             , tagsMatchRule tags tok
                                             , let context = getContext tok allToks conds
                                             , allCondsHold context]
 
        -- other: analyses that don't have the wanted readings,
        -- but some word in the same location does have the wanted reading(s)
        other :: TagSet -> [(Token,[Token])]
        other tags = [(tok, concat context) | tok <- allWithReading tags
                                            , not (tagsMatchRule tags tok)
                                            , let context = getContext tok allToks conds]
                                           -- , allCondsHold context]
                                --no need to check allCondsHold; it comes from allWithReadings

 
        -- all words that have the desired reading in one of their analyses. e.g.
        --      allToks = [(1,[N,Pl]), (2,[V,Sg]), (2,[N,Pl])]
        --      tags    = [V]
        -- ====> `chosen tags' will return (2,[V,Sg)
        -- ====> (2,[V,Sg]) and (2,[N,Pl]) are returned
        allWithReading tags = intersectBy sameInd allToks wantedToks
          where wantedToks = map fst $ chosen tags


getContext :: Token           -- ^ a single analysis
               -> [Token]     -- ^ list of all analyses
               -> [Condition] -- ^ list of conditions grouped by AND
               -> [[Token]]   -- ^ context for the first arg. If all conditions match for a token, there will be as many non-empty Token lists as Conditions.
getContext tok allToks []     = []
getContext tok allToks ((C position (bool,ctags)):cs) = getContext tok allToks cs ++
  case ctags of
    []     -> [[tok]] -- empty tags in condition = remove/select always
    [[]]   -> [[tok]] -- since we need a context, give the word itself
    (t:ts) -> case position of
                (Exactly n) -> [filter (neg' . tagsMatchRule ctags) (exactly n tok)]
                (AtLeast n) -> [filter (neg' . tagsMatchRule ctags) (atleast n tok)]
                (Barrier n bs)  -> [filter (neg' . tagsMatchRule ctags) (barrier n bs tok)]

  where neg' = if bool then id else not

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

disambiguate :: Bool -> [Rule] -> Sentence -> IO Sentence
disambiguate verbose rules sentence = do
  s <- newSolver
  let chunkedSent = chunk sentence
  bitsForTags <- sequence [ newBit s | _ <- chunkedSent ]
  bitsForRules <- sequence [ newBit s | _ <- rules ]
  let toks = zip chunkedSent bitsForTags
      unambig = anchor toks :: [[Bit]]
      appliedrules = [ nt x:c | (r,x) <- zip rules bitsForRules
                               , c    <- applyRule toks r ]
  when verbose $ do
    putStrLn "\ntokens:"
    mapM_ print toks
    -- putStrLn "\nformulas:"
    -- mapM_ print unambig
    -- mapM_ print appliedrules
  mapM_ (addClauseBit s) unambig
  mapM_ (addClauseBit s) appliedrules
  --b <- maximize s [] bitsForRules
  b <- maximizeFromTop s  bitsForRules -- 7095 out of 7667 are different
  --b <- discardFromBottom s [] bitsForRules -- 7128 out of 7667 are different

  if b then
       do rs <- sequence [ modelValueBit s x | x <- bitsForRules ]
          when verbose $ do
            putStrLn "These rules were not applied due to conflicts:"
            mapM_ putStrLn [ show r | (b, r) <- zip rs rules, b /= Just True ]

          bs <- sequence [ modelValueBit s x | x <- bitsForTags ]
          let truetoks = [ t | (b, t) <- zip bs toks , b == Just True ]
          when verbose $ do
            putStrLn "\nThe following tag sequence was chosen:"
            mapM_ prTok truetoks
          return (dechunk truetoks)
    else
       do putStrLn "No solution"
          conf <- conflict s
          print conf
          return []

  where prTok :: Token -> IO ()
        prTok = putStrLn . showTags . getTags


test :: IO ()
test = mapM_ (disambiguate True rls) CG_data.exs

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
             , rmParticle ]

