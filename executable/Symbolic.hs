import CG_base
import CG_SAT 
import CG_parse
import Control.Monad ( when )
import Data.List
import Debug.Trace
import SAT
import System.Environment
import System.IO ( hFlush, stdout )
import System.IO.Unsafe


testrules = concat $ snd $ parseRules False "SELECT:r1 (aa) OR (acr) IF (-1C (mf)) ;\nREMOVE:r2 (aa) IF (-1C (acr)) ;"
tinyrules = concat $ snd $ parseRules False 
          ( "SET DetNoAdj = (det) - (adj) ;" ++
            "SET AdjNoDet = (adj) - (det) ;" ++
           -- "REMOVE:r1 (v) IF (-1C (det)) (NOT 0 (det)) ;" ++
           -- "REMOVE:r2 (adj) - (attr);" ++
           "REMOVE:r3 (n) IF (-1 DetNoAdj OR AdjNoDet) (-1 (v) OR (n)) ;" ++
           "REMOVE:r3 (n) IF ( (-1 DetNoAdj OR AdjNoDet) OR (-2 (v)) ) ;" )

toTags' :: TagSet -> [[Tag]]
toTags' = concatMap (nub . (\(a,b) -> if all null b then a else b)) . toTags

main = do
  args <- getArgs
  case args of
   [] -> do putStrLn "test"
            ts <- (filter (not.null) . map parse . words) `fmap` readFile "data/spa_tags.txt"
  
            print ts
            let spl = splits testrules
            print spl
            (tsets, _) <- readRules' "data/spa_smallset.rlx"
            let tc = nub $ ts ++ concatMap toTags' tsets
            print tc
            results <- mapM (testRule True False ts tc) spl
            corpus <- concat `fmap` readData "data/spa_story.txt"
            --mapM_ ( (flip checkCorpus) corpus . snd) results
            putStrLn "\n---------\n"


   ("tiny":_)
      -> do let ts = map ((:[]) . Tag) ["adj","det","v","n"] 
            let tc = drop 1 ts ++ [[Tag "adj", Tag "pred"], [Tag "adj", Tag "attr"], [Tag "adj", Tag "det"], [Tag "def", Tag "det"]]
            let spl = splits $ reverse tinyrules
            let (verbose,debug) = (True, True)
            results <- mapM (testRule verbose debug ts tc) spl
            let badrules = [ rule | (False,rule) <- results ]
            mapM_ (findConflict ts tc) badrules

   ("nld":r)
      -> do let verbose = "v" `elem` r || "d" `elem` r
            let debug = "d" `elem` r
            ts <- (filter (not.null) . map parse . words) `fmap` readFile "data/nld/nld_tags.txt"
  
            (tsets, rls) <- readRules' "data/nld/nld.rlx"
            let rules = concat rls
            when debug $ mapM_ print rules
            let tcInGr = concatMap toTags' tsets
            print tcInGr
            tcInLex <- (map parse . words) `fmap` readFile "data/nld/nld_tagcombs.txt"
            let tc = nub $ tcInGr  -- ++ tcInLex
            let spl = splits rules
            results <- mapM (testRule verbose debug ts tc) spl
            --corpus <- concat `fmap` readData "data/nld_story.txt"
            --checkCorpus results corpus
            
            let badrules = [ rule | (False,rule) <- results ]

            putStrLn "bad rules:"
            --mapM_ print badrules
            
            --mapM_ (findConflict ts tc) badrules

            putStrLn "\n---------\n"
   (gr:r)
      -> do let verbose = "v" `elem` r || "d" `elem` r
            let debug = "d" `elem` r
            (tsets, rls) <- readRules' gr
            let rules = concat rls
            mapM_ print rules

            let tc = nub $ concatMap toTags' tsets
            let spl = splits rules
            results <- mapM (testRule verbose debug tc tc) spl
            let badrules = [ rule | (False,rule) <- results ]
            mapM_ (findConflict tc tc) badrules
            
  where 
   splits :: (Eq a) => [a] -> [(a,[a])]
   splits xs = xs `for` \x -> let Just ind = elemIndex x xs
                              in  (x, take ind xs)

   for = flip fmap

--------------------------------------------------------------------------------

--testRule :: Bool -> Bool -> [[Tag]] -> [[Tag]] -> (Rule, [Rule]) -> IO (Bool,[Token])
testRule :: Bool -> Bool -> [[Tag]] -> [[Tag]] -> (Rule, [Rule]) -> IO (Bool,(Rule,[Rule]))
testRule verbose debug alltags tagcombs (rule, rules) = do
  when verbose $ do
    putStrLn "************* testRule ***************"
    putStrLn $ "Testing with " ++ show rule ++ " as the last rule"
    putStrLn "the rest of the rules: " >> mapM_ print rules

  s <- newSolver
  let (ruleWidth,symbWords) = width rule
  allLits <- sequence 
              [ sequence [ newLit s | _ <- tagcombs ] | _ <- [1..ruleWidth] ] :: IO [[Lit]]

  let ss = concat
             [ [ (((m,False), addWF m tags),lit) | (lit, tags) <- zip lits tagcombs ]
                   | (lits, m) <- zip allLits [1..length allLits]
             ]  :: [Token]

  let sWordsInOrder = groupBy sameIndSW $ sort $ concat $ concat symbWords
  let sWordMap = [(sWord, lits) | (sWords, lits) <- zip sWordsInOrder allLits 
                                , sWord <- sWords ]
  --mapM_ print sWordMap

  when debug $ do 
    putStrLn "symbolic sentence:"
    mapM_ print ss

  cls <- concat `fmap` 
              sequence [ f swords | swords <- symbWords :: [[[SymbWord]]]
                                  , let f = slOrRm s sWordMap ]
                                                
  when verbose $ do
    putStr $ "rule " ++ show rule
    putStrLn $ ": "  ++ show (length cls) ++ " clauses"
  
  sequence_ [ do when debug $ print cl
                 addClause s cl | cl <- cls ] --

  b <- solve s []
  if b then do
    as <- sequence [ modelValue s x | x <- concat allLits ]
    let truetoks = [ t | (True, t) <- zip as ss ]
    when verbose $ putStrLn $ showSentence (dechunk truetoks)
   else do
    putStrLn "bad initial rule"
    conf <- conflict s
    putStr "conflict: "
    print conf
  putStrLn "\n---------\n"

  applied <- nub `fmap` sequence [ analyseGrammar s ss rl
                                   | rl <- rules
                                   , (fst $ width rl) <= ruleWidth ]

  sequence_ [ do addClause s cl
                 when debug $ 
                   putStrLn $ show rl ++ ": " ++ show cl                   
                 | (rl, cls) <- zip rules applied 
                 , cl <- cls ]

  b <- solve s []
  if b then do
    as <- sequence [ modelValue s x | x <- concat allLits ]
    let truetoks = [ t | (True, t) <- zip as ss ]
    when verbose $ putStrLn $ showSentence (dechunk truetoks)
--    return (True,truetoks)
    return (True,(rule,rules))
       else do
         putStrLn "Could not find sentence that matches conditions" 
         conf <- conflict s
         putStr "conflict: "
         print conf
         return (False,(rule,rules))


 where 
  addWF m = (WF ("w" ++ show m) :)

  sh True = '1' 
  sh False = '0'

  -- one condition corresponds to [[SymbWord]]s:
  -- for template,  [ [c1], [c2] ]
  -- for AND or barrier, [[c1,c2]]
  -- for single condition,  [[c1]]
  slOrRm :: Solver -> [(SymbWord,[Lit])] -> [[SymbWord]] -> IO [Clause]
  slOrRm s sWordMap swordsFromOneCond = -- trace ("slOrRm: " ++ show swordsFromOneCond) $
   do
    (newlits,cls) <- unzip `fmap` mapM (slOrRm' s sWordMap) swordsFromOneCond
    --when debug $ putStrLn ("slOrRm: " ++ show  newlits ++ " " ++ show cls)
    let nlCls = if length swordsFromOneCond > 1 
                  then [concat $ concat newlits] else concat newlits
    return $ nlCls ++ concat cls

  slOrRm' s sWordMap ind0s = do 
    let n = length tagcombs - 1
    let (trgs,conds) = partition (isTarget.info) ind0s
    newlits <- sequence [ newLit s | _ <- trgs ] --literal for each OR option in target
    when debug $ putStrLn ("slOrRm.newlits: " ++ show newlits)
    let trgCls = concatMap (filter notNegUnit) $ 
             [ concatMap (uncurry (disj wn n nl)) tg_difs
                            | (nl, sw@(SW info tg_difs)) <- zip newlits trgs
                            , let Just wn = lookup sw sWordMap ]  :: [Clause]

    
    (cNls, cCls) <- if null conds then return ([],[]) 
                     else slCond' s sWordMap conds
    return $ (if null newlits then cNls else newlits:cNls, trgCls ++ cCls)

    where
     disj :: [Lit] -> Int -> Lit -> Trg -> Dif -> [Clause]
     disj wn n nl trg dif = 
       let indTrg = concatMap (lookup' tagcombs) trg
           indDif = if null (concat dif) then [] else lookup' tagcombs (concat dif)
           indNeut = [0..n] \\ (indTrg ++ indDif)

       in [ neg nl:[ wn !! ind | ind <- indTrg ] ] ++
          [ neg nl:[ wn !! ind | ind <- indNeut ] ] ++
          [ [neg nl, neg$wn!!ind] | ind <- indDif ]

  slCond' :: Solver -> [(SymbWord,[Lit])] -> [SymbWord] -> IO ([Clause],[Clause])
  slCond' s sWordMap swords = trace ("slCond': " ++ show  (map targets swords)) $
   do
    let n = length tagcombs - 1 


    -- newlits:: [[Lit]]
    --  [Inside this list separate conditions grouped by AND
    --     [Inside these lists: tagsets grouped by OR]
    --  nope there is no consistency with the order in toConds, thanks for asking ^_^
    --  ]
    newlits <- sequence 
                 [ sequence [ newLit s | _ <- trg_difs ] 
                            | trg_difs <- map targets swords ] :: IO [[Lit]]
    when debug $
      putStrLn $ ("slCond'.newlits: " ++ show newlits)
    let disjs = concatMap (filter notNegUnit) $ 
                [ uncurry f trg_dif 
                              | (nlits, sw@(SW info trg_difs) ) <- zip newlits swords
                              , (nl, trg_dif) <- zip nlits trg_difs
                              , let Just wn = lookup sw sWordMap
                              , let f = case (isPositive info, isCautious info) of
                                          (False,True)  -> negativeC wn n nl
                                          (False,False) -> negative wn n nl
                                          (True,True) -> disjunctionC wn n nl
                                          (True,False) -> disjunction wn n nl ]
    return (newlits, disjs)

    where
     
     -- (-1 foo) : at least one foo must be true
     disjunction :: [Lit] -> Int -> Lit -> Trg -> Dif -> [Clause]
     disjunction wn n nl trg dif = 
       let indTrg = concatMap (lookup' tagcombs) trg
           indDif = if null (concat dif) then [] else concatMap (lookup' tagcombs) dif

       in [ neg nl:[ wn !! ind | ind <- indTrg ] ] ++
          [ [neg nl, neg(wn!!ind)] | ind <- indDif ]

     -- (-1C foo) : at least one foo must be true && all non-foos false
     disjunctionC wn n nl trg dif = 
       let indTrg = concatMap (lookup' tagcombs) trg
           indDif = if null (concat dif) then [] else concatMap (lookup' tagcombs) dif
           indNeut = [0..n] \\ (indTrg ++ indDif)
       in [ [ neg nl, neg$wn!!ind ] | ind <- indNeut ++ indDif ] ++
          [   neg nl:[  wn !! ind | ind <- indTrg ] ]

     -- (NOT -1 foo) : all foos must be false, and >=1 non-foo must be true
     negative wn n nl trg dif =  trace ("negative: " ++ show indNegTrg ++ " " ++ show indNegDif) $
       -- let indNegTrg = concatMap (lookup' tagcombs) trg
       --     indNegDif = if null (concat dif) then [] else concatMap (lookup' tagcombs) dif
       --     trgsWithoutDif = indNegTrg \\ indNegDif
       --     indNeut = [0..n] \\ trgsWithoutDif
        [ [ neg nl, neg$wn!!ind ] | ind <- trgsWithoutDif ] ++ 
          [   neg nl:[  wn !! ind | ind <- indNeut ] ]
       where 
           indNegTrg = concatMap (lookup' tagcombs) trg
           indNegDif = if null (concat dif) then [] else concatMap (lookup' tagcombs) dif
           trgsWithoutDif = indNegTrg \\ indNegDif
           indNeut = [0..n] \\ trgsWithoutDif

     -- (NOT -1C foo) : all foos are false, or foo and >=1 non-foo are true
     -- TODO check this again when more sane
     negativeC wn n nl trg dif =
       let indNegTrg = concatMap (lookup' tagcombs) trg
           indNegDif = if null (concat dif) then [] else concatMap (lookup' tagcombs) dif
           trgsWoDif = indNegTrg \\ indNegDif
           indFailsC = [0..n] \\ trgsWoDif
           indFailsDif = indNegDif
       in [ neg nl:[ neg$wn!!ind | ind <- trgsWoDif ] ++ [wn !! ind | ind <- indFailsC] ]
        ++ [neg nl:[  wn !! ind | ind <- [0..n] ] ]

{- old: in [ neg nl:[neg $ wn!!ind | ind <- indN] ++ [wn !! ind | ind <- indY] ]
          ++ [ neg nl:[ wn!!ind | ind <- indN++indY ] ]
        -- to force neg tag to be there along with another tag, choose following:
        --  ++ map (neg nl:) (sequence [[ wn!!ind | ind <- indN], [ wn!!ind | ind <- indY]])
-}
      
  notNegUnit [x] = pos x
  notNegUnit _   = True



--------------------------------------------------------------------------------

findConflict :: [[Tag]]                        -- ^ All tags
                -> [[Tag]]                     -- ^ All tag combinations
                -> (Rule,                      -- ^ Rule to be tested
                    [Rule])                    -- ^ Rules before the tested rule
                -> IO (Rule,([Rule],[Rule]))   -- ^ Rule, non-conf rules, conf rules
findConflict alltags tagcombs (rule, rules) = do
  let rulecombs = [ (rule, delRules) | delRules <- delOne rules ]
  results <- mapM (testRule False False alltags tagcombs) rulecombs
  let badcombs = [ rules' | (False,(rule', rules')) <- results ]
  let goodcombs = [ rules' | (True,(rule', rules')) <- results ]
  
  let allbad  = nub $ concat badcombs
  let allgood = nub $ concat goodcombs
  let onlybad = allbad \\ allgood

  putStrLn ( "rule: " ++ show rule)
  putStrLn "good:"
  mapM_ print allgood
  putStrLn "bad:"
  mapM_ print onlybad

  putStrLn "bad combinations:"
  mapM_ print badcombs
  putStrLn "good combinations:"
  mapM_ print goodcombs

  return (rule, (allgood, onlybad))
  -- print "bad combs:"
  -- mapM_ print badcombs
  -- putStrLn "--------"
  -- print "ok combs:"
  -- mapM_ print goodcombs

  where
   delOne xs = xs : map (\x -> delete x xs) xs
--------------------------------------------------------------------------------

lookup' :: [[Tag]] -> [Tag] -> [Int] --list of indices where the wanted taglist is found
-- For empty tagsInCond, returns all indices in tagcombs.
-- This is wanted behaviour; when filling conditions with empty spaces, 
-- those spaces can have any tag.
lookup' tagcombs tagsInCond = --trace ("lookup': " ++ show tagsInCond) $
  findIndices (\tc -> all (\t -> t `elem` tc) tagsInCond) tagcombs
      

width :: Rule    -- ^ rule whose width to calculate 
--      -> [[Tag]] -- ^ all possible tag combinations
      -> (Int, [[[SymbWord]]]) -- ^ conditions and target
width rule = case rule of
  (Select _ target Always) -> (1, [[[defaultTrg $ toTags target]]])
  (Remove _ target Always) -> (1, [[[defaultTrg $ toTags target]]])
  (Select _ target conds) -> doStuff target conds
  (Remove _ target conds) -> doStuff target conds
 where 
  -- [SymbWord] : sWords generated by one condition.
  -- for instance Barrier generates at least 2 sWords.
  -- a template should then generate [[SymbWord]]
  -- [ [[ SW (0 isTarget foo bar) [(trg, dif)] ]]
  -- , [[ SW (1 notTargt har gle) [(trg, dif)]     <- e.g. barrier rule
  --    , SW (2 notTrgt bar gle) [(trg, dif)] ]]
  -- , [ [ SW -1 something  [(trg,dif)] ]            <- template rule
  --     [ SW -2 otherthing [(trg,dif)] ] 
  --   ]
  -- ]
  doStuff :: TagSet -> Condition -> (Int, [[[SymbWord]]])
  doStuff t cs = trace ("doStuff: " ++ show (toTags t) ++ " IF " ++ show (toConds cs) ++ " width:" ++ show i) $ 
    (i, foo)
    where 
      (i, foo) = fill $ [[defaultTrg (toTags t)]] : [map toSymbWord (toConds cs)]
--------------------------------------------------------------------------------

type Trg = [[Tag]]
type Dif = [[Tag]]

data SymbWord = SW { info :: Info
                   , targets :: [(Trg,Dif)] } deriving (Eq,Ord,Show)


--isOptional : 
--say we have a template condition IF ( (-1 foo) OR (-2 bar) ),
--and there are no other conditions for -2.
--we signal this by allowing nonexistent -2.
data Info = I { index      :: Int 
              , isCautious :: Bool
              , isTarget   :: Bool
              , isPositive :: Bool 
              , isOptional :: Bool } deriving (Eq,Ord,Show)


defaultTrg :: [(Trg,Dif)] -> SymbWord
defaultTrg = SW (I 0 False True True False)

defaultCond :: Int -> SymbWord
defaultCond i = SW ( I i False False True False )
                   [ ([[]], [[]]) ]

mkCond :: Int -> Bool -> Bool -> [(Trg,Dif)] -> SymbWord
mkCond ind caut posit targets = 
  SW ( I { index  = ind
         , isCautious = caut
         , isTarget   = False
         , isPositive = posit
         , isOptional = False })
     targets

mkOptional :: SymbWord -> SymbWord
mkOptional (SW (I ind isC isT isPos isOpt) trg_dif) =
           (SW (I ind isC isT isPos True) trg_dif)

sameIndSW :: SymbWord -> SymbWord -> Bool
sameIndSW sw1 sw2 = (index.info) sw1 == (index.info) sw2

--------------------------------------------------------------------------------

--Conditions grouped by AND
-- would not work to remove []s, need to retain distinction between
-- one condition that generates 2 SymbWords and one condition that generates 2 [SymbWord]s
-- AND-conditions and one condition is the same, it will be given one variable and stuff
toSymbWord :: [Condition] -> [SymbWord]
toSymbWord [] = []
toSymbWord (Always:_) = error "toSymbWord applied to Always: this should not happen"
toSymbWord (c:cs) = case c of
  C (Barrier  c ind btags) (positive,tags) -> [ mkCond ind c positive (toTags tags),
                                                barSW ind False btags ]
  C (CBarrier c ind btags) (positive,tags) -> [ mkCond ind c positive (toTags tags),
                                                barSW ind True btags ]
  C (Exactly  c ind      ) (positive,tags) -> [ mkCond ind c positive (toTags tags) ]
  C (AtLeast  c ind      ) (positive,tags) -> [ mkCond ind c positive (toTags tags) ]
 
 ++ toSymbWord cs
 where
  barSW ind c ts = SW ( I { index      = if ind>=0 then ind+1 else ind-1
                          , isCautious = c
                          , isTarget   = False
                          , isPositive = True --barrier tags always positive
                          , isOptional = False }) 
                      ( toTags ts )

  




fill :: [[[SymbWord]]] -> (Int, [[[SymbWord]]])
fill []        = error "fill: []"
fill swordlist = (width, sort $ swordlist ++ (map (:[])) swords)
 where
  (width, swords) = go flatConds minInd (1,[])
  flatConds = sort $ concat $ concat swordlist
  minInd = index $ info $ head flatConds
  go []     m (w,res)  = (w, res)
  go (x:xs) m (w,res) | ind-m == 1 = go xs ind (w+1,res)
                      | ind-m == 0 = go xs ind (w,res)
                      | otherwise  = go xs ind (w+1,filled:res)
    where
     ind = (index.info) x 
     filled = [ defaultCond k | k <- [m+1..ind-1] ]

-- fill (x:xs) = go (x:xs) (index $ info x) []
--  where
--   go []                          _m res = reverse res
--   go (x@(SW (I ind c t ng _) _):xs) m res | ind-m == 1 || ind-m == 0 = go xs ind (x:res)
--                                         | otherwise       = go xs ind (x:(filled++res))
--            where filled = [ defaultCond k | k <- [m+1..ind-1] ]



--------------------------------------------------------------------------------

-- chunk :: Sentence -> [(Info, [Tag])]
-- chunk sent = concat $ go sent 1
--    where go []    _m = []
--          go (x:xs) m = map ((,) (mkInfo m)) (addWF m x) : go xs (m+1)
--          addWF m = map (WF ("w" ++ show m) :)

---TODO
-- dechunk' :: [SymbWord] -> Sentence
-- dechunk' ts = map (map targets) $ groupBy sameIndSW ts

dechunk :: [Token] -> Sentence
dechunk ts = (map.map) getTags (groupBy sameInd ts)


fstEq (a,_) (b,_) = a==b

--------------------------------------------------------------------------------

parse :: String -> [Tag]
parse str = map toTag $ filter (not.null) $ split isValid str
 where 
  isValid c = c=='<' || c=='+'
  toTag ">>>" = BOS
  toTag "<<<" = EOS
  toTag []    = error "empty tag"
  toTag str = if last str=='>' then Tag (init str) else Lem str

split :: (a -> Bool) -> [a] -> [[a]]
split p [] = []
split p xs = takeWhile (not . p) xs : split p (drop 1 (dropWhile (not . p) xs))

printFancy :: String -> IO ()
printFancy s = 
  do putStr (s ++ back)
     hFlush stdout
     putStr (wipe ++ back)
 where
  n   = length s
  back = replicate n '\b'
  wipe = replicate n ' '


--------------------------------------------------------------------------------

--just a naive try to prune the corpus-free method by using a corpus.
--TODO rather restrict the number/combination of tags in the SAT formulas?

checkCorpus :: [Token] -> Sentence -> IO ()
checkCorpus symbsent corpus = do
  putStrLn "------------"
  putStrLn "checkCorpus:"
  let sent = dechunk symbsent
--  putStrLn $ showSentence sent
  let foo = filter (\s -> length s >= 2) (adjacent sent corpus)
  mapM_ pr $ map showSentence foo 
  where pr arg = putStrLn "---" >> putStrLn arg

elem' :: Analysis -> Analysis -> [Analysis]
elem' symbolic real = nub [ real | s <- symbolic, r<-real
                               , not $ null $ s `intersect` r ]

adjacent :: Sentence -> Sentence -> [Sentence]
adjacent (w1:w2:[]) corpus = 
 [ words1++words2 | (c1,c2) <- zip corpus (drop 1 corpus)
    , let words1 = elem' w1 c1
    , let words2 = elem' w2 c2
    , (not.null) words1
    , (not.null) words2
    , all (any mt1) [words1,words2] ] 

adjacent (w1:w2:ws) corpus = 
 [ words1 | (c1,c2) <- zip corpus (drop 1 corpus)
    , let words1 = elem' w1 c1
    , let words2 = elem' w2 c2
    , (not.null) words1
    , (not.null) words2
    , all (any mt1) [words1,words2] ]
 ++ adjacent (w2:ws) corpus

adjacent ws         corpus = []

mt1 :: [[Tag]] -> Bool
mt1 []     = False
mt1 (t:ts) = if length t > 1 then True else mt1 ts


--------------------------------------------------------------------------------
{--
checkIfApplies ::  Solver
                  -> [[Lit]]
                  -> Rule 
                  -> IO [Clause]
checkIfApplies s lits rule = do
  if length lits >= length sswidth 
    then do putStrLn "sufficiently long sentence"
            let inds = map (concatMap (map (lookup' [[]]) . snd)) sswidth
            print inds --e.g. [[[0,1]],[[2,3,4,5]],[[3,5],[2,4]]]
            --for lit in lits:
            --
            print sswidth
            return []
    else do putStrLn "conditions out of scope"
            return []
  where sswidth = width rule []



for rule in Pres:
  for w in symbolicSent:
     rl has no effect on w, because one of the following:

     1) conditions are out of scope (trivial, no clause)
     2) conditions in scope, but one doesn't hold
     3) tag has been removed in target
     4) all readings of  target have the desired tag (cannot remove)



general stuff:

we know length of symbolic sentence from the width of R
we know the target from R
conditions: if no C, then all tag combinations that have the tag in the condition
            if C, pick one of them (disjoint clauses?)
if empty space in conds, empty places must have >= 1 readings (but any will do)

--}