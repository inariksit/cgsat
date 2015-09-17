import CG_base
import CG_SAT hiding ( isCautious, isPositive )
import CG_parse
import SAT.Named
import SAT ( Solver, newSolver, conflict )

import Control.Monad ( when )
import Data.List
import Debug.Trace

import System.Environment
import System.IO ( hFlush, stdout )


ex_onlyTrgLeft = concat $ snd $ parseRules False
     ( "REMOVE:r1 (adj OR det) IF (1 v) ;" ++
    -- ( "REMOVE:r1 (adj OR det) ;" ++
       "REMOVE:r2 v IF (-1 adj LINK 0 det ) ;" ) 

ex_firstRuleStricter = concat $ snd $ parseRules False
     ( "REMOVE:r1 v IF (-1C det) ;" ++
       "REMOVE:r2 v IF (-1  det) ;" ) 

ex_threerules = concat $ snd $ parseRules False 
     ( "REMOVE:r1 (v) IF (-1C (det))  ;" ++
       "REMOVE:r2 (det) IF (1 (v)) ;"    ++
       "REMOVE:r3 (v) IF (-1 (det) ) ;" ) 

ex_barrier = concat $ snd $ parseRules False 
     ( "REMOVE:r1 (det) IF (1* (adj)) ; " ++ --will go for only (det def) left
       "REMOVE:r2 (v)   IF (1* (def) BARRIER (adj)) ;" )

ex_testDisj = concat $ snd $ parseRules False
     ( "SET DetNoDef  = (det) - (def) ;" ++
       "SET AdjNoAttr = (adj) - (attr) ;" ++
       "REMOVE:r1 (n) IF (-1 DetNoDef OR AdjNoAttr) (-1 (det) OR (adj)) ;" ++
       "REMOVE:r2 (n) IF ( (-1 DetNoDef OR AdjNoAttr) OR (-2 (v)) ) (-2 (adj)) ;" )


ex_complex = concat $ snd $ parseRules False 
     ( "REMOVE:r1 (n) IF (-1C (det));"  ++
       "SELECT:s2 (det) IF (1 (v));" ++
--     "REMOVE:r2 (*) - (det) IF (1 (v)) ; " ++
       "REMOVE:r3  (v) IF (-1 (det)) (0 (n)) ;" )


toTags' :: TagSet -> [[Tag]]
toTags' = concatMap (nub . (\(a,b) -> if all null b then a else b)) . toTags

main = do
  args <- getArgs
  case args of
   [] -> do putStrLn "test"
            let ts = map ((:[]) . Tag) ["adj","det","v","n"] 
            let tc = ts
           -- let tc = drop 1 ts ++ [[Tag "adj", Tag "pred"], [Tag "adj", Tag "attr"], [Tag "pron", Tag "def"], [Tag "det", Tag "def"]]
            print ts
            let spl = splits ex_threerules
            print spl
            print tc
            results <- mapM (testRule True True ts tc) spl
            corpus <- concat `fmap` readData "data/spa/spa_story.txt"
            --mapM_ ( (flip checkCorpus) corpus . snd) results
            putStrLn "\n---------\n"


   ("tiny":_)
      -> do let ts = map ((:[]) . Tag) ["adj","det","v","n"] 
            --let tc = ts 
            let tc = drop 1 ts ++ [[Tag "adj", Tag "pred"], [Tag "adj", Tag "attr"], [Tag "pron", Tag "def"], [Tag "det", Tag "def"]]
            let splits' x = [last (splits x)]
            let otl_spl  = splits' ex_onlyTrgLeft
            let first_stricter_spl = splits' ex_firstRuleStricter
            let second_stricter_spl = splits' $ reverse ex_firstRuleStricter
            let three = splits' ex_threerules
            let barrier = splits' ex_barrier
            let disj = splits' ex_testDisj
            let complex = splits' ex_complex

            let doEverything spl = do 
                     putStrLn "Next test!\n***********"
                     let (verbose, debug) = (True, True)
                     results <- mapM (testRule verbose debug ts tc) spl
                     mapM_ (findConflict ts tc) [ rule | (False,rule) <- results ]

            mapM_ doEverything 
                  [ otl_spl
                  , first_stricter_spl
                  , second_stricter_spl
                  , three
                  , barrier
                  , disj
                  , complex
                  ]




   ("nld":r)
      -> do let verbose = "v" `elem` r || "d" `elem` r
            let debug = "d" `elem` r
            ts <- (filter (not.null) . map parse . words) `fmap` readFile "data/nld/nld_tags.txt"
  
            (tsets, rls) <- readRules' "data/nld/nld.rlx"
            let rules = concat rls
            when debug $ mapM_ print rules
            let tcInGr = nub $ concatMap toTags' tsets
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
   ("spa":r)
      -> do ts <- (filter (not.null) . map parse . words) `fmap` readFile "data/spa_tags.txt"
            (tsets, _) <- readRules' "data/spa/spa_smallset.rlx"
            let tc = nub $ ts ++ concatMap toTags' tsets
            print "spa"
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

shTC :: [Tag] -> Int -> String
shTC ts i = "w" ++ show i ++ (concatMap (\t -> '<':show t++">") ts)


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
              [ sequence [ newLit s (shTC t n) | t <- tagcombs ] | n <- [1..ruleWidth] ] :: IO [[Lit]]

  let ss = concat
             [ [ mkToken m (addWF m tags) lit | (lit, tags) <- zip lits tagcombs ]
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
                 addClause s cl | cl <- cls ]

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

  rls_applied_helps <- sequence [ do as_hs <- analyseGrammar s ss rl
                                     let rl_as_hs = map (\(a,b) -> (rl,a,b)) as_hs
                                     return rl_as_hs --to make sure only applied rules come in
                                   | rl <- rules
                                   , (fst $ width rl) <= ruleWidth ] 

  let rls_applied = [ (rl,cls) | foo <- rls_applied_helps 
                               , (rl, cls, _) <- foo ]
               
  let helps = nub $ concat
               [ cls | foo <- rls_applied_helps 
                     , (_, _, cls)  <- foo ]



--  ass <- doStuff True s helps [] rls_applied


  sequence_ [ do addClause s cl
                 when True $ --debug $ 
                   putStrLn $ show rl ++ ": " ++ show cl
                 | (rl, cls) <- rls_applied 
                 , cl <- cls ] 


  b <- solve s []
  if b then do
--    when debug $ safePrValues helps s
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
    let nlCls = if length swordsFromOneCond > 1 
                  then [concat $ concat newlits] else concat newlits
    return $ nlCls ++ concat cls

  slOrRm' s sWordMap ind0s = do 
    let n = length tagcombs - 1
    let (trgs,conds) = partition (isTarget.info) ind0s
    newlits <- sequence [ newLit s ("trg"++(show $ index $ info t)) | t <- trgs ] --literal for each OR option in target
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
           indDif = if null (concat dif) then [] else concatMap (lookup' tagcombs) dif
           indNeut = [0..n] \\ (indTrg ++ indDif)

       in [ neg nl:[ wn !! ind | ind <- indTrg ] ] ++
          [ neg nl:[ wn !! ind | ind <- indNeut ] ] ++
          [ [neg nl, neg$wn!!ind] | ind <- indDif ]

  slCond' :: Solver -> [(SymbWord,[Lit])] -> [SymbWord] -> IO ([Clause],[Clause])
  slCond' s sWordMap swords = 
   do
    let n = length tagcombs - 1 

    -- newlits:: [[Lit]]
    --  [Inside this list separate conditions grouped by AND
    --     [Inside these lists: tagsets grouped by OR]
    --  nope there is no consistency with the order in toConds, thanks for asking ^_^
    --  ]
    newlits <- sequence 
                 [ sequence [ newLit s "" | n <- [0..length trg_difs-1] ] 
                            | trg_difs <- map targets swords ] :: IO [[Lit]]
    when debug $
      putStrLn $ ("slCond'.newlits: " ++ show newlits)
    let disjs = concatMap (filter notNegUnit) $ 
                [ uncurry f trg_dif 
                    | (nlits, sw@(SW info trg_difs) ) <- zip newlits swords
                    , (nl, trg_dif) <- zip nlits trg_difs
                    , let f = case (isPositive info, isCautious info,lookup sw sWordMap) of
                               (True, True, Just wn) -> disjunctionC wn n nl
                               (True, False,Just wn) -> disjunction wn n nl 
                               (False,False,Just wn) -> negative wn n nl
                               (False,True, Just wn)  -> negativeC wn n nl
                               (_,   _,    Nothing) -> error "slCond.index out of bounds" ]

    return (newlits, disjs)

    where
     
     -- (-1 foo) : >=1 foo is true
     disjunction :: [Lit] -> Int -> Lit -> Trg -> Dif -> [Clause]
     disjunction wn n nl trg dif = 
       let indTrg = concatMap (lookup' tagcombs) trg
           indDif = if null (concat dif) then [] else concatMap (lookup' tagcombs) dif
       in [ neg nl:[ wn !! ind | ind <- indTrg ] ] ++
          [ [neg nl, neg (wn!!ind)] | ind <- indDif ]

     -- (-1C foo) : >=1 foo is true, and all non-foos are false
     disjunctionC wn n nl trg dif = 
       let indTrg = concatMap (lookup' tagcombs) trg
           indDif = if null (concat dif) then [] else concatMap (lookup' tagcombs) dif
           trgsWoDif = indTrg \\ indDif
           indFailsC = [0..n] \\ trgsWoDif --either it's in Dif or it's not specified
       in [   neg nl:[  wn !! ind | ind <- trgsWoDif ] ] ++
          [ [ neg nl, neg (wn!!ind) ] | ind <- indFailsC ]
          

     -- (NOT -1 foo) : all foos are false, and >=1 non-foo is true
     negative wn n nl trg dif = 
       let indNegTrg = concatMap (lookup' tagcombs) trg
           indNegDif = if null (concat dif) then [] else concatMap (lookup' tagcombs) dif
           trgsWithoutDif = indNegTrg \\ indNegDif
           indNeut = [0..n] \\ trgsWithoutDif
       in [ [ neg nl, neg (wn!!ind) ] | ind <- trgsWithoutDif ] ++ 
          [   neg nl:[  wn !! ind | ind <- indNeut ] ]


     -- (NOT -1C foo) : >=1 non-foo is true
     negativeC wn n nl trg dif =
       let indNegTrg = concatMap (lookup' tagcombs) trg
           indNegDif = if null (concat dif) then [] else concatMap (lookup' tagcombs) dif
           trgsWoDif = indNegTrg \\ indNegDif
           indFailsC = [0..n] \\ trgsWoDif --either it's in Dif or it's not specified
       in [ neg nl:[ wn !! ind | ind <- indFailsC ] ]
      
  notNegUnit [x] = pos x
  notNegUnit _   = True



--------------------------------------------------------------------------------

findConflict :: [[Tag]]                     -- ^ All tags
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
  --    , SW (2 notTargt bar gle) [(trg, dif)] ]]
  -- , [ [ SW -1 something  [(trg,dif)] ]            <- template rule
  --     [ SW -2 otherthing [(trg,dif)] ] 
  --   ]
  -- ]
  doStuff :: TagSet -> Condition -> (Int, [[[SymbWord]]])
  doStuff t cs = --trace ("doStuff: " ++ show (toTags t) ++ " IF " ++ show (toConds cs) ++ " width:" ++ show i) $ 
    (i, foo)
    where 
      (i, foo) = fill $ [[defaultTrg (toTags t)]] : [map toSymbWord (toConds cs)]
--------------------------------------------------------------------------------

data SymbWord = SW { info :: Info
                   , targets :: [(Trg,Dif)] } deriving (Eq,Ord,Show)


data Info = I { index      :: Int 
              , isCautious :: Bool
              , isTarget   :: Bool
              , isPositive :: Bool } deriving (Eq,Ord,Show)


defaultTrg :: [(Trg,Dif)] -> SymbWord
defaultTrg = SW (I 0 False True True )

defaultCond :: Int -> SymbWord
defaultCond i = SW ( I i False False True )
                   [ ([[]], [[]]) ]

mkCond :: Int -> Bool -> Bool -> [(Trg,Dif)] -> SymbWord
mkCond ind caut posit targets = 
  SW ( I { index  = ind
         , isCautious = caut
         , isTarget   = False
         , isPositive = posit })
     targets


sameIndSW :: SymbWord -> SymbWord -> Bool
sameIndSW sw1 sw2 = (index.info) sw1 == (index.info) sw2

--------------------------------------------------------------------------------

--all conditions are grouped by AND
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
                          , isPositive = True }) --barrier tags always positive
                      ( toTags ts )

  



--TODO error maybe here ???
fill :: [[[SymbWord]]] -> (Int, [[[SymbWord]]])
fill []        = error "fill: []"
fill swordlist = (width, swordlist ++ (map (:[])) swords)
 where
  (width, swords) = go flatConds minInd (1,[])
  flatConds = sort $ concat $ concat swordlist
  minInd = index $ info $ head flatConds
  go []     oldInd (wc,res)  = (wc, res)
  go (x:xs) oldInd (wc,res) | n == 1 = go xs ind (wc+n,res)
                            | n == 0 = go xs ind (wc, res)
                            | otherwise  = go xs ind (wc+n,filled:res)
    where
     ind = (index.info) x 
     n = ind-oldInd
     filled = [ defaultCond k | k <- [oldInd+1..ind-1] ]


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
--rather restrict the number/combination of tags in the SAT formulas?

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