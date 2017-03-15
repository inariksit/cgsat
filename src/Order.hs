module Order ( 
    order
  , howmanyReadings
  , checkByTarget
  , feedingOrder
  ) where

import CGHS ( Rule, sortByContext, groupRules )
import CGSAT
import Analyse
import CGSAT.Base

import Data.List ( permutations, sort )
import Data.Maybe ( catMaybes )
import qualified Data.Map as M
import qualified Data.IntMap as IM
--------------------------------------------------------------------------------
-- Feeding order

feedingOrder :: Rule -> [Rule] -> RWSE [Rule]
feedingOrder rl rls = catMaybes `fmap` sequence

  [ do s' <- liftIO newSolver 
       fdrl <- local (withNewSolver s') $ do
                  let (w,trgInd) = width rl
                  newConfig w
                  tempSen <- gets sentence
                  (condsHold,_) <- trigger rl trgInd
                  liftIO $ print ("feedingOrder.condsHold ", condsHold)
                  applyToAllTrue prevRl
                  --apply prevRl
                  b <- liftIO $ solveAndPrint True s' [condsHold] tempSen
                  --b <- liftIO $ solve s' [condsHold]
                  return $ if b then Just prevRl else Nothing
       liftIO $ deleteSolver s'
       return fdrl

    | prevRl <- rls ]

bleedingOrder :: Rule -> [Rule] -> RWSE [Rule]
bleedingOrder rl = undefined



applyToAllTrue :: Rule -> RWSE ()
applyToAllTrue rule = do
  s <- asks solver 

  ------------
  let applyToCohort sen i = do
        (condsHold, trgsAndOthers) <- trigger rule i `catchError` \e -> return (true,[]) --do liftIO $ print e ; return (true,[])
        newTrgLits <- liftIO $ unzip3 `fmap` sequence
          [ do newWFs <- sequence 
                 [ do newWFLit <- implies s newName condsHold (neg oldWFLit)
                      return (wf,newWFLit)
                  | (wf,oldWFLit) <- maybe [] M.assocs mw
                  , let newName = show oldWFLit ++ "'" ]

               newLems <- sequence 
                [ do newLemLit <- implies s newName condsHold (neg oldLemLit)
                     return (lem,newLemLit)
                  | (lem,oldLemLit) <- maybe [] M.assocs ml
                  , let newName = show oldLemLit ++ "'" ]
               newRds <- sequence 
                [ do newRdLit <- implies s newName condsHold (neg oldRdLit)
                     return (rd,newRdLit)
                  | (rd,oldRdLit) <- maybe [] M.assocs mr
                  , let newName = show oldRdLit ++ "'" ]
                           --print ("apply.newRds ", newRds)                   
               return (newWFs, newLems, newRds) -- :: IO ([],[],[])
           | ( SCoh mw ml mr, _) <- trgsAndOthers ] :: RWSE ( [[(WF,Lit)]], [[(Lem,Lit)]], [[(MorphReading,Lit)]] )

        let newcoh = updateCohort (sen ! i) newTrgLits
                 --liftIO $ print ("apply.newCoh    ", newcoh)
        let newsen = updateSentence sen i newcoh
        return newsen
      ------------


  (Config w sen) <- get
  newSen <- foldM applyToCohort sen [1..w]
  put (Config w newSen)

 where
  updateCohort :: Cohort -> ( [[(WF,Lit)]], [[(Lem,Lit)]], [[(MorphReading,Lit)]] ) -> Cohort
  updateCohort (Coh w l r) (newWFs,newLems,newRds) = 
   Coh (forceTrue w newWFs)
       (forceTrue l newLems)
       (forceTrue r newRds)

  forceTrue :: (Ord k) => Map k Lit -> [[(k,Lit)]] -> Map k Lit
  forceTrue kmap newKs = 
    let updatedKs = foldl updateLit kmap (concat newKs)
        oldKs = M.difference kmap updatedKs
        oldKsTrue = foldl updateLit oldKs $ M.keys oldKs `zip` repeat true
    in M.union updatedKs oldKs --True 

  updateLit :: (Ord k) => Map k Lit -> (k,Lit) -> Map k Lit
  updateLit coh (a,newrd) = M.adjust (const newrd) a coh

  updateSentence :: Sentence -> Int -> Cohort -> Sentence
  updateSentence sen i newcoh = IM.adjust (const newcoh) i sen

--------------------------------------------------------------------------------
-- Grouping by targets and sorting by contexts; then run conflict check

checkByTarget :: [Rule] -> Int -> Int -> RWSE [(Conflict,Rule)]
checkByTarget rules from to = do
  let verbose = False
  let groupedRls = sortByContext `map` groupRules rules :: [[Rule]]
  let rulesFromTo = drop from $ take to groupedRls
  liftIO $ mapM_ (\x -> do print (head x)
                           let len = length x
                           putStrLn (show len ++ " rules...")
                           print (last x)
                           putStrLn "\n"
                 ) rulesFromTo
  confs <- mapM (testRules verbose) rulesFromTo
  let cs_rs = [ [ (c,rl) | (c,rl) <- zip cs rls, isConflict c ]
                | (cs,rls) <- zip confs rulesFromTo ]
  liftIO $ mapM_ print cs_rs
  return (concat cs_rs)


--------------------------------------------------------------------------------
-- Based on solveMaximize

order :: [Rule] -> RWSE [Rule]
order rules
 | length rules < 5 = orderPerm rules
 | otherwise = do
    ns <- mapM (howmanyReadings . (:[])) rules
    let (best,rest) = (     snd $ head $ sort $ zip ns rules
                      , map snd $ tail $ sort $ zip ns rules )
    (:) best `fmap` order rest

orderPerm :: [Rule] -> RWSE [Rule]
orderPerm rules = do
  let allRls = permutations rules
  rds <- mapM howmanyReadings allRls
  return (snd $ minimum $ zip rds allRls)


howmanyReadings :: [Rule] -> RWSE Int
howmanyReadings rules = do
  liftIO $ putStrLn "---------"
  liftIO $ putStrLn "howmanyReadings"
  liftIO $ mapM_ print rules
  liftIO $ putStrLn "---------"
  s' <- liftIO newSolver
  let largestWidth = maximum $ map (fst . width) rules
  i <- local (withNewSolver s') $ do
    conf <- mkConfig largestWidth
    put conf
    mapM_ apply rules
    (i,j) <- mostReadingsLeft [] --TODO: put something in there that forces rules actually apply

    liftIO $ putStrLn $ show i ++ " analyses out of " ++ show j
    return i
  liftIO $ deleteSolver s'
  return i



