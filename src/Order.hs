module Order ( 
    order
  , howmanyReadings
  , checkByTarget
  , feedingOrder
  , needsPrevious
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


needsPrevious :: Rule -> RWSE Bool
needsPrevious rl = do
  s' <- liftIO newSolver 
  let (w,trgInd) = width rl
  (Config len sen) <- get
  b <- local (withNewSolver s') $ do 
          truesen <- trueSentence w
          put (Config w truesen)
          (condsHold,_) <- trigger rl trgInd
          liftIO $ solve s' [condsHold]
  liftIO $ deleteSolver s'
  put (Config len sen)
  return b

feedingOrder :: Rule -> [Rule] -> RWSE [Rule]
feedingOrder rl rls = catMaybes `fmap` sequence
  --TODO: less copypaste, do something smart with needsPrevious / a whole new function that calls it in local (withNewSolver s') ? Feels stupid to do the withNewSolver and trueSentence all over again
  [ do s' <- liftIO newSolver 
       let (w,trgInd) = width rl
       (Config len sen) <- get
       fdrl <- local (withNewSolver s') $ do
                  truesen <- trueSentence w
                  put (Config w truesen)
                  apply prevRl
                  conf <- ruleTriggers False rl trgInd
                  return $ case conf of
                    NoConf -> Just prevRl
                    _ -> Nothing

       liftIO $ deleteSolver s'
       return fdrl

    | prevRl <- rls ]

bleedingOrder :: Rule -> [Rule] -> RWSE [Rule]
bleedingOrder rl = undefined


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



