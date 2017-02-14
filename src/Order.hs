module Order ( 
    order
  , howmanyReadings
  , checkByTarget
  ) where

import CGHS ( Rule, sortByContext, groupRules )
import CGSAT
import Analyse
import CGSAT.Base

import Data.List ( permutations, sort )


--------------------------------------------------------------------------------
-- Grouping by targets and sorting by contexts; then run conflict check

checkByTarget :: [Rule] -> RWSE [(Conflict,Rule)]
checkByTarget rules = do
  let verbose = False
  let groupedRls = sortByContext `map` groupRules rules :: [[Rule]]
  liftIO $ mapM_ (\x -> do print (head x)
                           putStrLn "..."
                           print (last x)
                           putStrLn "\n"
                 ) (take 5 groupedRls)
  confs <- mapM (testRules verbose) (take 5 groupedRls)
  let cs_rs = [ [ (c,rl) | (c,rl) <- zip cs rls, isConflict c ]
                | (cs,rls) <- zip confs groupedRls ]
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
    (:) best `fmap` (order rest)

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
  s' <- liftIO $ newSolver
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



