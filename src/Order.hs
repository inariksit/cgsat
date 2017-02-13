module Order ( order, howmanyReadings
    ) where

import CGHS ( Rule )
import CGSAT
import CGSAT.Base

import Data.List ( permutations, sort )

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



