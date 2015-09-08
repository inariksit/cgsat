import CG_base
import Control.Monad (liftM2, when)
import Data.List
import Data.Maybe
import SAT
import SAT.Bool

--------------------------------------------------------------------------------
-- Add name for variables, for nicer looking output

data Lit' = LP {nm :: String, lt :: Lit} deriving (Ord,Eq)
type RuleTarget = (Lit',Lit')

instance Show Lit' where
  show (LP name lit) = name

newLit' :: Solver -> String -> IO Lit'
newLit' s name = LP name `fmap` newLit s

solve' :: Solver -> [Lit'] -> IO Bool
solve' s = solve s . map lt 

addClause' :: Solver -> [Lit'] -> IO ()
addClause' s  = addClause s . map lt 

andl' :: Solver -> String -> [Lit'] -> IO Lit'
andl' s name lps = LP name `fmap` andl s (map lt lps)

neg' :: Lit' -> Lit'
neg' (LP name lit) = LP name (neg lit)

modelValue' s = modelValue s . lt

equiv' :: Solver -> String -> Lit' -> Lit' -> IO Lit'
equiv' s name lp1 lp2 = LP name `fmap` equiv s (lt lp1) (lt lp2)

implies' :: Solver -> String -> Lit' -> Lit' -> IO Lit'
implies' s name lp1 lp2 = LP name `fmap` implies s (lt lp1) (lt lp2)

--------------------------------------------------------------------------------

main = do
  s <- newSolver
  in_prep <- newLit' s "in<prep>"
  in_adv <- newLit' s "in<adv>"
  target <- newLit' s "target"
  dummy  <- newLit' s "dummy"
  if_1C_prep <- andl' s "IF (-1C prep)" [in_prep, neg' in_adv]
  rm_trg <- implies' s "REMOVE target" if_1C_prep (neg' target) 
  rm_adv <- implies' s "REMOVE adv" (LP "always" true) (neg' in_adv) 
  rm_prep <- implies' s "REMOVE prep" (LP "always" true) (neg' in_prep) 

  -- rm_trg <- equiv' s "REMOVE target" if_1C_prep (neg' target) 
  -- rm_adv <- equiv' s "REMOVE adv" (LP "always" true) (neg' in_adv) 
  -- rm_prep <- equiv' s "REMOVE prep" (LP "always" true) (neg' in_prep) 

  addClause' s [in_prep,in_adv] -- >=1 analysis must be true
  addClause' s [target,dummy]

  let lits = [in_prep,in_adv,target,dummy,if_1C_prep,rm_trg,rm_adv,rm_prep]

  let clsAndTrgs = [ (rm_trg,target), (rm_adv,in_adv), (rm_prep,in_prep),  (rm_trg,target)] :: [RuleTarget]

  confs <- catMaybes `fmap` doStuff True s lits [] clsAndTrgs
  let removed = concatMap (removeOne clsAndTrgs) confs
  let orderchange = concatMap (changeOrder clsAndTrgs) confs

  vals <- mapM (checkConflict s) (orderchange++removed)

  let nonconf = [ comb | (True, comb) <- zip vals (orderchange++removed) ]
  putStrLn "not conf:"
  mapM_ print nonconf

  let conf = [ comb | (False, comb) <- zip vals (orderchange++removed) ]
  putStrLn "conflicting combinations:"
  mapM_ print conf
            
  where 

   changeOrder xs y = let xs' = delete y xs 
                          is  = [0..length xs'] in
                      is `for` \i -> take i xs' ++ [y] ++ drop i xs'

   removeOne xs y = let is  = [1..length xs] in
                    is `for` \i -> if xs !! (i-1) == y
                                      then xs
                                      else take (i-1) xs ++ drop i xs


   for = flip fmap

   doStuff :: Bool -> Solver -> [Lit'] -> [Lit'] -> [RuleTarget] -> IO [Maybe RuleTarget]
   doStuff _ _ _    _   []          = return []
   doStuff v s lits ass ((r,t):rts) = do
     (newAss, maybeRT) <- testSolve v s lits (r,t) ass :: IO ([Lit'], Maybe RuleTarget)
     newList <- doStuff v s lits (ass++newAss) rts
     return (maybeRT:newList)

   checkConflict :: Solver        -- ^ solver
                  -> [RuleTarget] -- ^ rule sequence to check with
                  -> IO Bool      -- ^ True if no conflict
   checkConflict s rts = do 
     results <- catMaybes `fmap` doStuff False s [] [] rts
     return $ null results --this means it does NOT conflict
   
--------------------------------------------------------------------------------


testSolve :: Bool -> Solver -> [Lit'] -> RuleTarget -> [Lit'] -> IO ([Lit'], Maybe RuleTarget)
testSolve v s lits (rule,trg) ass = do
     b <- solve' s (rule:ass)
     if b then 
         do newAss <- afterSolving v s trg lits
            when v $ print (ass,newAss)
            when v $ putStrLn "----"
            return (newAss, Nothing)
       else 
         do when v $ do
                  putStrLn $ "conflict with " ++ show rule
                  putStrLn "----"
            return ([], Just (rule,trg))
     


afterSolving :: Bool -- ^ verbose or not
             -> Solver -- ^ solver to use
             -> Lit'   -- ^ target lits
             -> [Lit'] -- ^ all lits, just for printing purposes
             -> IO [Lit'] -- ^ assumptions to carry to next round
afterSolving v s trgLit allLits = do
  targetVal <- modelValue' s trgLit
  if v 
    then do
      vs <- sequence [ modelValue s x | (LP name x) <- allLits ] 
      sequence_ [ putStrLn (show x ++ shB v) | (x, v) <- zip allLits vs ]
      print (trgLit, targetVal)
    else return ()
           --if target is removed, add that as a clause -- cannot retain it anymore
  if not targetVal
   then return [neg' trgLit] --addClause' s [neg' trgLit]
   else return [] -- ()



shB :: Bool -> String
shB True  = "\t1"
shB False = "\t0"