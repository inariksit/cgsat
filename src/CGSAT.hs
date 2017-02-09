module CGSAT (

    apply, trigger, defaultRules

  , solveAndPrint
  , mostReadingsLeft

  , RWSE, rwse, evalRWSE
  , CGException(..)
  , Config, mkConfig, emptyConfig
  , Env, envRules
  , width

  , litsFromCohort

  , dummyGenerate

  ) where

import CGHS.Rule hiding ( Not, Negate )
import qualified CGHS.Rule as R
import CGHS 

import SAT.Named

import CGSAT.Base
import CGSAT.Context
import CGSAT.Tagset

import qualified Data.Map as M
import qualified Data.IntMap as IM

import Data.Foldable ( fold )
import Data.List ( elemIndex, findIndices, intercalate, nub )
import Data.Maybe ( catMaybes, fromMaybe, isNothing )



--------------------------------------------------------------------------------

type TargetCohort = Cohort
-- Split the three maps of Cohort into those that only include target.
-- Just to make some type signatures clearer.


dummyGenerate :: [Rule] -> RWSE ()
dummyGenerate rules = do
  s <- asks solver
  let largestWidth = maximum $ map (fst . width) rules
  sent <- mkSentence largestWidth
  liftIO $ defaultRules s sent
  mapM_ apply rules
  liftIO $ solveAndPrint False s [] sent
  

solveAndPrint :: Bool -> Solver -> [Lit] -> Sentence -> IO ()
solveAndPrint vrb s ass sen = do
   b <- solve s ass
   if b 
   then do
      when vrb $ print ass
      trueVals <- sequence 
         [ do trueWFs <- modelValue s `filterM` M.elems wfs
              trueLems <- modelValue s `filterM` M.elems lems
              trueRds <- modelValue s `filterM` M.elems rds
              return (trueWFs,trueLems,trueRds)
            | (Coh wfs lems rds) <- IM.elems sen ]
      mapM_ print trueVals --TODO nicer printout
      putStrLn "----"
    else do
         putStrLn $ "solveAndPrint: Conflict with assumptions " ++ show ass


--------------------------------------------------------------------------------
-- 

-- | Apply the rule to all applicable cohorts in the sentence
apply :: Rule -> RWSE ()
apply rule = do 
  s <- asks solver

  ----------- The seitan of the function ------------
  let applyToCohort sen i = do

      --Trigger throwErrors NoReadingsLeft if the rule tries to remove all;
      -- as per VISL CG-3 behaviour, remove nothing.
       (allCondsHold, trgsAndOthers) 
         <- trigger rule i `catchError` \e -> case e of 
              NoReadingsLeft -> return (true,[])
              OutOfScope _ _ -> return (true,[])
              TagsetNotFound s -> do liftIO $ putStrLn ("Warning: tagset " ++ s ++ " not found")
                                     return  (true,[])
              _              -> throwError e

       if null trgsAndOthers -- if one of the expected errors was caught,

        then return sen -- just return the sentence unchanged

        else 
          do newTrgLits <- liftIO $ unzip3 `fmap` sequence
              [ do cannotApply <- orl' s [ neg allCondsHold -- Same for all versions of target
                                         , neg otherReadingsLeft ] -- Different for each version of target
                   newWFs <- sequence 
                              [ do newWFLit <- andl s newName [ oldWFLit, cannotApply ]
                                   return (wf,newWFLit)
                                  | (wf,oldWFLit) <- M.assocs (coh_w trgCoh) 
                                  , let newName = show oldWFLit ++ "'" ]

                   newLems <- sequence 
                              [ do newLemLit <- liftIO $ andl s newName [ oldLemLit, cannotApply ]
                                   return (lem,newLemLit)
                                  | (lem,oldLemLit) <- M.assocs (coh_l trgCoh) 
                                  , let newName = show oldLemLit ++ "'" ]
                   newRds <- sequence 
                              [ do newRdLit <- andl s newName [ oldRdLit, cannotApply ]
                                   return (rd,newRdLit)
                                  | (rd,oldRdLit)  <- M.assocs (coh_r trgCoh) 
                                  , let newName = show oldRdLit ++ "'" ]

                   return (newWFs, newLems, newRds) -- :: IO ([],[],[])
                    | (trgCoh, otherReadingsLeft) <- trgsAndOthers ] -- :: [ ([],[],[]) ]


             let newcoh = updateCohort (sen ! i) newTrgLits
             let newsen = updateSentence sen i newcoh
             return newsen

  ----------------------------------------------------

  (Config w sen) <- get
  newSen <- foldM applyToCohort sen [1..w]
  put (Config w newSen)

 where
  --updateCohort :: Cohort -> ( [[(WF,Lit)]], [[(Lem,Lit)]], [[(MorphReading,Lit)]] ) -> Cohort
  updateCohort (Coh w l r) (newWFs,newLems,newRds) = 
    Coh (foldl updateLit w (concat newWFs))
        (foldl updateLit l (concat newLems))
        (foldl updateLit r (concat newRds))

  updateLit :: (Ord k) => Map k Lit -> (k,Lit) -> Map k Lit
  updateLit coh (a,newrd) = M.adjust (const newrd) a coh

  updateSentence :: Sentence -> Int -> Cohort -> Sentence
  updateSentence sen i newcoh = IM.adjust (const newcoh) i sen


trigger :: Rule 
        -> Int -- ^ Rule applied to an index in the sentence
        -> RWSE ( Lit -- ^ Conditions of that rule
                , [(TargetCohort,Lit)] )-- ^ List of possible targets and others                
trigger rule origin = do
  (Config len sen) <- get
  env@(Env ws ls rs s) <- ask
  conds <- condLits rule origin
  trgCoh <- case IM.lookup origin sen of
              Nothing -> do tell [ "trigger: target position " ++ show origin ++ 
                                   " out of scope, sentence length " ++ show len ]
                            throwError (OutOfScope origin "trigger")
              Just ch -> return ch
  targetSplitReadings 
         <- do let normTagset = normaliseTagsetAbs (target rule) env
               case normTagset of
                 Left AllTags -> do tell [ "trigger: rule " ++ show rule ++ 
                                        " tries to remove or select all readings" ]
                                    throwError NoReadingsLeft
                 Right srs -> return (getOrList srs)
  

  condshold <- liftIO $ andl' s (getAndList conds) -- All conditions hold: this is same for the whole cohort
  trg_others <- mapM (targetAndOthers s trgCoh)  -- Target and others is relative to each SplitReading:
                     targetSplitReadings         -- returning a list of (trg,other) pairs
                   

  return (condshold, trg_others)

 where
  targetAndOthers :: Solver -> Cohort -> SplitReading 
                  -> RWSE (TargetCohort,Lit)
  targetAndOthers s coh srd = do
    let (targetCoh,otherCoh) = partitionTarget (oper rule) coh srd


    otherWFs <- safeElems (coh_w otherCoh)
    otherLem <- safeElems (coh_l otherCoh)
    otherRds <- safeElems (coh_r otherCoh)
    isOther <- liftIO $ 
                andl' s =<< sequence [ orl s (wdn++":nt_wordform") otherWFs
                                     , orl s (wdn++":nt_lemma") otherLem
                                     , orl s (wdn++":nt_morph.rd") otherRds ]

    return (targetCoh,isOther)

  wdn = "w" ++ show origin    

  -- If some if the maps in the non-target cohort is null,
  -- this means that everything is in the target, and the rule
  -- would try remove all readings. For this, we throw an error.
  safeElems :: Map k Lit -> RWSE [Lit]
  safeElems m = if M.null m then throwError NoReadingsLeft else return (M.elems m)

--------------------------------------------------------------------------------

defaultRules :: Solver -> Sentence -> IO ()
defaultRules s sentence = 
   sequence_ [ do addClause s (M.elems w)          
                  addClause s (M.elems l)
                  addClause s (M.elems r) -- Cohort must have >=1 reading
                  atMostOne s (M.elems w) -- , and exactly 1 word form
                  --TODO: add ambiguity class constraints
               | (Coh w l r) <- IM.elems sentence ] 

--------------------------------------------------------------------------------

partitionTarget :: Oper -> Cohort -> SplitReading -> (Cohort,Cohort)
partitionTarget op coh sr = case op of
  SELECT -> (outcoh,incoh)
  REMOVE -> (incoh,outcoh)
  _   -> (incoh,outcoh) --TODO other operations
 where
  (incoh,outcoh) = partitionCohort coh sr 



width :: Rule -> (Int,Int)
width rule = (length [minw..maxw], maybe 9999 (1+) (elemIndex 0 [minw..maxw]))
 where                                   
  ctxScopes = fmap scopes (context rule) :: AndList (OrList Int) -- And [Or [1], Or [1,2,3], Or [-2,-1]]
  flatScopes = fold (getAndList ctxScopes) :: OrList Int -- Or [1,1,2,3,-2,-1]
  (minw,maxw) = (0 `min` minimum flatScopes, 0 `max` maximum flatScopes)


mostReadingsLeft :: [Lit] -> RWSE (Int,Int)
mostReadingsLeft ass = do
  s <- asks solver
  sen <- gets sentence
  liftIO $ defaultRules s sen
  let allLits = concatMap litsFromCohort (IM.elems sen)
  k <- liftIO $ count s allLits  -- :: IO Unary
  b <- liftIO $ solveMaximize s ass k
  trueLits <- liftIO $ modelValue s `filterM` allLits
  return (length trueLits, length allLits)

litsFromCohort :: Cohort -> [Lit]
litsFromCohort (Coh x y z) = xlits++ylits++zlits
 where
  xlits = M.elems x
  ylits = M.elems y
  zlits = M.elems z