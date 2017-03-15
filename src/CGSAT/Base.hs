{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

module CGSAT.Base (
    -- * Control.Monad
    filterM, foldM, liftM2, mapAndUnzipM, when

    -- * Control.Monad.RWS
  , RWST, MonadState, MonadReader, MonadWriter
  , runRWST, ask, asks, get, gets, put, tell, local

    -- * Control.Monad.Except
  , MonadError, ExceptT, runExceptT
  , throwError, catchError 
  , lift 

    -- * Control.Monad.IO.Class
  , MonadIO, liftIO 

    -- * Control.Exception 
  , Exception

    -- * Map, IntMap, IntSet
  , Map, IntMap, (!), IntSet


    -- * SAT+
  , Solver(..), newSolver, deleteSolver

    -- ** SAT.Named
  , Lit(..), true, false
  , andl, andl', orl, orl'
  , implies, equiv, neg, atMostOne
  , solve, addClause, modelValue
   

    -- * RWSE
  , rwse, evalRWSE, RWSE(..), CGException(..)

    -- ** Env
  , Env(..), mkEnv, withNewSolver
  , envRules

    -- ** Config
  , Config(..), emptyConfig, mkConfig, newConfig
  , Sentence, mkSentence
  , Cohort(..), emptyCohort
  , SplitCohort(..), partitionCohort

    -- ** Log
  , Log

    -- * SplitReading
  , SplitReading(..), MorphReading(..)
  , Lem(..), WF(..), Tag(..)
  , fromLem, fromWF, fromReading
  , isLexSR, mergeSRs
  )
where

-- Exporting standard functions along with my own, 
-- so that I don't have to import all of them all the time.


import Control.Monad 
import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.IO.Class
import Control.Exception

import Data.Map ( Map )
import qualified Data.Map as M
import Data.IntMap ( IntMap, (!) )
import qualified Data.IntMap as IM
import Data.IntSet ( IntSet )
import qualified Data.IntSet as IS 

import SAT ( Solver, newSolver, deleteSolver )
import SAT.Named

import CGHS.Rule ( Oper(..) )
import CGHS hiding ( Tag(..), Reading )
import qualified CGHS

import Data.List ( findIndices, nub, partition )
import Data.Maybe ( mapMaybe )

import Paths_cgsat

--------------------------------------------------------------------------------
-- RWSE, CGException

newtype RWSE a = RWSE { runRWSE :: ExceptT CGException (RWST Env Log Config IO) a }
  deriving ( Functor, Applicative, Monad, MonadIO, MonadWriter Log
           , MonadState Config, MonadReader Env, MonadError CGException )

rwse :: Env 
     -> Config 
     -> RWSE a
     -> IO ( Either CGException a, Config, Log )
rwse env conf (RWSE m) = runRWST (runExceptT m) env conf

evalRWSE :: Env -> RWSE a -> IO a
evalRWSE env m = do (Right x,_,_) <- rwse env emptyConfig m
                    return x

data CGException = TagsetNotFound String | OutOfScope Int String 
                 | NoReadingsLeft String
                 | UnknownError String deriving ( Show,Eq )

instance Exception CGException

--------------------------------------------------------------------------------
-- Log, Config

type Log = [String]


data Config = Config { senlength :: Int
                     , sentence :: Sentence 
                     } deriving (Show)

newConfig :: Int -> RWSE ()
newConfig w = mkConfig w >>= put

mkConfig :: Int -> RWSE Config
mkConfig w = Config w `fmap` mkSentence w

emptyConfig = Config 0 IM.empty

--------------------------------------------------------------------------------
-- Sentence, Cohort

type Sentence = IntMap Cohort

data Cohort = Coh { coh_w :: Map WF Lit -- One per cohort
                  , coh_l :: Map Lem Lit   -- Several per cohort
                  , coh_r :: Map MorphReading Lit  -- Several per cohort -- TODO connect to lemmas
                  } deriving (Show,Eq)

data SplitCohort = SCoh { scoh_w :: Maybe (Map WF Lit)
                  , scoh_l :: Maybe (Map Lem Lit)
                  , scoh_r :: Maybe (Map MorphReading Lit)
                  } deriving (Show,Eq)

emptyCohort :: Cohort
emptyCohort = Coh M.empty M.empty M.empty

partitionCohort :: Cohort -> SplitReading -> RWSE (SplitCohort,SplitCohort)
partitionCohort (Coh wMap lMap rMap) (SR ws ls rs) = do
  (inWFs,outWFs) <- inOut elem ws wMap "(searched as a word form)"
  (inLems,outLems) <- inOut elem ls lMap "(searched as a lemma)"
  (inRds,outRds) <- inOut isSubr rs rMap "(searched as a morphological reading)"
  return (SCoh inWFs inLems inRds, SCoh outWFs outLems outRds)
 where
  inOut f xs xmap errormsg = do
    if null xs then return (Nothing,Nothing)
     else
      do let (maybeInXs,maybeOutXs) = M.partitionWithKey (\k _ -> k `f` xs) xmap
         if M.null maybeInXs
           then do --liftIO $ print (getOrList xs)
                   --liftIO $ print xmap
                   throwError $ TagsetNotFound (show (getOrList xs) ++ " " ++ errormsg)
           else return (Just maybeInXs, Just maybeOutXs) 

  isSubr :: MorphReading -> OrList MorphReading -> Bool
  isSubr rd rds = any (subr rd) rds

  subr (Rd uspec) (Rd spec) = includes uspec spec

-- | Using the readings and the solver in environment, create a sentence.
mkSentence :: Int -> RWSE Sentence
mkSentence w
         | w <= 0    = return IM.empty
         | otherwise = do
  s <- asks solver
  morphrds  <- getOrList `fmap` asks env_rds
  lemmas <- getOrList `fmap` asks env_lems
  wforms <- getOrList `fmap` asks env_wfs

  liftIO $ IM.fromList `fmap` sequence
     [ (,) n `fmap`
       do --rdiMap <- sequence (IM.mapWithKey (mkLit s n) morphrds)
          rdMap <- do rdLits <- mapM (mkLit s n) morphrds
                      return $ M.fromList (zip morphrds rdLits)
          lemMap <- do lemLits <- mapM (mkLitLex s n) lemmas
                       return $ M.fromList (zip lemmas lemLits)
          wfMap <- do wfLits <- mapM (mkLitLex s n) wforms
                      return $ M.fromList (zip wforms wfLits)
          return (Coh wfMap lemMap rdMap)
       | n <- [1..w] ] 


 where
  mkLitLex :: (Show a) => Solver -> Int -> a -> IO Lit
  mkLitLex s n t = newLit s (showReading (And [t]) n)

  mkLit :: Solver -> Int -> MorphReading -> IO Lit
  mkLit s n (Rd rd) = newLit s (showReading rd n)

  showReading :: (Show a) => AndList a -> Int -> String
  showReading (And ts) wdi = "w" ++ show wdi ++ concatMap (\t -> '_':show t) ts


--------------------------------------------------------------------------------
-- Newtypes. These have the same names as the type Tag in CGHS, 
-- but in CGHS, WF, Lem and Tag are constructors, here they are types.

newtype WF = WF { getWF :: CGHS.Tag } deriving (Eq,Ord)

instance Show WF where
  show (WF wf) = show wf

fromWF :: CGHS.Tag -> Maybe WF
fromWF x@(CGHS.WF _) = Just (WF x)
fromWF _             = Nothing

newtype Lem = Lem { getLem :: CGHS.Tag } deriving (Eq,Ord)

instance Show Lem where
  show (Lem lem) = show lem

fromLem :: CGHS.Tag -> Maybe Lem
fromLem x@(CGHS.Lem _) = Just (Lem x)
fromLem _              = Nothing

newtype Tag = Tag { getTag :: CGHS.Tag } deriving (Eq,Ord)

instance Show Tag where
  show (Tag tag) = show tag

fromTag :: CGHS.Tag -> Tag
fromTag = Tag

newtype MorphReading = Rd { getReading :: AndList Tag } deriving (Eq,Show,Ord)

fromReading :: CGHS.Reading -> MorphReading
fromReading rd = Rd (fmap fromTag morphtags)
 where (morphtags,_) = removeLexReading rd

-- The type Reading that CGHS/Rule exports is actually an underspecified reading.
-- For our symbolic sentences, each reading has a word form, lemma and other tags.
data SplitReading = SR { sr_w :: OrList WF
                       , sr_l :: OrList Lem
                       , sr_r :: OrList MorphReading } deriving (Eq,Ord)

instance Show SplitReading where
  show (SR w l r) = "{ " ++ show (getOrList w) ++ "\n  " ++
                           show (getOrList l) ++ "\n  " ++
                           show (getOrList r) ++ " }"

isLexSR :: SplitReading -> Bool
isLexSR (SR w l r) = null (getOrList r)

mergeSRs :: SplitReading -> SplitReading -> SplitReading
mergeSRs (SR w l r) (SR w' l' r') = SR (mappend w w') (mappend l l') (mappend r r')

--------------------------------------------------------------------------------
-- Env
                
data Env = Env { env_wfs :: OrList WF
               , env_lems :: OrList Lem
               , env_rds :: OrList MorphReading
               , solver :: Solver }

withNewSolver :: Solver -> Env -> Env
withNewSolver s env = env { solver = s }

mkEnv :: Solver -> [CGHS.Reading] -> [CGHS.Tag] -> [CGHS.Tag] -> Env
mkEnv s rds ls ws = Env (mkWFs ws) (mkLems ls) (mkRds rds) s 
 where 
  mkRds = Or . map fromReading
  mkLems x = Or (unknownLem:mapMaybe fromLem x)
  mkWFs x = Or (unknownWF:WF (CGHS.WF "dummy"):mapMaybe fromWF x)


unknownLem :: Lem
unknownLem = Lem (CGHS.Lem "unknown")

unknownWF :: WF
unknownWF = WF (CGHS.WF "unknown")


----------------------------------------------------------------------------
-- Read rules from a file, make an Env, return the Env and the rules.

envRules :: (String,[String]) -> Solver -> IO (Env,[Rule])
envRules (lang,r) s = do 

  let verbose = "v" `elem` r || "d" `elem` r
  let subr = if "withsub" `elem` r then ".withsub" else ""
  let parseReadings = if "withsub" `elem` r 
                       then map parseReadingApeSubr . words
                       else map parseReadingApe . words
  let parseLexforms = map readTag . filter (not.null) . words

  grfile  <- getDataFileName (lang ++ ".rlx")
  lexfile <- getDataFileName (lang ++ ".lexforms")
  rdsfile <- getDataFileName (lang ++ ".readings" ++ subr)

  -- acfile  <- getDataFileName (lang ++ ".ambiguity-classes")
  -- frmfile <- getDataFileName (lang ++ ".formula")
  
  ----------------------------------------------------------------------------

  let compact = False
  (tsets,ruless) <- parse compact `fmap` readFile grfile
  readingsByUser <- parseReadings `fmap` readFile rdsfile --Apertium format
  lexformsByUser <- parseLexforms `fmap` readFile lexfile                     

  -- Use only lexical forms in grammar, otherwise ignore underspecified readings
  let readingsInGr = concatMap (tagSet2Readings.snd) tsets
  let (_,lexformsInGr) = unzip $ map removeLexReading (readingsInGr)
  let allLexforms = nub $ concat (lexformsByUser:lexformsInGr)
  let (lemmas,wforms) = partition isLem allLexforms

  -- Remove lexical readings from the readings given in the file.
  -- In principle the files should already be separated, this is just to make sure.
  let morphReadings = map (fst . removeLexReading) (bosRd:eosRd:readingsByUser)

  -- Printouts, for checking if the data looks correctly parsed etc.
  when verbose $ do 
    print (length morphReadings, take 15 morphReadings)
    print (length lemmas, take 15 lemmas)
    print (length wforms, take 15 wforms)
    putStrLn "---------"

  -- Make env, filter only select/remove rules, and return!
  let env = mkEnv s morphReadings lemmas wforms
  let rules = filter (selOrRm . oper) (concat ruless)
  return (env,rules)

selOrRm :: Oper -> Bool
selOrRm SELECT = True
selOrRm REMOVE = True
selOrRm _ = False

isLem :: CGHS.Tag -> Bool
isLem (CGHS.Lem _) = True
isLem _            = False

bosRd :: CGHS.Reading
bosRd = And [CGHS.BOS]

eosRd :: CGHS.Reading
eosRd = And [CGHS.EOS]