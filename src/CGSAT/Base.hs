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
  , Map, IntMap, IntSet


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
  , Config(..), emptyConfig, mkConfig
  , Sentence, mkSentence
  , Cohort(..), emptyCohort

    -- ** Log
  , Log

    -- * SplitReading
  , SplitReading(..), MorphReading(..)
  , Lem(..), WF(..), Tag(..)
  , fromLem, fromWF, fromReading
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
import Data.IntMap ( IntMap )
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

data CGException = TagsetNotFound String | OutOfScope Int String | NoReadingsLeft 
                 | UnknownError String deriving ( Show,Eq )

instance Exception CGException

--------------------------------------------------------------------------------
-- Log, Config

type Log = [String]


data Config = Config { senlength :: Int
                     , sentence :: Sentence 
                     } deriving (Show)


mkConfig :: Int -> RWSE Config
mkConfig w = Config w `fmap` mkSentence w

emptyConfig = Config 0 IM.empty

--------------------------------------------------------------------------------
-- Sentence, Cohort

type Sentence = IntMap Cohort

data Cohort = Coh { wordforms :: Map WF Lit -- One per cohort
                  , lemmas :: Map Lem Lit   -- Several per cohort
                  , readings :: IntMap Lit  -- Several per cohort -- TODO connect to lemmas
                  } deriving (Show,Eq)

emptyCohort :: Cohort
emptyCohort = Coh M.empty M.empty IM.empty

-- | Using the readings and the solver in environment, create a sentence.
mkSentence :: Int -> RWSE Sentence
mkSentence w
         | w <= 0    = return IM.empty
         | otherwise = do
  s <- asks solver
  rds  <- asks rdMap
  lemmas <- asks lems
  wforms <- asks wfs

  liftIO $ IM.fromList `fmap` sequence
     [ (,) n `fmap`
       do rdiMap <- sequence (IM.mapWithKey (mkLit s n) rds)
          lemMap <- do lemLits <- mapM (mkLitLex s n) lemmas'
                       return $ M.fromList (zip lemmas' lemLits)
          wfMap <- do wfLits <- mapM (mkLitLex s n) wforms'
                      return $ M.fromList (zip wforms' wfLits)
          return (Coh wfMap lemMap rdiMap)
       | n <- [1..w]
       , let lemmas' = getOrList lemmas 
       , let wforms' = getOrList wforms ] 


 where
  mkLitLex :: (Show a) => Solver -> Int -> a -> IO Lit
  mkLitLex s n t = newLit s (showReading (And [t]) n n)

  mkLit :: Solver -> Int -> Int -> MorphReading -> IO Lit
  mkLit s n m (Rd rd) = newLit s (showReading rd n m)

  showReading :: (Show a) => AndList a -> Int -> Int -> String
  showReading (And ts) wdi rdi = "w" ++ show wdi ++ concatMap (\t -> '_':show t) ts


--------------------------------------------------------------------------------
-- Newtypes. These have the same names as the type Tag in CGHS, 
-- but in CGHS, WF, Lem and Tag are constructors, here they are types.

newtype WF = WF { getWF :: String } deriving (Eq,Ord)
instance Show WF where
  show (WF str) = show (CGHS.WF str)

fromWF :: CGHS.Tag -> Maybe WF
fromWF (CGHS.WF x) = Just (WF x)
fromWF _           = Nothing

newtype Lem = Lem { getLem :: String } deriving (Eq,Ord)
instance Show Lem where
  show (Lem str) = show (CGHS.Lem str)

fromLem :: CGHS.Tag -> Maybe Lem
fromLem (CGHS.Lem x) = Just (Lem x)
fromLem _            = Nothing

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
-- Maybe make some newtypes to enforce that wform is WF _ and lemma is Lem _?
data SplitReading = SR { wform :: WF
                       , lemma :: Lem
                       , reading :: MorphReading } deriving (Eq)

instance Show SplitReading where
  show (SR w l r) = show w ++ " " ++ show l ++ " " ++ show r


{- With the split of lex. and morph. tags, we represent all the combinations
   as a Cartesian product: [WF] x [Lem] x [[Tag]]. 
  
   
-}


--------------------------------------------------------------------------------
-- Env
                
{-  tagMap 
           ‾‾`v

         vblex |-> [321,         322,         323      ...]
                     |            |            |
                    vblex sg p1  vblex sg p2  vblex sg ...  
                ___,^
           rdMap 
-}
data Env = Env { tagMap :: Map Tag IntSet 
               , rdMap :: IntMap MorphReading 
               , lems :: OrList Lem
               , wfs :: OrList WF
               , solver :: Solver }

withNewSolver :: Solver -> Env -> Env
withNewSolver s env = env { solver = s }


mkEnv :: Solver -> [CGHS.Reading] -> [CGHS.Tag] -> [CGHS.Tag] -> Env
mkEnv s rds ls ws = Env (mkTagMap rds) (mkRdMap rds) (mkLems ls) (mkWFs ws) s 
 where 
  mkLems = Or . mapMaybe fromLem
  mkWFs = Or . mapMaybe fromWF

{- rdMap --  1 |-> vblex sg p3
             2 |-> noun sg mf
          9023 |-> adj sg mf comp -}
mkRdMap :: [CGHS.Reading] -> IntMap MorphReading
mkRdMap = IM.fromDistinctAscList . zip [1..] . map (Rd . fmap fromTag)

{- tagMap    --  vblex |-> IS(1,30,31,32,..,490)
                 sg    |-> IS(1,2,3,120,1800)
                 mf    |-> IS(2,20,210,9023) -}
mkTagMap :: [CGHS.Reading] -> Map Tag IntSet 
mkTagMap rds = M.fromList $
  ts `for` \t -> let getInds = IS.fromList . map (1+) . findIndices (elem t)
                 in (fromTag t, getInds rdLists) 
 where 
  for = flip fmap 
  rdLists = map getAndList rds
  ts = nub $ concat rdLists


----------------------------------------------------------------------------
-- Read rules from a file, make an Env, return the Env and the rules.

envRules :: (String,[String]) -> Solver -> IO (Env,[Rule])
envRules (lang,r) s = do 

  let verbose = "v" `elem` r || "d" `elem` r
  let subr = if "withsub" `elem` r then ".withsub" else ".nosub"
  let rdsfromgrammar = True --"undersp" `elem` r || "rdsfromgrammar" `elem` r
  let parseReading = if "withsub" `elem` r 
                       then parseReadingApeSubr
                       else parseReadingApe
 
  let dirname = "data/" ++ lang ++ "/" 
  let grfile  = dirname ++ lang ++ ".rlx"
  let lexfile = dirname ++ lang ++ ".lexforms"
  let rdsfile = dirname ++ lang ++ ".readings" --  ++ subr

  --let acfile  = dirname ++ lang ++ ".ambiguity-classes"
  --let frmfile = dirname ++ lang ++ ".formula"
  
  ----------------------------------------------------------------------------

  (tsets,ruless) <- parse `fmap` readFile grfile
  let rules = filter (selOrRm . oper) (concat ruless)

  readingsInLex <- (map parseReading . words) `fmap` readFile rdsfile --Apertium format
  lexformsInLex <- (map readTag . filter (not.null) . words) `fmap` readFile lexfile                     

  let readingsInGr = if rdsfromgrammar --OBS. will mess up ambiguity class constraints
                      then concatMap tagSet2Readings tsets
                      else []

  let (nonLexReadings,lexformsInGr) = unzip $ map removeLexReading (readingsInGr++readingsInLex)

  let (lemmas,wforms) = partition isLem (nub $ concat lexformsInGr++lexformsInLex)


  let env = mkEnv s nonLexReadings lemmas wforms
  when verbose $ do 
    print (length readingsInLex, take 50 readingsInLex)

    print (length readingsInGr, take 50 readingsInGr)
    putStrLn "---------"

    putStrLn $ show (length rules) ++ " rules"
    mapM_ print (take 15 rules)


  return (env,rules)

selOrRm :: Oper -> Bool
selOrRm SELECT = True
selOrRm REMOVE = True
selOrRm _ = False

isLem :: CGHS.Tag -> Bool
isLem (CGHS.Lem _) = True
isLem _            = False

