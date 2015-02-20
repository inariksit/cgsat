module CG_parse where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import AbsApertium
import LexApertium
import ParApertium
import PrintApertium
import AbsCG
import LexCG
import ParCG
import qualified PrintCG as PrCG
import ErrM

import Control.Applicative
import Control.Monad.State.Lazy
import Data.Either
import Data.List
import Debug.Trace

import qualified CG

type Env = [(String,[CG.Tag])]



getRules :: String -> IO [CG.Rule]
getRules s = case pGrammar (ParCG.myLexer s) of
            Bad err  -> do putStrLn "getRules: syntax error"
                           putStrLn err
                           exitFailure 
            Ok  tree -> do let rules = evalState (getCGRules tree) []
                           mapM_ pr rules
                           return $ rights rules
  where pr (Right rule) = putStrLn $ show rule
        pr (Left string) = putStrLn string

getData :: String -> IO [CG.Analysis]
getData s = case pText (ParApertium.myLexer s) of
            Bad err  -> do putStrLn "getData: syntax error"
                           putStrLn err
                           exitFailure 
            Ok text  -> do let anals = transText text
                           mapM_ print anals
                           return anals

main :: IO ()
main = do args <- getArgs
          case args of
             [file1,file2] -> do readFile file1 >>= getRules
                                 readFile file2 >>= getData
                                 putStrLn "foo"
             _             -> do putStrLn "Usage: CG_parse <rules> <data>"
                                 exitFailure


---

getCGRules :: Grammar -> State Env [Either String CG.Rule]
getCGRules (Defs defs) = do mapM updateEnv defs
                            env <- get
                            return $ map (getRules env) defs
  where updateEnv :: Def -> State Env ()
        updateEnv (SetDef  s) = transSetDecl s
        updateEnv (RuleDef r) = return ()

        getRules :: Env -> Def -> Either String CG.Rule
        getRules _ (SetDef  s) = Left $ PrCG.printTree s
        getRules e (RuleDef r) = Right $ evalState (transRule r) e


---- CG parsing

transSetDecl :: SetDecl -> State Env ()
transSetDecl (Set (SetName (UIdent name)) tags) = do 
  env <- get
  tl <- mapM transTag tags
  let tagList = concat tl
  put $ (name, tagList):env

                                      

transTag :: Tag -> State Env [CG.Tag]
transTag tag = case tag of
  Lemma str    -> return [CG.Lem str] 
  Tag (Id str) -> return [CG.Tag str]
  AND tags     -> do ts <- mapM transTag tags
                     return (concat ts)
  EOS          -> return [CG.Tag ">>>"]
  Named (SetName (UIdent name)) -> do
    env <- get
    case lookup name env of
      Nothing -> error $ "Tagset " ++ show name ++ " not defined!"
      Just ts -> (return ts :: State Env [CG.Tag])


transTagSet :: TagSet -> State Env [CG.Tag]
transTagSet ts = case ts of
  TagSet tagset  -> transTagSet tagset
  OR tag tagset  -> liftM2 (++) (transTag tag) (transTagSet tagset)
 ---TODO all set operations!
  Diff ts1 ts2   -> liftM2 (\\) (transTagSet ts1) (transTagSet ts2)
  Cart ts1 ts2   -> liftM2 (++) (transTagSet ts1) (transTagSet ts2)
  All            -> return []
  NilT tag       -> transTag tag

transRule :: Rule -> State Env CG.Rule
transRule rl = case rl of
  SelectIf tags conds -> liftM2 CG.Select (transTagSet tags) (transConds conds)
  RemoveIf tags conds -> liftM2 CG.Remove (transTagSet tags) (transConds conds)
  SelectAlways tags   -> liftM2 CG.Select (transTagSet tags) (return $ CG.POS CG.always)
  RemoveAlways tags   -> liftM2 CG.Remove (transTagSet tags) (return $ CG.POS CG.always)
  MatchLemma lem rule -> do eval <- transRule rule
                            case eval of
                               CG.Select ts c -> return $ CG.Select ts (insert c lem)
                               CG.Remove ts c -> return $ CG.Remove ts (insert c lem)
  where insert :: CG.Test -> String -> CG.Test
        insert (CG.POS cs) str = CG.POS $ CG.AND cs (CG.mkC "0" [CG.Lem str])
        insert (CG.NEG cs) str = CG.NEG $ CG.AND cs (CG.mkC "0" [CG.Lem str])

transConds :: [Cond] -> State Env CG.Test
transConds c = do conds <- mapM transCond c
                  return $ CG.POS $ foldr1 CG.AND conds

transCond :: Cond -> State Env CG.Condition
transCond c = case c of
  C cond         -> transCond cond
  Linked c1 c2   -> liftM2 CG.AND (transCond c1) (transCond c2) ----TODO
  CPos pos ts    -> liftM2 CG.C (transPosition pos) (transTagSet' True  ts)
  CNotPos pos ts -> liftM2 CG.C (transPosition pos) (transTagSet' False ts)
  CBarrier pos ts bar -> handleBar pos ts bar True
  CNotBar pos ts bar  -> handleBar pos ts bar False
  where transTagSet' :: Bool -> TagSet -> State Env (Bool, [CG.Tag])
        transTagSet' b ts = do tags <- transTagSet ts
                               return (b, tags)
        handleBar pos ts bar bool = do
          pos' <- transPosition pos
          let int = case pos' of 
                          CG.Exactly i -> error "this is parse error in real CG, sorry"
                          CG.AtLeast i -> i
          btags <- transBarrier bar
          liftM2 CG.C (return $ CG.Barrier int btags) (transTagSet' bool ts)


transPosition :: Position -> State Env CG.Position
transPosition pos = return $ case pos of
  Exactly (Signed str) -> CG.Exactly $ read str
  AtLeast (Signed str) -> CG.AtLeast $ read str

transBarrier :: Barrier -> State Env [CG.Tag]
transBarrier (Barrier ts) = transTagSet ts


-- Apertium morpho output parsing

transText :: Text -> [CG.Analysis]
transText x = case x of
  Lines lines  -> map transLine lines


transLine :: Line -> CG.Analysis
transLine x = case x of
  Line _wordform analyses -> map transAnalysis analyses
  LinePunct (Punct str)   -> [[CG.Lem str, CG.Tag "punct"]]
  NoAnalysis (Iden id) _  -> [[CG.Lem id]]


transAnalysis :: Analysis -> [CG.Tag]
transAnalysis (Anal (Iden id) tags) = CG.Lem id:(map transTagA tags)

transTagA :: TagA -> CG.Tag
transTagA (TagA (Iden id)) = CG.Tag id


