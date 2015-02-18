import System.Environment (getArgs)
import System.Exit (exitFailure)

import AbsCG
import LexCG
import ParCG
import PrintCG
import ErrM

import Control.Monad.State.Lazy
import Data.List
import Debug.Trace
import qualified CG

type Env = [(String,[CG.Tag])]



--foo :: String -> IO () 
foo s = case pGrammar (myLexer s) of
            Bad err  -> do putStrLn "SYNTAX ERROR"
                           putStrLn err
                           exitFailure 
            Ok  tree -> do let rules = evalState (changeFormat tree) []
                           mapM_ print rules

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> readFile file >>= foo
            _      -> do putStrLn "Usage: CG_parse <SourceFile>"
                         exitFailure


--TODO: set defs go to some kind of state
changeFormat :: Grammar -> State Env [Either String CG.Rule]
changeFormat (Defs defs) = do mapM updateEnv defs
                              env <- get
                              return $ map (getRules env) defs
  where updateEnv :: Def -> State Env ()
        updateEnv (SetDef  s) = handleSetDecl s
        updateEnv (RuleDef r) = return ()

        getRules :: Env -> Def -> Either String CG.Rule
        getRules _ (SetDef  s) = Left $ printTree s
        getRules e (RuleDef r) = Right $ evalState (handleRule r) e

handleSetDecl :: SetDecl -> State Env ()
handleSetDecl (Set (SetName (UIdent name)) tags) = do 
  env <- get
  tl <- mapM handleTag tags
  let tagList = concat tl
      env' = (name, tagList):env
  put env'
                                      

handleTag :: Tag -> State Env [CG.Tag]
handleTag tag = case tag of
  Lemma str    -> return [CG.Lem str] 
  Tag (Id str) -> return [CG.Tag str]
  AND tags     -> do foo <- mapM handleTag tags
                     return (concat foo)
  EOS          -> return [CG.Tag "(>>>)"]
  Named (SetName (UIdent name)) -> do
    env <- get
    case lookup name env of
      Nothing -> error $ "Tagset " ++ show name ++ " not defined!"
      Just ts -> (return ts :: State Env [CG.Tag])


handleTagSet :: TagSet -> State Env [CG.Tag]
handleTagSet ts = case ts of
  TagSet tagset  -> handleTagSet tagset
  OR tag tagset  -> liftM2 (++) (handleTag tag) (handleTagSet tagset)
  Diff ts1 ts2   -> liftM2 (\\) (handleTagSet ts1) (handleTagSet ts2)
  Cart ts1 ts2   -> liftM2 (intersect) (handleTagSet ts1) (handleTagSet ts2) ---TODO
  All            -> return []
  NilT tag       -> handleTag tag

handleRule :: Rule -> State Env CG.Rule
handleRule rl = case rl of
  SelectIf tags conds -> liftM2 CG.Select (handleTagSet tags) (handleCond conds)
  RemoveIf tags conds -> liftM2 CG.Remove (handleTagSet tags) (handleCond conds)
  SelectAlways tags   -> liftM2 CG.Select (handleTagSet tags) (return $ CG.POS CG.always)
  RemoveAlways tags   -> liftM2 CG.Remove (handleTagSet tags) (return $ CG.POS CG.always)
  MatchLemma lem rule -> do eval <- handleRule rule
                            case eval of
                               CG.Select ts c -> return $ CG.Select ts (insert c lem)
                               CG.Remove ts c -> return $ CG.Remove ts (insert c lem)
  where insert :: CG.Test -> String -> CG.Test
        insert (CG.POS cs) str = CG.POS $ CG.AND cs (CG.mkC "0" [CG.Lem str])
        insert (CG.NEG cs) str = CG.NEG $ CG.AND cs (CG.mkC "0" [CG.Lem str])

handleCond :: [Cond] -> State Env CG.Test
handleCond x = return $ CG.POS CG.always