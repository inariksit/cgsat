module CG_parse where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import BNFC.AbsApertium
import BNFC.LexApertium
import BNFC.ParApertium
import BNFC.AbsCG
import BNFC.LexCG
import BNFC.ParCG
import BNFC.PrintCG
import BNFC.ErrM

import Control.Applicative
import Control.Monad.State.Lazy
import Data.Either
import Data.List
import Debug.Trace

import qualified CG

type Env = [(String,CG.TagSet)]


test = False

parseRules :: String -> IO [CG.Rule]
parseRules s = case pGrammar (BNFC.ParCG.myLexer s) of
            Bad err  -> do putStrLn "parseRules: syntax error"
                           putStrLn err
                           exitFailure 
            Ok  tree -> do let rules = evalState (parseCGRules tree) []
                           when test (mapM_ pr rules)
                           return $ rights rules
  where pr (Right rule) = putStrLn $ show rule
        pr (Left string) = putStrLn string

parseData :: String -> IO [CG.Sentence]
parseData s = case pText (BNFC.ParApertium.myLexer s) of
            Bad err  -> do putStrLn "parseData: syntax error"
                           putStrLn err
                           exitFailure 
            Ok text  -> do return $ (split . transText) text

main :: IO ()
main = do args <- getArgs
          case args of
             [file1,file2] -> do readFile file1 >>= parseRules
                                 readFile file2 >>= parseData
                                 putStrLn "foo"
             _             -> do putStrLn "Usage: CG_parse <rules> <data>"
                                 exitFailure


---

parseCGRules :: Grammar -> State Env [Either String CG.Rule]
parseCGRules (Defs defs) = do mapM updateEnv defs
                              env <- get
                              return $ map (parseRules env) defs
  where updateEnv :: Def -> State Env ()
        updateEnv (SetDef  s) = transSetDecl s
        updateEnv (RuleDef r) = return ()

        parseRules :: Env -> Def -> Either String CG.Rule
        parseRules _ (SetDef  s) = Left $ BNFC.PrintCG.printTree s
        parseRules e (RuleDef r) = Right $ evalState (transRule r) e



split :: [CG.Analysis] -> [CG.Sentence]
split as = go as []
  where go [] ys = ys
        go xs ys = let beforePunct = takeWhile (not . isPunct) xs 
                       fromPunct   = dropWhile (not . isPunct) xs
                       punct = if null fromPunct then [] else head fromPunct 
                       newxs = if null fromPunct then [] else tail fromPunct
                       newsent = startToken:beforePunct ++ [punct, endToken]
                   in go newxs (newsent:ys)

        startToken = [[CG.Lem ">>>", CG.Tag ">>>"]]
        endToken   = [[CG.Lem "<<<", CG.Tag "<<<"]]

        isPunct :: CG.Analysis -> Bool
        isPunct = tagsInAna [CG.Lem ".", CG.Lem "!", CG.Lem "?"]

        tagsInAna :: [CG.Tag] -> CG.Analysis -> Bool
        tagsInAna tags as = or $ map ((not.null) . intersect tags) as


---- CG parsing

transSetDecl :: SetDecl -> State Env ()
transSetDecl (Set (SetName (UIdent name)) tags) = trace (show tags) $ do 
  env <- get
  tl <- mapM transTag tags
  let tagList = concat tl
  put $ (name, tagList):env

                                      

transTag :: Tag -> State Env CG.TagSet
transTag tag = case tag of
  Lemma str    -> return [[CG.Lem str]]
  Tag (Id str) -> return [[CG.Tag str]]
  AND tags     -> do ts <- mapM transTag tags
                     let allInOne = [concat (concat ts)]
                     return allInOne
  Named (SetName (UIdent name)) -> do
    env <- get
    case lookup name env of
      Nothing -> error $ "Tagset " ++ show name ++ " not defined!"
      Just ts -> (return ts :: State Env CG.TagSet)


transTagSet :: TagSet -> State Env CG.TagSet
transTagSet ts = case ts of
  TagSet tagset  -> transTagSet tagset
  NilT tag       -> transTag tag
  All            -> return [[]]
  OR tag tagset  -> do tags1 <- transTag tag
                       tags2 <- transTagSet tagset
                       return $ tags1 ++ tags2

  ----TODO all set operations!

  Diff All ts    -> error "TODO this should have effect on a higher level"
  Diff ts All    -> error "something except everything? are you a philosopher?"
  Diff ts1 ts2   -> do tags1 <- transTagSet ts1
                       tags2 <- transTagSet ts2
                       return $ tags1 \\ tags2
  

  Cart ts1 ts2   -> do tags1 <- transTagSet ts1   
                       tags2 <- transTagSet ts2
                       let combs = [[x,y] | x<-concat tags1, y<-concat tags2]
                       return combs


transRule :: Rule -> State Env CG.Rule
transRule rl = case rl of
  SelectIf tags conds -> liftM2 CG.Select (transTagSet tags) (transConds conds)
  RemoveIf tags conds -> liftM2 CG.Remove (transTagSet tags) (transConds conds)
  SelectAlways tags   -> liftM2 CG.Select (transTagSet tags) (return $ CG.POS CG.always)
  RemoveAlways tags   -> liftM2 CG.Remove (transTagSet tags) (return $ CG.POS CG.always)
  MatchLemma lem rule -> do cgrule <- transRule rule
                            case cgrule of
                               CG.Select ts c -> return $ CG.Select (cart ts lem) c
                               CG.Remove ts c -> return $ CG.Remove (cart ts lem) c
  where cart :: CG.TagSet -> String -> CG.TagSet
        cart ts str = [[CG.Lem str,t] | t<-concat ts]


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
  where transTagSet' :: Bool -> TagSet -> State Env (Bool, CG.TagSet)
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

transBarrier :: Barrier -> State Env CG.TagSet
transBarrier (Barrier ts) = transTagSet ts


-- Apertium morpho output parsing

transText :: Text -> [CG.Analysis]
transText x = case x of
  Lines lines  -> map transLine lines


transLine :: Line -> CG.Analysis
transLine x = case x of
  Line (Iden wform) analyses -> map (transAnalysis wform) analyses
  LinePunct (Punct str)     -> [[CG.WF str, CG.Lem str, CG.Tag "punct"]]
  NoAnalysis (Iden wform) _ -> [[CG.WF wform]]


transAnalysis :: String -> Analysis -> [CG.Tag]
transAnalysis wf (Anal (Iden id) tags) = CG.WF wf:CG.Lem id:(map transTagA tags)

transTagA :: TagA -> CG.Tag
transTagA (TagA (Iden id)) = CG.Tag id


