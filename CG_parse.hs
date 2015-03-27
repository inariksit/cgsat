module CG_parse ( parseRules
                , parseData
                , readRules
                , readData ) where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import Apertium.Abs
import Apertium.Lex
import Apertium.Par
import Apertium.ErrM as AErr
import CG.Abs
import CG.Lex
import CG.Par
import CG.Print
import CG.ErrM as CGErr

import Control.Applicative
import Control.Monad.State.Lazy
import Data.Either
import Data.List
import Debug.Trace

import qualified CG_base as CGB

type Env = [(String, CGB.TagSet)]


parseRules :: Bool -> String -> [CGB.Rule]
parseRules test s = case pGrammar (CG.Par.myLexer s) of
            CGErr.Bad err  -> error err
            CGErr.Ok  tree -> let rules = evalState (parseCGRules tree) []
                              in trace (if test then (unwords $ map pr rules) else "") $
                              rights rules
  where pr (Right rule)  = show rule
        pr (Left string) = string

parseData :: String -> [CGB.Sentence]
parseData s = case pText (Apertium.Par.myLexer s) of
            AErr.Bad err  -> error err
            AErr.Ok text  -> split $ transText text

--just because it's nice to use them  rules <- readRules foo
readRules :: String -> IO [CGB.Rule]
readRules fname = readFile fname >>= return . parseRules False

readData :: String -> IO [CGB.Sentence]
readData fname = readFile fname >>= return . parseData

main :: IO ()
main = do args <- getArgs
          case args of
             [file1,file2] -> do readFile file1 >>= return . parseRules True --verbose
                                 readData file2
                                 putStrLn "read rules and data"
             _             -> do putStrLn "Usage: CG_parse <rules> <data>"
                                 exitFailure


---

parseCGRules :: Grammar -> State Env [Either String CGB.Rule]
parseCGRules (Defs defs) = do mapM updateEnv defs 
                              --in case the grammar doesn't specify boundaries 
                              modify ((">>>",bos) :)
                              modify (("<<<",eos) :)
                              env <- get
                              return $ map (parseRules' env) defs
  where updateEnv :: Def -> State Env ()
        updateEnv (SetDef  s) = do nameTags <- transSetDecl s
                                   modify (nameTags :)
        updateEnv (RuleDef r) = do return ()

        parseRules' :: Env -> Def -> Either String CGB.Rule
        parseRules' _ (SetDef  s) = Left $ CG.Print.printTree s
        parseRules' e (RuleDef r) = Right $ evalState (transRule r) e



split :: [CGB.Analysis] -> [CGB.Sentence]
split as = go as []
  where go [] ys = ys
        go xs ys = let beforePunct = takeWhile (not . isPunct) xs 
                       fromPunct   = dropWhile (not . isPunct) xs
                       punct = if null fromPunct then [] else head fromPunct 
                       newxs = if null fromPunct then [] else tail fromPunct
                       newsent = bos:beforePunct ++ punct:eos:[]
                   in go newxs (newsent:ys)

        isPunct :: CGB.Analysis -> Bool
        isPunct = tagsInAna [CGB.Tag "sent", CGB.Lem ".", CGB.Lem "!", CGB.Lem "?"]

        tagsInAna :: [CGB.Tag] -> CGB.Analysis -> Bool
        tagsInAna tags as = or $ map ((not.null) . intersect tags) as

bos = [[CGB.BOS]]
eos = [[CGB.EOS]]

strip :: Int -> String -> String
strip n = drop n . reverse . drop n . reverse

---- CG parsing

transSetDecl :: SetDecl -> State Env (String, CGB.TagSet)
transSetDecl (Set setname tagset) = 
  case setname of
    BOS          -> return (">>>", bos)
    EOS          -> return ("<<<", eos)
    (SetName (UIdent name)) -> do 
      ts <- transTagSet tagset
      return (name, ts)
transSetDecl (List setname tags) = 
  case setname of
    BOS          -> return (">>>", bos)
    EOS          -> return ("<<<", eos)
    (SetName (UIdent name)) -> do 
      tl <- mapM transTag tags
      return (name, concat tl)


                                      

transTag :: Tag -> State Env CGB.TagSet
transTag tag = case tag of
  Lemma (Str s) -> case s of
                   ('"':'<':_) -> return [[CGB.WF (strip 2 s)]]
                   ('"':    _) -> return [[CGB.Lem (strip 1 s)]]
                   _           -> return [[CGB.Lem s]]
  Tag (Id str) -> return [[CGB.Tag str]]
  AND tags     -> do ts <- mapM transTag tags
                     let allInOne = [concat (concat ts)]
                     return allInOne

  Named setname -> case setname of
    (SetName (UIdent name)) -> do
      env <- get
      case lookup name env of
        Nothing -> error $ "Tagset " ++ show name ++ " not defined!"
        Just ts -> (return ts :: State Env CGB.TagSet)
    BOS -> return bos
    EOS -> return eos

transTagSet :: TagSet -> State Env CGB.TagSet
transTagSet ts = case ts of
  TagSet tagset  -> transTagSet tagset
  NilT tag       -> transTag tag
  All            -> return [[]]
  OR tag _or tagset  -> do tags1 <- transTag tag
                           tags2 <- transTagSet tagset
                           return $ tags1 ++ tags2

  ----TODO all set operations!

  Diff All ts    -> do env <- get
                       let allTags = concatMap snd env 
                       rmTags <- transTagSet ts
                       return $ allTags \\ rmTags
  Diff ts All    -> error "something except everything? are you a philosopher?"
  Diff ts1 ts2   -> do tags1 <- transTagSet ts1
                       tags2 <- transTagSet ts2
                       return $ tags1 \\ tags2
  

  Cart ts1 ts2   -> do tags1 <- transTagSet ts1   
                       tags2 <- transTagSet ts2
                       let combs = map concat $ sequence [tags1, tags2]
                       return combs


transRule :: Rule -> State Env CGB.Rule
transRule rl = case rl of
  SelectIf _sl tags _if conds -> liftM2 CGB.Select (transTagSet tags) (transCondSet conds)
  RemoveIf _sl tags _if conds -> liftM2 CGB.Remove (transTagSet tags) (transCondSet conds)
  SelectAlways _sl tags   -> liftM2 CGB.Select (transTagSet tags) (return $ CGB.POS CGB.always)
  RemoveAlways _sl tags   -> liftM2 CGB.Remove (transTagSet tags) (return $ CGB.POS CGB.always)
  MatchLemma (Str lem) rl -> do cgrule <- transRule rl
                                case cgrule of
                                  CGB.Select ts c -> return $ CGB.Select (cart ts lem) c
                                  CGB.Remove ts c -> return $ CGB.Remove (cart ts lem) c
  where cart :: CGB.TagSet -> String -> CGB.TagSet
        cart ts str = case str of
           ('"':'<':_) -> [[CGB.WF  (strip 2 str), t] | t <- concat ts]
           ('"':_    ) -> [[CGB.Lem (strip 1 str), t] | t <- concat ts]
           _           -> [[CGB.Lem str        , t] | t <- concat ts]
        

transCondSet :: [Cond] -> State Env CGB.Test
transCondSet cs = do
  conds <- mapM transCond cs
  return $ CGB.POS $ foldr1 CGB.AND conds

transCond :: Cond -> State Env CGB.Condition
transCond c = case c of
  CPos pos ts         -> liftM2 CGB.C (transPosition pos) (transTagSet' True  ts)
  CNotPos pos ts      -> liftM2 CGB.C (transPosition pos) (transTagSet' False ts)
  CBarrier pos ts bar -> handleBar pos ts bar True
  CNotBar pos ts bar  -> handleBar pos ts bar False
  CTempl templs       -> do conds <- mapM (transCond . (\(Templ c) -> c)) templs
                            return $ foldr1 CGB.OR conds
  CLinked (c:cs)      -> do first@(CGB.C pos tags) <- transCond c
                            let base = getPos pos
                            conds <- mapM transCond cs
                            return $ foldr CGB.AND first (fixPos base conds [])

  where isLink0 []                 = True
        isLink0 (CGB.C pos _ts:cs) = getPos pos == 0 && isLink0 cs

        fixPos base []                  res = res
        fixPos base (CGB.C pos tags:cs) res = 
          let newBase = base + getPos pos
              newPos = changePos pos newBase
          in fixPos newBase cs ((CGB.C newPos tags):res)

        getPos pos =
          case pos of
            CGB.Exactly i -> i 
            CGB.AtLeast i -> i
            CGB.Barrier i _ -> i 

        changePos pos newI = 
          case pos of
            CGB.Exactly i -> CGB.Exactly newI 
            CGB.AtLeast i -> CGB.AtLeast newI
            CGB.Barrier i ts -> CGB.Barrier newI ts 

        transTagSet' :: Bool -> TagSet -> State Env (Bool, CGB.TagSet)
        transTagSet' b ts = 
          do tags <- transTagSet ts
             return (b, tags)

        handleBar pos ts bar bool = 
          do pos' <- transPosition pos
             let int = case pos' of 
                          CGB.Exactly i -> i
                          CGB.AtLeast i -> i
             btags <- transBarrier bar
             liftM (CGB.C $ CGB.Barrier int btags) (transTagSet' bool ts)


transPosition :: Position -> State Env CGB.Position
transPosition pos = return $ case pos of
  Exactly (Signed str)     -> CGB.Exactly $ read str
  AtLeastPre (Signed str)  -> CGB.AtLeast $ read str
  AtLeastPost (Signed str) -> CGB.AtLeast $ read str
  AtLPostUnam (Signed str) -> CGB.AtLeast $ read str
transBarrier :: Barrier -> State Env CGB.TagSet
transBarrier (Barrier ts) = transTagSet ts


--  morpho output parsing

transText :: Text -> [CGB.Analysis]
transText x = case x of
  Lines lines  -> map transLine lines


transLine :: Line -> CGB.Analysis
transLine x = case x of
  Line (Iden wform) anas    -> map (transAnalysis wform) anas
  LinePunct (Punct p) anas  -> map (transAnalysis p) anas
  OnlyPunct (Punct ",")     -> [[CGB.WF ",", CGB.Lem ",", CGB.Tag "cm"]]
  OnlyPunct (Punct ".")     -> [[CGB.WF ".", CGB.Lem ".", CGB.Tag "sent",CGB.EOS]]
  OnlyPunct (Punct str)     -> [[CGB.WF str, CGB.Lem str, CGB.Tag "punct"]]
  NoAnalysis (Iden wform) _ -> [[CGB.WF wform]]


transAnalysis :: String -> Analysis -> [CGB.Tag]
transAnalysis wf ana = CGB.WF wf:transAna ana
  where transAna ana = case ana of
          IdenA (Iden id) tags   -> CGB.Lem id:(map transTagA tags)
          PunctA (Punct ".") tags -> CGB.Lem ".":CGB.EOS:(map transTagA tags)
          PunctA (Punct id) tags -> CGB.Lem id:(map transTagA tags)
          CompA ana1 ana2        -> transAna ana1 ++ transAna ana2
          CollA ana1 ana2        -> transAna ana1 ++ transAna ana2

transTagA :: TagA -> CGB.Tag
transTagA (TagA (Iden id)) = CGB.Tag id


