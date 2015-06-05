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


parseRules :: Bool -> String -> [[CGB.Rule]] -- sections
parseRules test s = case pGrammar (CG.Par.myLexer s) of
            CGErr.Bad err  -> error err
            CGErr.Ok  tree -> let rules = evalState (parseCGRules tree) []
                              in trace (if test then unwords $ pr rules else "") $
                                 map rights rules
  where pr = concatMap $ map pr'
        pr' (Right rule)  = show rule
        pr' (Left string) = string

parseData :: String -> [CGB.Sentence]
parseData s = case pText (Apertium.Par.myLexer s) of
            AErr.Bad err  -> error err
            AErr.Ok text  -> map sentence $ split isSent $ transText text
  where isSent :: CGB.Analysis -> Bool
        isSent = tagsInAna [CGB.Tag "sent", CGB.Lem ".", CGB.Lem "!", CGB.Lem "?"]

        tagsInAna :: [CGB.Tag] -> CGB.Analysis -> Bool
        tagsInAna tags as = any ((not.null) . intersect tags) as

        sentence s = [bos] ++ s ++ [eos]


--just because it's nice to use them  rules <- readRules foo
readRules :: String -> IO [[CGB.Rule]]
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

parseCGRules :: Grammar -> State Env [[Either String CGB.Rule]]
parseCGRules (Sections secs) = mapM parseSection secs

parseSection :: Section -> State Env [Either String CGB.Rule]
parseSection (Defs defs) = do mapM updateEnv defs 
                              --in case the grammar doesn't specify boundaries 
                              modify ((">>>", CGB.TS bos) :)
                              modify (("<<<", CGB.TS eos) :)
                              env <- get
                              return $ map (parseRules' env) defs
  where updateEnv :: Def -> State Env ()
        updateEnv (SetDef  s) = do nameTags <- transSetDecl s
                                   modify (nameTags :)
        updateEnv (RuleDef r) = do return ()

        parseRules' :: Env -> Def -> Either String CGB.Rule
        parseRules' _ (SetDef  s) = Left $ CG.Print.printTree s
        parseRules' e (RuleDef r) = Right $ evalState (transRule r) e


split :: (a -> Bool) -> [a] -> [[a]]
split p [] = []
split p xs = takeWhile (not . p) xs : split p (drop 1 (dropWhile (not . p) xs))

bos = [[CGB.BOS]]
eos = [[CGB.EOS]]

strip :: Int -> String -> String
strip n = drop n . reverse . drop n . reverse

---- CG parsing

transSetDecl :: SetDecl -> State Env (String, CGB.TagSet)
transSetDecl (Set setname tagset) = 
  case setname of
    BOS          -> return (">>>", CGB.TS bos)
    EOS          -> return ("<<<", CGB.TS eos)
    (SetName (UIdent name)) -> do 
      ts <- transTagSet tagset
      return (name, ts)
transSetDecl (List setname tags) = 
  case setname of
    BOS          -> return (">>>", CGB.TS bos)
    EOS          -> return ("<<<", CGB.TS eos)
    (SetName (UIdent name)) -> do 
      tl <- mapM transTag tags
      let tl' = concatMap CGB.toTags tl
      return (name, CGB.TS tl')


                                      

transTag :: Tag -> State Env CGB.TagSet
transTag tag = case tag of
  Lemma (Str s) -> case s of
                   ('"':'<':_) -> return $ CGB.TS [[CGB.WF (strip 2 s)]]
                   ('"':    _) -> return $ CGB.TS [[CGB.Lem (strip 1 s)]]
                   _           -> return $ CGB.TS [[CGB.Lem s]]
  Tag (Id str) -> return $ CGB.TS [[CGB.Tag str]]
  AND tags     -> do ts <- mapM transTag tags
                     let ts' = map CGB.toTags ts
                         allInOne = [concat (concat ts')]
                     return $ CGB.TS allInOne

  Named setname -> case setname of
    (SetName (UIdent name)) -> do
      env <- get
      case lookup name env of
        Nothing -> error $ "Tagset " ++ show name ++ " not defined!"
        Just ts -> (return ts :: State Env CGB.TagSet)
    BOS -> return $ CGB.TS bos
    EOS -> return $ CGB.TS eos


transTagSet :: TagSet -> State Env CGB.TagSet
transTagSet ts = case ts of
  TagSet tagset  -> transTagSet tagset
  NilT tag       -> transTag tag
  All            -> return $ CGB.All
  OR tag _or tagset  -> do tags1 <- transTag tag
                           tags2 <- transTagSet tagset
                           return $ CGB.Or tags1 tags2

  ----TODO all set operations!

  Diff All ts    -> do env <- get
                       let allTags = concatMap (CGB.toTags . snd) env :: [[CGB.Tag]]
                       rmTags <- transTagSet ts
                       return $ CGB.Diff (CGB.TS allTags) rmTags
  Diff ts All    -> error "something except everything? are you a philosopher?"
  Diff ts1 ts2   -> do tags1 <- transTagSet ts1
                       tags2 <- transTagSet ts2
                       return $ CGB.Diff tags1 tags2
  

  Cart ts1 ts2   -> do tags1 <- transTagSet ts1   
                       tags2 <- transTagSet ts2
                       return $ CGB.Cart tags1 tags2


transRule :: Rule -> State Env CGB.Rule
transRule rl = case rl of
  SelectIf (SELECT_1 nm) tags _if conds ->
    liftM2 (CGB.Select (getName nm)) (transTagSet tags) (transCondSet conds)
  RemoveIf (REMOVE_1 nm) tags _if conds ->
    liftM2 (CGB.Remove (getName nm)) (transTagSet tags) (transCondSet conds)
  SelectAlways (SELECT_1 nm) tags ->
    liftM2 (CGB.Select (getName nm)) (transTagSet tags) (return CGB.Always)
  RemoveAlways (REMOVE_1 nm) tags ->
    liftM2 (CGB.Remove (getName nm)) (transTagSet tags) (return CGB.Always)
  MatchLemma (Str lem) rl -> 
    do cgrule <- transRule rl
       case cgrule of
         CGB.Select n ts c -> return $ CGB.Select n (cart ts lem) c
         CGB.Remove n ts c -> return $ CGB.Remove n (cart ts lem) c
  where cart :: CGB.TagSet -> String -> CGB.TagSet
        cart ts str = case str of
           ('"':'<':_) -> CGB.TS [[CGB.WF  (strip 2 str), t]
                                    | t <- concat (CGB.toTags ts)]
           ('"':_    ) -> CGB.TS [[CGB.Lem (strip 1 str), t] 
                                    | t <- concat (CGB.toTags ts)]
           _           -> CGB.TS [[CGB.Lem str          , t] 
                                    | t <- concat (CGB.toTags ts)]

        getName (MaybeName_1 (Id id)) = CGB.Name id
        getName MaybeName_2           = CGB.NoName
        

transCondSet :: [Cond] -> State Env CGB.Condition
transCondSet cs = do
  conds <- mapM transCond cs
  return $ foldr1 CGB.AND conds

transCond :: Cond -> State Env CGB.Condition
transCond c = case c of
  CPos pos ts         -> liftM2 CGB.C (transPosition pos) (transTagSet' True  ts)
  CNotPos pos ts      -> liftM2 CGB.C (transPosition pos) (transTagSet' False ts)
  CBarrier pos ts bar -> handleBar pos ts bar True
  CNotBar pos ts bar  -> handleBar pos ts bar False
  CTempl templs       -> do cs <- mapM (transCond . (\(Templ c) -> c)) templs
                            return $ foldr1 CGB.OR cs
  CLinked (c:cs)      -> do first@(CGB.C pos tags) <- transCond c
                            let base = getPos pos
                            conds <- mapM transCond cs
                            return $ foldr CGB.AND first (fixPos base conds [])

  where fixPos base []                  res = res
        fixPos base (CGB.C pos tags:cs) res = 
          let newBase = base + getPos pos
              newPos = changePos pos newBase
          in fixPos newBase cs ((CGB.C newPos tags):res)

        getPos pos =
          case pos of
            CGB.Exactly _ i -> i 
            CGB.AtLeast _ i -> i
            CGB.Barrier i _ -> i 

        changePos pos newI = 
          case pos of
            CGB.Exactly b i -> CGB.Exactly b newI 
            CGB.AtLeast b i -> CGB.AtLeast b newI
            CGB.Barrier i ts -> CGB.Barrier newI ts 

        transTagSet' :: Bool -> TagSet -> State Env (Bool, CGB.TagSet)
        transTagSet' b ts = 
          do tags <- transTagSet ts
             return (b, tags)

        handleBar pos ts bar bool = 
          do pos' <- transPosition pos
             let int = case pos' of 
                          CGB.Exactly _ i -> i
                          CGB.AtLeast _ i -> i
             btags <- transBarrier bar
             liftM (CGB.C $ CGB.Barrier int btags) (transTagSet' bool ts)



transPosition :: Position -> State Env CGB.Position
transPosition pos = case pos of
  Exactly (Signed str)     -> return $ CGB.Exactly False $ read str
  AtLeastPre (Signed str)  -> return $ CGB.AtLeast False $ read str
  AtLeastPost (Signed str) -> return $ CGB.AtLeast False $ read str
  AtLPostCaut (Signed str) -> return $ CGB.AtLeast True $ read str
  Cautious position        -> cautious `fmap` transPosition position
  where cautious (CGB.Exactly _b num) = CGB.Exactly True num
        cautious (CGB.AtLeast _b num) = CGB.AtLeast True num

transBarrier :: Barrier -> State Env CGB.TagSet
transBarrier (Barrier ts) = transTagSet ts


--  morpho output parsing
-- type Analysis = [[Tag]]

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


