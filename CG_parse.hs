module CG_parse ( parseRules
                , parseData
                , parseReading
                , readRules
                , readData 
                , readReadings
                , readRules' ) where

import System.Environment ( getArgs )
import System.Exit ( exitFailure )

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
import Text.Regex ( mkRegex )

import qualified CG_base as CGB

data Env = Env { named :: [(String, CGB.TagSet)]
               , unnamed :: [CGB.TagSet] 
               , templates :: [(String,CGB.Condition)] }

emptyEnv = Env [] [] []

--toTags from CGB returns (target::[[Tag]], diff::[[Tag]]).
--This is needed only for the set operation Diff.
--Other set or list operations only have the wanted tags in the first element.
--Also we use toTagsLIST only with LISTs so it's completely safe ^___^
toTagsLIST = concatMap fst . CGB.toTags

parseReading :: String -> CGB.Reading
parseReading str = maintags ++ concat subtags
 where
  (mainr:subrs) = split (=='+') str
  maintags = map toTag $ filter (not.null) $ split isValid mainr
  subrs_ns = map CGB.FromStart [1..] `zip` map (split isValid) subrs :: [(CGB.Subpos,[String])]
  subtags = map (\(n, strs) -> map (CGB.Subreading n . toTag) strs) subrs_ns
  isValid = (=='<') 


  toTag ">>>" = CGB.BOS
  toTag "<<<" = CGB.EOS
  toTag []    = error "empty tag"
  toTag str | last str == '>' = CGB.Tag (init str)
            | last str == '$' = CGB.WF (init str)
            | otherwise       = CGB.Lem str

parseRules :: Bool -> String -> ([CGB.TagSet], [[CGB.Rule]]) -- sections
parseRules test s = case pGrammar (CG.Par.myLexer s) of
            CGErr.Bad err  -> error err
            CGErr.Ok  tree -> let (rules,tags) = runState (parseCGRules tree) emptyEnv
                              in trace (if test then unwords $ pr rules else "") 
                                 ( map snd (named tags) ++ unnamed tags
                                 , map rights rules )
  where pr = concatMap $ map pr'
        pr' (Right rule)  = show rule
        pr' (Left string) = string

parseData :: String -> [CGB.Sentence]
parseData s = case pText (Apertium.Par.myLexer s) of
            AErr.Bad err  -> error err
            AErr.Ok text  -> let cohorts = transText text :: [CGB.Cohort]
                                 sents = split isEOS cohorts
                             in map sentence sents
  where isEOS :: CGB.Cohort -> Bool
        isEOS = tagsInAna [CGB.Tag "sent", CGB.Lem ".", CGB.Lem "!", CGB.Lem "?"]

        tagsInAna :: [CGB.Tag] -> CGB.Cohort -> Bool
        tagsInAna tags = any ((not.null) . intersect tags)

        sentence s = s --[bos] ++ s ++ [fullstop] ++ [eos]

--------------------------------------------------------------------------------
--just because it's nice to use them  rules <- readRules foo
readReadings :: FilePath -> IO [CGB.Reading]
readReadings fname = fmap (map parseReading . words) (readFile fname)

readRules :: FilePath -> IO [[CGB.Rule]]
readRules fname = fmap (snd . parseRules False) (readFile fname)

readRules' :: FilePath -> IO ([CGB.TagSet], [[CGB.Rule]])
readRules' fname = fmap (parseRules False) (readFile fname)


readData :: FilePath -> IO [CGB.Sentence]
readData fname = fmap parseData (readFile fname)

main :: IO ()
main = do args <- getArgs
          case args of
             [file1,file2] -> do foo1 <- fmap (parseRules True) (readFile file1) --verbose
                                 foo2 <- parseData `fmap` readFile file2 
                                 print foo1
                                 print foo2
                                 putStrLn "read rules and data"
             _             -> do putStrLn "Usage: CG_parse <rules> <data>"
                                 exitFailure


--------------------------------------------------------------------------------

parseCGRules :: Grammar -> State Env [[Either String CGB.Rule]]
parseCGRules (Sections secs) = mapM parseSection secs

parseSection :: Section -> State Env [Either String CGB.Rule]
parseSection (Defs defs) =
 do mapM_ updateEnv defs 
    --in case the grammar doesn't specify boundaries 
    modify $ \env -> env { named = (">>>", CGB.TS bos) : named env }
    modify $ \env -> env { named = ("<<<", CGB.TS eos) : named env }
    env <- get
    parseAndModify env defs []
                           
 where
  parseAndModify :: Env -> [Def] -> [Either String CGB.Rule] -> State Env [Either String CGB.Rule]
  parseAndModify e [] acc       = do return acc
  parseAndModify e (d:defs) acc = do let (e', strOrRl) = parseRules' e d
                                     put e'
                                     parseAndModify e' defs (strOrRl:acc)
  
  updateEnv :: Def -> State Env ()
  updateEnv (RuleDef r)  = return ()
  updateEnv (SetDef s)   = do names <- transSetDecl s
                              modify $ \env -> env { named = names : named env }
  updateEnv (TemplDef t) = do templs <- transTemplDecl t
                              modify $ \env -> env { templates = templs : templates env }


  parseRules' :: Env -> Def -> (Env, Either String CGB.Rule)
  parseRules' e (RuleDef  r) = let (r', e') = runState (transRule r) e
                               in (e', Right r')
  parseRules' e (SetDef   s) = (e, Left $ CG.Print.printTree s)
  parseRules' e (TemplDef t) = (e, Left $ CG.Print.printTree t)

split :: (a -> Bool) -> [a] -> [[a]]
split p [] = []
split p xs = takeWhile (not . p) xs : split p (drop 1 (dropWhile (not . p) xs))

bos = [[CGB.BOS]]
eos = [[CGB.EOS]]
fullstop = [[CGB.WF ".", CGB.Lem ".", CGB.Tag "sent"]]

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
    (SetMeta (UIdent name)) -> do 
      ts <- transTagSet tagset
      return ("<" ++ name ++ ">", ts)
    (SetSynt (UIdent name)) -> do 
      ts <- transTagSet tagset
      return ("@" ++ name, ts)

transSetDecl (List setname tags) = 
  case setname of
    BOS          -> return (">>>", CGB.TS bos)
    EOS          -> return ("<<<", CGB.TS eos)
    (SetName (UIdent name)) -> do 
      tl <- mapM transTag tags
      let tl' = concatMap toTagsLIST tl
      return (name, CGB.TS tl')
    (SetMeta nm) -> transSetDecl $ List (SetName nm) tags
    (SetSynt nm) -> transSetDecl $ List (SetName nm) tags

    --(SetMeta (UIdent name)) 
    --  tl <- mapM transTag tags
    --  let tl' = concatMap toTagsLIST tl
    --  return (name, CGB.TS tl')
    --(SetSynt (UIdent name)) -> do 
    --  tl <- mapM transTag tags
    --  let tl' = concatMap toTagsLIST tl
    --  return (name, CGB.TS tl')      

transTemplDecl :: TemplDecl -> State Env (String, CGB.Condition)
transTemplDecl templ = case templ of
  SingleTempl name cond  -> undefined
  ListTempl   name conds -> undefined

transTag :: Tag -> State Env CGB.TagSet
transTag tag = case tag of
  Lemma (Str s) -> do let ts = case s of
                                ('"':'<':_) -> CGB.TS [[CGB.WF (strip 2 s)]]
                                ('"':    _) -> CGB.TS [[CGB.Lem (strip 1 s)]]
                                _           -> CGB.TS [[CGB.Lem s]]
                      modify $ \env -> env { unnamed = ts : unnamed env }
                      return ts
  LemmaCI str   -> transTag (Lemma str) 
  --Regex (Str s) -> do let ts = CGB.TS [[CGB.Rgx (mkRegex s) s]]
  --                    modify $ \env -> env { unnamed = ts : unnamed env }
  --                    return ts
  Regex (Str s) -> return $ CGB.TS [[]]

  Tag (Id str)  -> do let ts = CGB.TS [[CGB.Tag str]]
                      modify $ \env -> env { unnamed = ts : unnamed env }
                      return ts
  AND tags      -> do ts <- mapM transTag tags
                      let ts' = map toTagsLIST ts --safe: only returs TS, no set constructors
                          allInOne = CGB.TS [concat (concat ts')]
                      modify $ \env -> env { unnamed = allInOne : unnamed env }
                      return allInOne

  Named setname -> case setname of
    (SetName (UIdent name)) -> do
      env <- gets named
      case lookup name env of
        Nothing -> error $ "Tagset " ++ show name ++ " not defined!"
        Just ts -> (return ts :: State Env CGB.TagSet)
    (SetMeta (UIdent name)) -> do let ts = CGB.TS [[CGB.Tag name]]
                                  modify $ \env -> env { named = (name,ts) : named env }
                                  return ts
    (SetSynt (UIdent name)) -> do let ts = CGB.TS [[CGB.Tag name]]
                                  modify $ \env -> env { named = (name,ts) : named env }
                                  return ts
    BOS -> return $ CGB.TS bos
    EOS -> return $ CGB.TS eos


transTagSet :: TagSet -> State Env CGB.TagSet
transTagSet ts = case ts of
  TagSet tagset  -> transTagSet tagset
  NilT tag       -> transTag tag
  All            -> return CGB.All
  OR tag _or tagset  -> do tags1 <- transTag tag
                           tags2 <- transTagSet tagset
                           let ts' = CGB.Or tags1 tags2
                           modify $ \env -> env { unnamed = ts' : unnamed env }
                           return ts'

  Diff ts All    -> error "something except everything? are you a philosopher?"
  Diff ts1 ts2   -> do tags1 <- transTagSet ts1
                       tags2 <- transTagSet ts2
                       let ts' = CGB.Diff tags1 tags2
                       modify $ \env -> env { unnamed = ts' : unnamed env }
                       return ts'

  Cart ts1 ts2   -> do tags1 <- transTagSet ts1   
                       tags2 <- transTagSet ts2
                       let ts' = CGB.Cart tags1 tags2
                       modify $ \env -> env { unnamed = ts' : unnamed env }
                       return ts'

transRule :: Rule -> State Env CGB.Rule
transRule rl = case rl of
  SelectIf (SELECT1 nm sr) tags _if conds ->
    liftM2 (CGB.Select (getName nm)) (transTagSet tags) (transCondSet conds)
  RemoveIf (REMOVE1 nm sr) tags _if conds ->
    liftM2 (CGB.Remove (getName nm)) (transTagSet tags) (transCondSet conds)
  SelectAlways (SELECT1 nm sr) tags ->
    liftM2 (CGB.Select (getName nm)) (transTagSet tags) (return CGB.Always)
  RemoveAlways (REMOVE1 nm sr) tags ->
    liftM2 (CGB.Remove (getName nm)) (transTagSet tags) (return CGB.Always)
  MatchLemma (Str lem) rl -> 
    do cgrule <- transRule rl
       case cgrule of
         CGB.Select n ts c 
           -> do let ts' = cart ts lem
                 modify $ \env -> env { named = (lem,ts') : named env }
                 return $ CGB.Select n ts' c
         CGB.Remove n ts c
           -> do let ts' = cart ts lem
                 modify $ \env -> env { unnamed = ts' : unnamed env }
                 return $ CGB.Remove n ts' c

  where cart :: CGB.TagSet -> String -> CGB.TagSet
        cart ts str = case str of
           ('"':'<':_) -> CGB.TS [[CGB.WF  (strip 2 str), t]
                                    | t <- concat (toTagsLIST ts)]
           ('"':_    ) -> CGB.TS [[CGB.Lem (strip 1 str), t] 
                                    | t <- concat (toTagsLIST ts)]
           _           -> CGB.TS [[CGB.Lem str          , t] 
                                    | t <- concat (toTagsLIST ts)]

        getName (MaybeName1 (Id id)) = CGB.Name id
        getName MaybeName2           = CGB.NoName

        getSubr (SubrTarget (Signed str)) 
            = let i = read str in
              Just $ if i<0 then CGB.FromStart i else CGB.FromEnd i
        getSubr SubrTargetStar  = Just CGB.Wherever
        getSubr SubrEmpty       = Nothing
        

transCondSet :: [Cond] -> State Env CGB.Condition
transCondSet cs = do
  conds <- mapM transCond cs
  return $ foldr1 CGB.AND conds


transCond :: Cond -> State Env CGB.Condition
transCond c = case c of
--  CondPos pos ts          -> liftM2 CGB.C (transPosition pos) (transTagSet' True  ts)
  CondPos pos ts          -> do (pos', subr) <- transPosition pos 
                                ts' <- transTagSet ts
                                return $ case subr of
                                  Nothing -> CGB.C pos' (CGB.Pos, ts')
                                  Just i  -> CGB.C pos' (CGB.Pos, mapSubr i ts')

  CondNotPos pos ts       -> do (pos', subr) <- transPosition pos 
                                ts' <- transTagSet ts
                                return $ case subr of
                                  Nothing -> CGB.C pos' (CGB.Neg, ts')
                                  Just i  -> CGB.C pos' (CGB.Neg, mapSubr i ts')
  CondBarrier pos ts bts  -> barrier pos ts bts CGB.Pos CGB.NotCareful
  CondNotBar pos ts bts   -> barrier pos ts bts CGB.Neg CGB.NotCareful
  CondCBarrier pos ts bts -> barrier pos ts bts CGB.Pos CGB.Careful
  CondNotCBar pos ts bts  -> barrier pos ts bts CGB.Neg CGB.Careful
  CondTemplate (SetName (UIdent templName))
                          -> do templs <- gets templates 
                                let Just cond = lookup templName templs
                                return cond
  CondTemplInl templs     -> do cs <- mapM (transCond . (\(Template c) -> c)) templs
                                return $ foldr1 CGB.OR cs

  --TODO 
  CondLinked (c1:c2:cs)   -> do parent@(CGB.C posParent _tags) <- transCond c1
                                firstChild@(CGB.C posBase _tags) <- transCond c2
                                let base = getPos posBase
                                otherChildren <- mapM transCond cs
                                let fixedConds = map (addParent posParent) (firstChild:fixPos base otherChildren [])
                                return $ foldr CGB.AND parent fixedConds


  where fixPos base []                  res = res
        fixPos base c@(CGB.C pos tags:cs) res = --trace (show c ++ " " ++ show res) $
          let newBase = base + getPos pos
              newPos = changePos pos newBase
          in fixPos newBase cs (CGB.C newPos tags:res)

        getPos pos =
          case pos of
            CGB.Exactly _ i -> i 
            CGB.AtLeast _ i -> i
            CGB.Barrier _ _ i _ -> i 

        changePos pos newI = 
          case pos of
            CGB.Exactly c i -> CGB.Exactly c newI 
            CGB.AtLeast c i -> CGB.AtLeast c newI
            CGB.Barrier c bc i ts -> CGB.Barrier c bc newI ts 

        barrier pos ts bts isPositive bcaut =
          do pos' <- transPosition pos
             let (caut,int) = case fst pos' of 
                          CGB.Exactly b i -> (b,i)
                          CGB.AtLeast b i -> (b,i)
             btags <- transTagSet bts  --there is no `BARRIER NOT ts' option,
                                       --you just write `BARRIER (*)-ts'
             ctags <- transTagSet ts
             return $ CGB.C (CGB.Barrier caut bcaut int btags) (isPositive,ctags)

        mapSubr i (CGB.TS ts) = CGB.TS $ (map.map) (CGB.Subreading i) ts
        mapSubr i (CGB.Diff ts1 ts2) = CGB.Diff (mapSubr i ts1) (mapSubr i ts2)
        mapSubr i (CGB.Cart ts1 ts2) = CGB.Cart (mapSubr i ts1) (mapSubr i ts2)

        addParent parent (CGB.C pos tags) = CGB.C (CGB.LINK parent pos) tags


transPosition :: Position -> State Env (CGB.Position, Maybe CGB.Subpos)
transPosition pos = case pos of
  Exactly (Signed num)  
               -> mainreading $ CGB.Exactly CGB.NotCareful $ read num
  AtLeastPre (Signed num)  
               -> mainreading $ CGB.AtLeast CGB.NotCareful $ read num
  AtLeastPost (Signed num) 
               -> mainreading $ CGB.AtLeast CGB.NotCareful $ read num
  AtLPostCaut1 (Signed num)
               -> mainreading $ CGB.AtLeast CGB.Careful $ read num
  AtLPostCaut2 (Signed num) 
              -> mainreading $ CGB.AtLeast CGB.Careful $ read num
  Subreading (Signed num1) (Signed num2)
              -> return (CGB.Exactly CGB.NotCareful $ read num1
                        , Just $ if read num2 < 0
                                  then CGB.FromEnd (read num2)
                                  else CGB.FromStart (read num2))

  SubreadingStar (Signed num) 
              -> return (CGB.Exactly CGB.NotCareful $ read num, Just CGB.Wherever)
  Cautious position  
              -> cautious `fmap` transPosition position
 where cautious (CGB.Exactly _b num, subr) = (CGB.Exactly CGB.Careful num, subr)
       cautious (CGB.AtLeast _b num, subr) = (CGB.AtLeast CGB.Careful num, subr)
       mainreading position = return (position, Nothing)

--  morpho output parsing
-- type Reading = [Tag]

transText :: Text -> [CGB.Cohort] --CGB.Sentence
transText x = case x of
  Lines lines  -> map transLine lines

transLine :: Line -> CGB.Cohort
transLine x = case x of
  Line (Iden wform) anas    -> map (transAnalysis 0 wform) anas
  LinePunct (Punct p) anas  -> map (transAnalysis 0 p) anas
  OnlyPunct (Punct ",")     -> [[CGB.WF ",", CGB.Lem ",", CGB.Tag "cm"]]
  OnlyPunct (Punct ".")     -> [[CGB.WF ".", CGB.Lem ".", CGB.Tag "sent",CGB.EOS]]
  OnlyPunct (Punct str)     -> [[CGB.WF str, CGB.Lem str, CGB.Tag "punct"]]
  NoAnalysis (Iden wform) _ -> [[CGB.WF wform]]


transAnalysis :: Integer -> String -> Analysis -> CGB.Reading --[CGB.Tag]
transAnalysis i wf ana = CGB.WF wf:transAna i ana
  where transAna 0 ana = case ana of
          IdenA (Iden id) tags    -> CGB.Lem id:map transTagA tags
          PunctA (Punct ".") tags -> CGB.Lem ".":CGB.EOS:map transTagA tags
          PunctA (Punct id) tags -> CGB.Lem id:map transTagA tags
          SubrA ana1 ana2        -> transAna i ana1 ++ transAna (i+1) ana2
          MweA ana1 ana2        -> transAna i ana1 ++ transAna i ana2 --no subreadings for MWEs
        transAna n (SubrA ana1 ana2) --ana1 is IdenA, ana2 can be another SubrA 
                       = CGB.Subreading (CGB.FromStart n) `fmap` transAna 0 ana1 ++ transAna (n+1) ana2
        transAna n ana = CGB.Subreading (CGB.FromStart n) `fmap` transAna 0 ana --last one of subreadings

transTagA :: TagA -> CGB.Tag
transTagA (TagA (Iden id)) = CGB.Tag id