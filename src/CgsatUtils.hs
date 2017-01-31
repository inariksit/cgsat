module CgsatUtils ( envRules
				  )
where

import Rule 
import CG_SAT
import CghsUtils ( parseReadingApe, parseReadingApeSubr
				 , readTag, removeLexReading, tagSet2Readings )
import Parse ( parse )
import SAT ( Solver )

import Control.Monad ( when )
import Data.List ( nub, partition )

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

	  putStrLn "\n\n=========="
	  mapM_ print lemmas
	  mapM_ print wforms
	  putStrLn "==========\n\n"

  return (env,rules)

selOrRm :: Oper -> Bool
selOrRm SELECT = True
selOrRm REMOVE = True
selOrRm _ = False

isLem :: Tag -> Bool
isLem (Lem _) = True
isLem _       = False