module CG_SAT (
  apply
)
where

-- TODO!!! Make all the apply etc. functions here; unify format between Analyse and Disambiguate!

apply :: Solver -> WIndSet -> Sentence -> Rule' -> IO Sentence
apply s allinds sentence rule = do
  --putStrLn ("apply " ++ show rule)
  let (trgIndsRaw,_) = trg rule -- :: IntSet
  let otherIndsRaw   = allinds IS.\\ trgIndsRaw
  let (trgInds,otherInds) = if isSelect' rule
                              then (otherIndsRaw,trgIndsRaw)
                              else (trgIndsRaw,otherIndsRaw)

  --         :: Sentence -> (SIndex,Word) -> Sentence
  let applyToWord sentence (i,word) = do
       disjConds <- mkConds s allinds sentence i (cnd rule) "apply"
       case disjConds of
         Nothing -> return sentence --conditions out of scope, no changes in sentence
         Just cs -> do
           let trgIndsList = IS.toList trgInds

           condsHold <- orl' s cs
           let trgPos   = mapMaybe   (lu' word) trgIndsList
           let otherNeg = map (neg . lu word) (IS.toList otherInds)

           someTrgIsTrue <- orl' s trgPos 
           noOtherIsTrue <- andl' s otherNeg
           onlyTrgLeft <- andl' s [ someTrgIsTrue, noOtherIsTrue ]
           cannotApply <- orl' s [ neg condsHold, onlyTrgLeft ]

           newTrgLits <- sequence
             --wN<a>' is true if both of the following:
             [ andl s newTrgName [ oldTrgLit     --wN<a> was also true, and
                                 , cannotApply ] --rule cannot apply 
               | oldTrgLit <- trgPos
               , let newTrgName = show oldTrgLit ++ "'" ]
           let newsw = foldl changeAna word (zip trgIndsList newTrgLits)
           return $ changeWord sentence i newsw
           
  foldM applyToWord sentence (IM.assocs sentence)

 where

  changeAna :: Word -> (WIndex,Lit) -> Word
  changeAna word (i,newana) = IM.adjust (const newana) i word

  changeWord :: Sentence -> SIndex -> Word -> Sentence
  changeWord sent i newsw = IM.adjust (const newsw) i sent

  lu' xs x = IM.lookup x xs
  lu  xs x = IM.findWithDefault false x xs -- neg will be called, so false will turn into true. I imagine that this is faster than call map twice?
--------------------------------------------------------------------------------
