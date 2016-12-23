module CG_data where

import CG_base
import CG_parse

-- Analyses
the = ("the",[[WF "the", Lem "the", Tag "det"]])
bear = ("bear", [WF "bear", Lem "bear", Tag "n", Tag "sg"] : [[WF "bear", Lem "bear", Tag "vblex",y] | y <- [Tag "pl", Tag "fin"]])

sleeps = ("sleeps",[[WF "sleeps", Lem "sleep", Tag "n", Tag "pl"],
          [WF "sleeps",Lem "sleep", Tag "vblex",Tag "sg", Tag "p3"]])

sleep = ("sleep",[[WF "sleep", Lem "sleep", Tag "n" ,Tag "sg"],
         [WF "sleep",Lem "sleep", Tag "vblex",Tag "inf"],
         [WF "sleep",Lem "sleep", Tag "vblex",Tag "pres"]])

in_ = ("in", [[WF "in", Lem "in", Tag "prep"],
       [WF "in", Lem "in", Tag "adv"]])

they = ("they",[[WF "they", Lem "they", Tag "pron", Tag "subj", Tag "nom", Tag "p3"]])

are = ("are",[[WF "are", Lem "be", Tag "vblex", Tag "pres"], [WF "are", Lem "are", Tag "n", Tag "sg"]])

both_ = ("both",[[WF "both", Lem "both", Tag "cnjcoo"], [WF "both", Lem "both", Tag "det"], [WF "both", Lem "both", Tag "pron"]])

happy = ("happy",[[WF "happy", Lem "happy", Tag "adj"]])

and_ = [[WF "and", Lem "and", Tag "cnjcoo"]]

go_ = map (WF "go":) [[Lem "go", Tag "vblex", Tag "inf"], [Lem "go", Tag "vblex", Tag "pres"], [Lem "go", Tag "n", Tag "sg"]]

to = map (WF "to":) [[Lem "to", Tag "prep"],
       [Lem "to", Tag "adv"]]

house = map (WF "house":) [[Lem "house", Tag "vblex", Tag "inf"], [Lem "house", Tag "vblex", Tag "pres"], [Lem "house", Tag "n", Tag "sg"]]

comma = [[WF ",", Lem ",", Tag "punct"]]


-- Sentence
ex0 = [ snd the --unambiguous
      , snd bear
      , snd sleeps 
      , snd in_
      , snd the
      , house ]

ex1 = [ snd they
      , snd are
      , snd both_
      , snd happy
      , and_
      , go_
      , to
      , snd the
      , house ]

ex2 = [ snd both_
      , house
      , and_
      , snd bear
      , snd sleep
      , comma
      , snd they
      , go_
      , and_
      , snd sleep ]

exs = [ex2, ex1, ex0]

----------------------------------------------------------------------

ex_abc1 = concat $ snd $ parseRules False
     ( "REMOVE:r1 (a) IF (-1 (c)) ; " ++
       "REMOVE:l  (a) IF (-1C  (c)) ; ")

ex_abc2 = concat $ snd $ parseRules False
     ( "REMOVE:r1 (a) IF (-1 (*) - (b)) ;" ++
       "REMOVE:r2 (b) IF ( 1 (a)) ;" ++
       "REMOVE:r3 (a) IF (-1 (b)) ;" ++
       "REMOVE:l  (a) IF (-1 (c)) ;" )

ex_tricky1 = concat $ snd $ parseRules False
     ( "REMOVE:r1 (a) IF (-1C (b)) ;" ++
       "REMOVE:r2 (b) IF ( 1 (a)) ;" ++    --should not remove
       "REMOVE:l  (a) IF (-1 (b)) ;" )

ex_tricky2 = concat $ snd $ parseRules False
     ( "REMOVE:r1 (a) IF (-1C (b)) ;" ++
       "SELECT:r2 (b) IF ( 1 (a)) ;" ++    --should select
       "REMOVE:l  (a) IF (-1 (b)) ;" )

ex_not = concat $ snd $ parseRules False
     ( "REMOVE:r1 (a) IF (NOT -1 (c)) ;" ++
       "REMOVE:r2 (a) IF (-1 (a)) ;" ++
       "REMOVE:l  (b) IF (-1 (c)) ;" )

kimmo_explicit = concat $ snd $ parseRules False
     ( "REMOVE:r1 (a) IF (0 (b)) (-1 (c)) ;" ++
       "REMOVE:r2 (b) IF (0 (a)) (-1 (c)) ;" ) -- ++
   --    "REMOVE:l  (c) IF (-1 (c)) ;" )

kimmo_implicit = concat $ snd $ parseRules False
     ( "REMOVE:r1 (a) IF  (-1 (c)) ;" ++
       "REMOVE:r2 (b) IF  (-1 (c)) ;" ) 
--       "REMOVE:l  (c) IF (-1 (c)) ;" )