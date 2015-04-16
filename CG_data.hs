module CG_data where

import CG_base

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

they = ("they",[[Lem "they", Tag "pron", Tag "subj", Tag "nom", Tag "p3"]])

are = ("are",[[Lem "be", Tag "vblex", Tag "pres"], [Lem "are", Tag "n", Tag "sg"]])

both_ = ("both",[[Lem "both", Tag "cnjcoo"], [Lem "both", Tag "det"], [Lem "both", Tag "pron"]])

happy = ("happy",[[Lem "happy", Tag "adj"]])

and_ = ("and",[[Lem "and", Tag "cnjcoo"]])

go_ = ("go",[[Lem "go", Tag "vblex", Tag "inf"], [Lem "go", Tag "vblex", Tag "pres"], [Lem "go", Tag "n", Tag "sg"]])

to = ("to",[[Lem "to", Tag "prep"],
       [Lem "to", Tag "adv"]])

house = ("house",[[WF "house", Lem "house", Tag "vblex", Tag "inf"], [WF "house", Lem "house", Tag "vblex", Tag "pres"], [WF "house", Lem "house", Tag "n", Tag "sg"]])

comma = (",",[[Lem ",", Tag "punct"]])


-- Sentence
ex0 = [ snd the --unambiguous
      , snd bear
      , snd sleeps 
      , snd in_
      , snd the
      , snd house ]

ex1 = [ snd they
      , snd are
      , snd both_
      , snd happy
      , snd and_
      , snd go_
      , snd to
      , snd the
      , snd house ]

ex2 = [ snd both_
      , snd house
      , snd and_
      , snd bear
      , snd sleep
      , snd comma
      , snd they
      , snd go_
      , snd and_
      , snd sleep ]

exs = [ex2, ex1, ex0]