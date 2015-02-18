module CG_data where

import CG

-- Analyses
the = [Lem "the":det]
bear = [Lem "bear", Tag "n", Tag "sg"] : [[Lem "bear", Tag "vblex",y] | y <- [Tag "pl", Tag "fin"]]

sleeps = [[Lem "sleep", Tag "n", Tag "pl"],
          [Lem "sleep", Tag "vblex",Tag "sg", Tag "p3"]]

sleep = [[Lem "sleep", Tag "n" ,Tag "sg"],
         [Lem "sleep", Tag "vblex",Tag "inf"],
         [Lem "sleep", Tag "vblex",Tag "pres"]]

in_ = [[Lem "in", Tag "prep"],
       [Lem "in", Tag "adv"]]

they = [[Lem "they", Tag "pron", Tag "subj", Tag "nom", Tag "p3"]]

are = [[Lem "be", Tag "vblex", Tag "pres"], [Lem "are", Tag "n", Tag "sg"]]

both_ = [[Lem "both", Tag "cnjcoo"], [Lem "both", Tag "det"], [Lem "both", Tag "pron"]]

happy = [[Lem "happy", Tag "adj"], [Lem "happy", Tag "particle"]] --just for test

and_ = [[Lem "and", Tag "cnjcoo"]]

go = [[Lem "go", Tag "vblex", Tag "inf"], [Lem "go", Tag "vblex", Tag "pres"], [Lem "go", Tag "n", Tag "sg"]]

to = [[Lem "to", Tag "prep"],
       [Lem "to", Tag "adv"]]

house = [[Lem "house", Tag "vblex", Tag "inf"], [Lem "house", Tag "vblex", Tag "pres"], [Lem "house", Tag "n", Tag "sg"]]

comma = [[Lem ",", Tag "punct"]]

part = [[Lem "particletest", Tag "particle"]]

-- Sentence
ex0 = [ the --unambiguous
      , bear
      , sleeps 
      , in_
      , the
      , house ]

ex1 = [ they
      , are
      , both_
      , happy
      , and_
      , go
      , to
      , the
      , house ]

ex2 = [ both_
      , house
      , and_
      , bear
      , sleep
      , comma
      , they
      , go
      , and_
      , sleep ]

exs = [ex2, ex1, ex0]