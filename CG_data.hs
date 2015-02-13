module CG_data where

import CG

-- Analyses
the = [[Lem "the", Det]]
bear = [Lem "bear", N,Sg] : [[Lem "bear", V,y] | y <- [Pl]]
--bear = [Lem "bear", N,Sg] : [[Lem "bear", V,y,z,w] | y <- [Sg,Pl], z <- [P1,P2], w <- [Subj,Imper]]
--bear = [Lem "bear", N,Sg] : [[Lem "bear", V,y,z] | y <- [Sg,Pl], z <- [P1,P2]]

sleeps = [[Lem "sleep", N,Pl],
          [Lem "sleep", V,Sg,P3]]

sleep = [[Lem "sleep", N,Sg],
         [Lem "sleep", V,Inf],
         [Lem "sleep", V,Pres]]

in_ = [[Lem "in", Prep],
       [Lem "in", Adv]]

they = [[Lem "they", Pron, Subj, Nom, P3]]

are = [[Lem "be", V, Pres], [Lem "are", N, Sg]]

both_ = [[Lem "both", CoordConj], [Lem "both", Det], [Lem "both", Pron]]

happy = [[Lem "happy", Adj], [Lem "happy", Particle]] --just for test

and_ = [[Lem "and", CoordConj]]

go = [[Lem "go", V, Inf], [Lem "go", V, Pres], [Lem "go", N, Sg]]

to = [[Lem "to", Prep],
       [Lem "to", Adv]]

house = [[Lem "house", V, Inf], [Lem "house", V, Pres], [Lem "house", N, Sg]]

comma = [[Lem ",", Punct]]

part = [[Lem "particletest", Particle]]

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