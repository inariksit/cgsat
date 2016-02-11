cgsat
=====

Implementing Constraint Grammar using SAT solver.

Installation: just type make.
It will first compile the BNFC files and then just do cabal configure && cabal build.

It's using https://github.com/koengit/satplus/ , not on hackage (?)

-----------------------

bnfc
  - BNFC grammars for parsing CG rules and Apertium input format.

data
  - Spanish, English, Hungarian and Finnish CG rules and morpho analysed data to use as examples

executable
  - Main: just to test disambiguate functions quickly, no comparison to any other
  - Symbolic: preliminary work on conflict detection, using SAT solver to generate input

test
  - Test: comparison with VISL CG-3 with or without gold standard + some other small stuff
  - QCTest: some QuickCheck tests (mostly because it's fun to random generate rules)



------------------------

Links to some papers about CG


http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.50.1197 -- CG rules from annotated corpora (1996) 
http://link.springer.com/chapter/10.1007/BFb0027315             -- Inductive logic programming (1998)
http://swarm.cs.pub.ro/~asfrent/msc/thesis.pdf                  -- Inductive logic programming (2014, MSc thesis)
http://stp.lingfil.uu.se/nodalida01/pdf/lager.pdf               -- CG rules from Brill tagger learning rules (2000)
http://www.ling.gu.se/~lager/Mutbl/Papers/lager_nivre.pdf       -- POS tagging from logical point of view, comparison of 4 different methods (2001)
        


