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


### Some remarks about creating a list of all readings

* Sentence boundary (BOS/EOS) is a word in the symbolic sentence
* `sent` is also a word in the symbolic sentence. The BOS/EOS (>>>/<<<) are magic tags by VISL CG-3, and I adopt the convention. When parsing, I split text into sentences by punctuation (yep not gonna work for A.C.R.O.N.Y.M.S.) and wrap it in BOS _ EOS.

For grammar testing, I generate a list of all readings from the Apertium lexicon. I do

`lt-expand apertium-<lang>.<lang>.dix | sed 's/:[<>]:/:/' | cut -f 2 -d :` to get rid of the word forms. This still has lemmas, so you can remove everything on the lines before the first `<`. Then just `sort -u`, and you have a list of readings, such as `<det><def><mf><sg>, <det><def><mfn><pl>`.

However, the grammar often contains tag combinations that have lemmas or word forms in them, such as `REMOVE ("haar") + Noun IF (1 Noun)`. We add these combinations to the list of readings as follows:


```
for i in `egrep -v "^ *#|^DELIMITERS|^SOFT-DELIMITERS" fin.rlx | egrep -o "\"[^\"]*\"" | tr -d '"'`; do echo $i | sed -E 's/([^:]*):([^<]*)(<.*)/\3<\2/ ; done | sort -u
```

If we want the word form, replace the sed with `sed -E 's/([^:]*):([^<]*)(<.*)/\3<\1/`. This is due to the way I'm parsing it: `<` is the split character, and then I check the end, if there's a `>`, then it's a normal tag, otherwise lemma. (I didn't put in special case for word form, but that is easy to add.)


```

------------------------

Links to some papers about CG


http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.50.1197 -- CG rules from annotated corpora (1996) 
http://link.springer.com/chapter/10.1007/BFb0027315             -- Inductive logic programming (1998)
http://swarm.cs.pub.ro/~asfrent/msc/thesis.pdf                  -- Inductive logic programming (2014, MSc thesis)
http://stp.lingfil.uu.se/nodalida01/pdf/lager.pdf               -- CG rules from Brill tagger learning rules (2000)
http://www.ling.gu.se/~lager/Mutbl/Papers/lager_nivre.pdf       -- POS tagging from logical point of view, comparison of 4 different methods (2001)
        


