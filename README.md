cgsat
=====

Implementing Constraint Grammar using SAT solver.

### Download:

`git clone https://github.com/inariksit/cgsat`

It's using [SAT+](https://github.com/koengit/satplus) by [Koen Claessen](https://github.com/koengit/), more precisely, [my fork](https://github.com/inariksit/satplus). As of December 2016, I've moved the actual Haskell format and the BNFC parser into another repository,  [cghs](https://github.com/inariksit/cghs). All of this should be included in `stack.yaml`.

### Install:

Type `stack build`. It should fetch stuff from my two other git repositories. You should also have `bnfc`, `happy` and `alex` as executables. 


### Structure of the repository:

`app`
  - Disambiguate: Use SAT-based implementation as a disambiguator, just like VISL CG-3 or any other CG engine out there.
  - Analyse: Detect conflicts in a grammar.

`data`
  - Spanish, English, Hungarian and Finnish CG rules and morpho analysed data to use as examples.

`doc`
  - Papers on this project: 2015 CG workshop at NoDaLiDa, and 2016 LREC. (See [cgexp](https://github.com/inariksit/cgexp) for another experiment, hopefully becoming a paper at 2017 CG workshop.)
  - (cg.bib)[https://github.com/inariksit/cgsat/blob/master/doc/cg.bib] for all things CG-related.

`graveyard`
  - All the various versions between December 2014 - December 2016. Not in any particular order, definitely doesn't compile.

`src`
  - CG_SAT: Takes the CG format in [cghs](https://github.com/inariksit/cghs), turns it into a SAT-problem. TODO rewrite this to work with the new format.
  - AmbiguityClass: Extra feature of ambiguity classes. Needs a representative lexicon in order to work properly. Written by [Koen Claessen](https://github.com/koengit/), to be modified to my new format.

`test`
  - Spec: TODO write some tests. Tests for the format and parser are in [cghs](https://github.com/inariksit/cghs).
        


