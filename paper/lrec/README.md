# Grammar analysis using SAT solver

Given a set of rules and a morphological lexicon (using the same tag set as your rules), find out all kinds of fun things about your grammar! ^_^

-----

## TODO

Plenty of stuff still not working properly

### *, barrier and LINK n

When creating a symbolic sentence, `1*` and `1` will only create one word to the right. Similarly, `1* ... BARRIER ...` will create just two more words, for minimal example.

`LINK n` needs to work too. So for example `REMOVE a IF -1 * b BARRIER c LINK 1 d LINK 1 e` currently normalises to this `REMOVE a IF (1 e) (0 d) (*-1  b BARRIER c)`.

### Strange behaviour with NOT 


```SELECT:n_a_eos_4 N IF (2C N) (1 CnjCoo) (0C Noun_Adj_PP - NP) (-1 Barrera_Inicial_SN) ;```

Works properly

```SELECT:n_a_eos_4 N IF (2C N) (1 CnjCoo) (0C Noun_Adj_PP LINK 0 NOT NP) (-1 Barrera_Inicial_SN) ;```

Reports conflict

Fix!

### Proper (less ad hoc) shrinking of conflicting rules

* Currently using binary search -- works if the conflict is just one rule, or two rules very close to each other
* Are there tricky cases? If we have rules `[r1...r100]` and rules 29 and 70 conflict, how do we start? Split at 1-50 and 51-100, if both halves can be run without conflict, then start adding 1-51, 1-52 etc. until 1-70 conflicts, then start removing from the beginning, until 30-70 doesn't conflict.

## Some remarks about creating a list of all readings

* Sentence boundary (BOS/EOS) is a word in the symbolic sentence
* `sent` is also a word in the symbolic sentence. The BOS/EOS (>>>/<<<) are magic tags by VISL CG-3, and I adopt the convention. When parsing, I split text into sentences by punctuation (yep not gonna work for A.C.R.O.N.Y.M.S.) and wrap it in BOS _ EOS.

For grammar testing, I generate a list of all readings from the Apertium lexicon. I do

`lt-expand apertium-<lang>.<lang>.dix | sed 's/:[<>]:/:/' | cut -f 2 -d :` to get rid of the word forms. This still has lemmas, so you can remove everything on the lines before the first `<`. Then just `sort -u`, and you have a list of readings, such as `<det><def><mf><sg>, <det><def><mfn><pl>`.

However, the grammar often contains tag combinations that have lemmas or word forms in them, such as `REMOVE ("haar") + Noun IF (1 Noun)`. We add these combinations to the list of readings as follows:


```
for i in `egrep -v "^ *#|^DELIMITERS|^SOFT-DELIMITERS" $LANG.rlx | egrep -o "\"[^\"]*\"" | tr -d '"'`; do egrep $i $LANG.expanded | sed -E 's/([^:]*):([^<]*)(<.*)/\3<\2/'' ; done | sort -u
```

If we want the word form, replace the sed with `sed -E 's/([^:]*):([^<]*)(<.*)/\3<\1/`. This is due to the way I'm parsing it: `<` is the split character, and then I check the end, if there's a `>`, then it's a normal tag, otherwise lemma. (I didn't put in special case for word form, but that is easy to add.)

I also add >>> and <<< into the readings.