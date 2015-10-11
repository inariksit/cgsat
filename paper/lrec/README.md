# Grammar analysis using SAT solver

Given a set of rules and a morphological lexicon (using the same tag set as your rules), find out all kinds of fun things about your grammar! ^_^

-----

## TODO

Plenty of stuff still not working properly

### *, barrier and LINK n

When creating a symbolic sentence, `1*` and `1` will only create one word to the right. Similarly, `1* ... BARRIER ...` will create just two more words, for minimal example.

`LINK n` needs to work too. So for example `REMOVE a IF -1 * b BARRIER c LINK 1 d LINK 1 e` currently normalises to this `REMOVE a IF (1 e) (0 d) (*-1  b BARRIER c)`.

### Proper (less ad hoc) shrinking of conflicting rules

* Binary search-ish ?
* Are there tricky cases? If we have rules `[r1...r100]` and rules 29 and 70 conflict, how do we start? Split at 1-50 and 51-100, if both halves can be run without conflict, then start adding 1-51, 1-52 etc. until 1-70 conflicts, then start removing from the beginning, until 30-70 doesn't conflict.

### Word-internal constraints

Ok the boundary thing looks like it works now. Then just add everything else \:D/