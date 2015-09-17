Task
----

We create an initial symbolic sentence `w` that would make the "last" rule fire. Then we want to make `w` so that none of the earlier rules has effect on it, because

 1. conditions are out of scope (trivial, no clauses)
 2. conditions in scope, but one or more doesn't hold
 3. tag has been removed in target
 4. all readings of target have the desired tag (cannot remove)
 

Examples
--------

Last rule:
```
   REMOVE inf  IF (-1 (prn pers))
```

Earlier rules:
```
   REMOVE:r_pr_v pr  IF (1 vblex vbmod vaux vbhaver vbser ) (NOT 0 "te" "om te" )
   REMOVE:r_adv_n adv  IF (1 n np )
   SELECT:s_pr_v pr  + "te" "om te"  IF (1 vblex vbmod vaux vbhaver vbser  + inf )
```

Examined rule creates a model with many solutions, among which the following:
```
  "<w1>"
        prn pers
  "<w2>"
        <<<
        vblex inf
```

Then we apply the earlier rules to our symbolic string. First two are fine. They don't affect the analyses that are crucial to our examined rule.
However, the third SELECT rule has `vblex inf` in its context. When we add this clause to SAT solver, it will try to find another model which satisfies the examined rule, but will make the SELECT rule to have no effect. We find one, which satisfies scenario 2: condition is not met.

```
  "<w1>"
        prn pers
  "<w2>"
        <<<
        inf
```

---------


An example of scenario 4:

Last rule:

```
  SELECT:s_pr_v pr  + "te" "om te"  IF (1 vblex vbmod vaux vbhaver vbser  + inf ) 
```

Earlier rules:
```
   REMOVE:r_pr_v pr  IF (1 vblex vbmod vaux vbhaver vbser ) (NOT 0 "te" "om te" )
   REMOVE:r_adv_n adv  IF (1 n np )
```

One solution for the initial constraints:

```
  "<w1>"
        >>>
        pr "te"
  "<w2>"
        vblex inf
```

First REMOVE rule conflicts. Solve by applying scenario 4: only targets are left.
Scenario 3 wouldn't work because `(pr "te")` is a requirement.
Scenario 2 wouldn't work because ???

Solution after all clauses:
```
"<w1>"
        pr "te"
        pr "om te"
"<w2>"
        vblex inf
```


## Interaction of previous rules

Look at the following three rules.

```
r1 = REMOVE V   IF (-1C Det) ;
r2 = REMOVE Det IF ( 1  V)   ;
r3 = REMOVE V   IF (-1  Det) ;
```

Is there an input which can go through the rules and trigger at the last?

```
"<w1>"
        det
        pron
"<w2>"
        v
        n
```

 * r1: Doesn't trigger because condition `(-1C Det)` isn't met.
 * r2: Cannot trigger, because r3 requires `w2` to have a `v` analysis.
   * Tries to go for "ok what if the target (`w1`) *only* has a `det` analysis!" 
   * In that case, the input would already trigger `r1`
   * Cannot do neither => conflict



~~~~


### Experimental new stuff, sequential, not adding clauses but just the results:

```
REMOVE:r1 adj|det IF (1 v): 
(w1<det>,~only_"<w1>"adj|det_left_XOR_rm_w1<det>_if_(w2<v>)),
(w1<adj><pred>,~only_"<w1>"adj|det_left_XOR_rm_w1<adj><pred>_if_(w2<v>))
(w1<adj><attr>,~only_"<w1>"adj|det_left_XOR_rm_w1<adj><attr>_if_(w2<v>))
(w1<det><def>,~only_"<w1>"adj|det_left_XOR_rm_w1<det><def>_if_(w2<v>))
w1<det>=True
w1<adj><pred>=True
w1<adj><attr>=False
w1<det><def>=False
~only_"<w1>"adj|det_left_XOR_rm_w1<det>_if_(w2<v>)=True
~only_"<w1>"adj|det_left_XOR_rm_w1<adj><pred>_if_(w2<v>)=True
~only_"<w1>"adj|det_left_XOR_rm_w1<adj><attr>_if_(w2<v>)=True
~only_"<w1>"adj|det_left_XOR_rm_w1<det><def>_if_(w2<v>)=True
([],[~w1<adj><attr>,~w1<det><def>])
     ^---- Seems like these are negative just at random D:
           But it still fits the rules "only w1 adj|det left", it does have both
	   But it's still bad to have such arbitrary stuff propagated early ...
           maybe maximise?
----
"<w1>"
	det
	adj pred
"<w2>"
	det
	v
```

Or what if we do like
...if rule has no effect, then we don't add it