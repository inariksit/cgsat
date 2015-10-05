# Grammar analysis using SAT solver

## Definition of conflict

```
    r1  
     .
G =  .
     .
    r^n
```

We have a grammar `G` which consists of rules `r1`--`r^n`, executed in that order.
We take all subsequences `r1-r^i` of G and ask the question

> Is there a sentence which can go through rules `r1-r^i-1` and trigger rule `r^i`?

Let us illustrate this with a concrete example.

```
    REMOVE b IF (-1 b) ;  
    REMOVE d IF ( 1 c) ;
G =  .
     .
    REMOVE a IF (-1 c) ;
```

The last rule (`l`) is `REMOVE a IF (-1 c)`, and we want to find if there is a sentence that triggers `l` *after having the previous rules applied to it*. It needs the following properties:

 * The sentence has to be at least 2 words long.
 * The target must have at least one reading with the tag `a`, and at least one reading without, so that `a` can be removed and the word still has readings.
 * The word before the target must have at least one reading with the tag `c`.

If there is a rule `rc` that makes `l` impossible to apply, no matter the input, then we say that rules `rc` and `l` conflict. Some examples for conflicts:


```
    REMOVE b IF (-1 b) ;  
    REMOVE c ;
G =  .
     .
    REMOVE a IF (-1 c) ;
```

Here the second rule prevents `l`'s condition from being true, so `l` can never be applied.


```
    REMOVE a IF (-1 b) ;  
    REMOVE d ;
G = REMOVE a IF (NOT -1 b) ;
     .
    REMOVE a IF (-1 c) ;
```

Here the first and third rules together remove `a` in all possible cases, leaving no `a` for the last rule to remove.

 1. conditions are out of scope (trivial, no clauses)
 2. conditions in scope, but one or more doesn't hold
 3. tag has been removed in target
 4. all readings of target have the desired tag (cannot remove)

## Examples


### Conditions out of scope

### Conditions don't hold

```
r1 = REMOVE v IF (-1C det)
rl = REMOVE v IF (-1 det)

one possible symbolic sentence:

w2<v>
or [w2<det>,w2<n>,...]
or [w1<det>,w1<det><def>]


"<w1>"
        det def
"<w2>"
        det
        v
```

Try to apply `REMOVE v IF (-1C det)` s.t. result will still trigger `rl`
* remove `v` not possible
* only `v` left not possible
* only chance is to make such conditions that trigger `rl` but not `r1`:

```
"<w1>"
        n
        det def
"<w2>"
        det
        v
```

### Only target left

```
r1 = REMOVE det IF (1 v)
rl = REMOVE v IF (-1 det)

one possible symbolic sentence:
w2<v>
or [w2<det>,w2<n>,...]

or [w1<det>,w1<det><def>]

"<w1>"
        det def
        pron def
"<w2>"
        n
        v
```

Try to apply `REMOVE det IF (1 v)` s.t. will trigger `rl`
* can't change conditions: `w2` must have a `v` in order to remove it
* remove `det` not possible: `w2` must have a `det`
* only `det` left` works:

```
or [w1<det>, w1<det><def>]
and [~w1<n>, ~w1<v>, ...]

"<w1>"
        det def
"<w2>"
        v
        n
```

### Remove target

```
r1 = REMOVE adj IF (1 v)
rl = REMOVE v IF (-1 det)

one possible symbolic sentence:
w2<v>
or [w2<det>,w2<n>,...]

or [w1<det>,w1<det><def>]

"<w1>"
        det def
	adv
	adj
"<w2>"
        n
        v
```

Try to apply `REMOVE adj IF (1 v)` s.t. will trigger `rl`
* can't change conditions: `w2` must have a `v` in order to remove it
* only `adj` left not possible: `w2` must have a `det`
* remove `adj`

```
or [w1<det>, w1<det><def>]
and [~w1<adj><attr>, ~w1<adj><pred>, ...]

"<w1>"
        det def
	adv
"<w2>"
        v
        n
```


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

But if we apply it to SELECT det instead, we have a problem:

## Problem:

Rule sequence

```
REMOVE:r1 v IF (-1C det)
SELECT:s2 det IF (1 v)
REMOVE:r3 v IF (-1 det)
```

Something that will get past the first rule *and* will trigger the 3rd rule:

```
"<w1>"
	det def
	foo
"<w2>"
	v
	n
```

`v` is not removed, because `w1` is not unambiguously `det`.

Something that will get past the second rule and will trigger the 3rd rule:

```
"<w1>"
	det def
"<w2>"
	v
	n
```

`w2` must be `v` in order to trigger `r3`.

The rule `s2` should be fine after `r1`. But when we input all clauses to the SAT solver, they conflict.

Solutions?

* Add clause to solver only if it removes something?
* Add only results of each solving to solver? ie. not the clause, but [~w1<foo>] as a clause.


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



