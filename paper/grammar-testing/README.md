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
l  = REMOVE v IF (-1 det)

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

Try to apply `REMOVE v IF (-1C det)` s.t. result will still trigger `l`
* remove `v` not possible
* only `v` left not possible
* only chance is to make such conditions that trigger `l` but not `r1`:

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
l  = REMOVE v IF (-1 det)

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

Try to apply `REMOVE det IF (1 v)` s.t. will trigger `l`
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
l  = REMOVE v IF (-1 det)

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

Try to apply `REMOVE adj IF (1 v)` s.t. will trigger `l`
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

## Real life scenarios

### Found a real conflict 

```
Conflict!
Cannot trigger the last rule: REMOVE adv IF (-1 det) (0 adj) (1 n|np)
with the previous rules:
 REMOVE:r_pr_v pr IF (1 vblex|vbmod|vaux|vbhaver|vbser) (NOT 0 "te"|"om te")
 REMOVE:r_adv_n adv IF (1 n|np)
 SELECT:s_pr_v pr + "te"|"om te" IF (1 vblex|vbmod|vaux|vbhaver|vbser + inf)
 ...

The sentence should have these properties:
* must have: [w2<adv>,w2<adv><itg>,w2<rel><adv>]
* must have: [w2<"<EOS>"><<<<><sent>,w2<>>>>,w2<prn><pers><p2>]...
* must hold: (-1 det) in 1 & (0 adj) in 2 & (1 n|np) in 3
```

Then it tries combinations of 2 requirements:

```
solveAndPrintSentence: Conflict with assumptions 
* must have: [w2<adv>,w2<adv><itg>,w2<rel><adv>]
* must hold: (-1 det) in 1 & (0 adj) in 2 & (1 n|np) in 3
```

Problem is combining targe with condition.
In order to find the previous rule that conflicts, start by finding rules that have the same target.




### Found a silly set definition

```
The sentence should have these properties:
* ~must have: 
  [ w2<det>
  , w2<n><np><adj><det><preadv><adv><vblex><vbmod><vbhaver><vbser><prn><pr><cnjcoo><cnjsub><cnjadv><rel><ij>
  , ...]
```

That w2 is such a multi-purpose word! ^_^

-----

## Nice examples where the new method works

Look at the following three rules.

```
r1 = REMOVE V   IF (-1C Det) ;
r2 = REMOVE Det IF ( 1  V)   ;
r3 = REMOVE V   IF (-1  Det) ;
```

Is there an input which can go through the rules and trigger at the last?

`r1` creates a new variable `w2'<v>` for the hypothesis that `w2` is `v`.
This variable is true, if `w2<v>` is true and there is a reason why we cannot apply `r1`.
These reasons are 
  * `w1` is not unambiguously `det` 
  * `w2` has only analyses with `v` left.

In other words, the value of the new variable is determined by the formula

```
  w2'<v> ⇐ w2<v> ∧ ( ¬w1<det_unambiguously> ∨ w2'<only_v> )
```

Solving at this point isn't particularly exciting; we could just get any solution, including multiple ones where `w1` and `w2` don't include determiners or verbs at all.

The value of `wN''<tag>` depends on `wN'<tag>`, which in turn depends on `wN<tag>`. This means that a rule which requires `wN''<tag>` to be true, will affect also `wN'<tag>` and `wN<tag>`. If  a d the value of a variable at any stage can be influenced by new rules.

only now add the example about the difference of r2 and s2!
motivate: value required by condition vs. value required by target

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

Now contrast with a rule sequence where the second rule actually performs an action to its target. As before, but `r2` is changed for `s2`.

```
REMOVE:r1 V IF (-1C Det)
SELECT:s2 Det IF (1 V)
REMOVE:r3 V IF (-1 Det)
```

Now it is possible to construe something that will get past the first rule *and* will trigger the 3rd rule:

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

The rule `s2` should be fine after `r1`. 


## Heuristics to search for conflicting rules

After applying all rules to the symbolic sentence, we try to solve with the following requirements:

1. Target of removal is in `trgSInd`
2. Something else is in `trgSInd`
3. All conditions hold

If we don't find a solution with this, we try to pinpoint where the problem is. For instance, if we succeed with 2+3 but not with 1+2 and 1+3, we can assume that some earlier rule is targeting our target.

If 1+2 is fine but _+3 conflicts, we start looking at the conditions. Three easy to detect reasons:

### Conditions within one rule are incompatible with each other

Real life example:

```
SELECT:subj4 PRS (*-1 ("ser") LINK 1 A LINK 1 ("que")) (0C Verb) ;
```

Just look a bit closer, how LINK 1 normalises to following:
```
SELECT:subj4 Prs IF (1 "que") (0 Adj) (*-1 "ser") (0C Verb) ;
```

and here we have a conflict: `IF (0 Adj)` and `IF (0C Verb)`.

Detect by creating a fresh sentence with new solver and just try to solve with both requirements.


### Tag combination is not predefined

Default option is to exctract all tag lists and sets defined somewhere in the grammar.
For stricter behaviour, we have option `strict-tags`, which only accepts predefined LIST and SET definitions. Given the following definitions:

```
LIST Det = (det def) (det def) (rel aa) ;
LIST Masc = m mf ;
SET DetMasc = Det + Masc ;
```

the rule `REMOVE ... IF (1 DetMasc)` is fine, but rules `REMOVE ... IF (1 (det def) OR (rel aa))` or even `REMOVE ... IF (1 Det + Masc)` are not.

### Other rule targets the condition of `l`

Suppose `l` is `REMOVE ... IF (1 Adj)`. The symbolic sentence is 2 words long and `w2` must be an adjective. We check all other rules and return those whose length is 2 or less, and target an adjective in the second word.


