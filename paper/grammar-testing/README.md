Task
----

We create an initial symbolic sentence `w` that would make the "last" rule fire. Then we want to make `w` so that none of the earlier rules has effect on it, because

 1. conditions are out of scope (trivial, no clauses)
 2. conditions in scope, but one or more doesn't hold
 3. tag has been removed in target
 4. all readings of target have the desired tag (cannot remove)
 

## New algorithm


Motivating example.

```
r1 = REMOVE a IF (-1 (*) - b) ;
r2 = REMOVE b IF (1 a) ;
r3 = REMOVE a IF (-1 b) ;

l  = REMOVE a IF (-1 c) ;
```

* Sequence `r1-r2-r3-l` should be fine: we can make condition for `r1` not hold by requiring that `b`. Then `r2` will remove that `b`, and prevent `r3` from firing.
* Sequence `r1-r3-l` should conflict: the conditions of `r1` and `r3` are `~b` and `b`, which makes r1 and r3 effectively `REMOVE a IF (-1 (*))`.
* Sequence `r1-r2-l` should be fine.
* Sequence `r3-r2-r1-l` should conflict, unless our CG allows `ADD` operation.

With the old scheme (construct all clauses & apply at the same time), the sequence `r1-r2-l` would conflict. The only way to pass `r1` is to make `(-1 (*) - b)` false, i.e. `w1<b>` true. Then `r2` is going to remove the `w1<b>` reading (again the only choice, because `r2`'s condition cannot be false). This will still leave `w1<c>`, `w2<a>` and `w2<c>` true, enough to trigger `l`.

We need to distinguish if a reading is removed by a rule or by the need to match a condition. Also, if a condition requires a reading to be true, that should not be permanent: it can be overridden by a rule (`r1-r2`), but not by another condition (`r1-r3`).

My idea: introduce two sets of literals for readings, `wⁿ<tag>_TRG` and  `wⁿ<tag>_COND`. Like this:

```
[[w1<a>_T, w1<b>_T, w1<c>_T],    [[w1<a>_C, w1<b>_C, w1<c>_C],    
 [w2<a>_T, w2<b>_T, w2<c>_T]]     [w2<a>_C, w2<b>_C, w2<c>_C]] 
```

And the relation between the literals is

```
 trg  => cond
~cond => ~trg
```

i.e. if a literal is true at the target layer, it leaks to the condition layer. If something gets negative at the condition layer, it leaks to the target layer. But if something is positive at the condition layer, it doesn't mean yet anything at the target layer. In other words, `(zipWith $ zipWith (\t c -> [neg t, c])) targets conditions`.

The initial symbolic sentence is made at the target layer:

```
l  = REMOVE a IF (-1 c) ;

[w1<c>_T]
[w2<a>_T]
[w2<b>_T, w2<c>_T]
```

Also the type `Token` requires a change: instead of `getLit`, there will be `getTrgLit` and `getCondLit`. Each token is initialised with distinct trg and cond lits.
The tokens are updated sometimes, so we cannot form all clauses at once. 

We apply the first rule: `r1 = REMOVE a IF (-1 (*) - b)`.

For the condition, we access the condition layer of `w1` and the target layer of `w2`:

```
getContext w2<a>
          [w1<a>, w1<b>, w1<c>, w2<a>, w2<b>, w2<c>]
  	  (-1 (*) - b) 
  = [([w1<a>,w1<c>],    --match
      [w1<b>      ])]   --diff

-- now we have our context, with match and diff, time to form clauses

w1_a_c  <- orl s [getCondLit w1<a>, --w1<a>_C
	      	  getCondLit w1<c>] --w1<c>_C
w1_not_b <- andl s [neg $ getCondLit w1<b>] --~w1<b>_C

w1_(*)_minus_b <- andl [w1_a_c, w1_not_b]

w2<a>_trg = getTrgLit w2<a>
only_w2<a>_left = ...
```

**Update `w2<a>_C` to `w2<a>_T`: now we have actually had a clause that has `w2<a>` as a target. 
We form the old clause with the cond layer of w2<a>, but after accessing those literals, we make the `getCondLit w2<a> == w2<a>_T` (`getTrgLit w2<a> == w2<a>_T` was already).
```
clause = [~w1_(*)_minus_b,  ~w2<a>_trg,  only_w2<a>_left]
```

Which actually means

```
clause = [w1<b>_C,  ~w2<a>_T,  only_w2<a>_left]
```


We can see already now that `~w2<a>_T` and `only_w2<a>_left` evaluate to False, so the first literal must be true. The negation of `w1_(*)_minus_b` will actually imply that `w1<b>_C` is True. But since this is at the condition layer, it doesn't imply anything at the target layer.

Apply `r2 = REMOVE b IF (1 a)`:

```
getContext w1<b>
          [w1<a>, w1<b>, w1<c>, w2<a>, w2<b>, w2<c>]
  	  (1 a) 
  = [([w2<a>],    --match
      [     ])]   --diff empty because condition doesn't has no diff nor is cautious


let w2<a>_cond = getCondLit w2<a> --now pointing to w2<a>_T

w1<b>_trg = getTrgLit w1<b>
only_w1<b>_left = ...
```

**Stateful update again! `getCondLit w1<b> == w1<b>_T`

```
clause = [~w2<a>_cond, ~w1<b>_trg, only_w1<b>_left]
       = [~w2<a>_T,    ~w1<b>_T,   only_w1<b>_left]
```

We see again that `~w2<a>_T` and `only_w1<b>_left` are false. `~w1<b>_T` must be true. Since we have done the state update, now whenever `w1<b>` is a condition literal, it points to `w1<b>_T` and its value is False.

Apply third rule.

```
r3 = REMOVE a IF (-1 b) ;

getContext w2<a>
          [w1<a>, w1<b>, w1<c>, w2<a>, w2<b>, w2<c>]
          (-1 b) 
  = [([w1<b>],   --match
      [     ])]  --diff empty because condition doesn't has no diff nor is cautious


let w1<b>_cond = getCondLit w1<b> --now pointing to w1<b>_T

w2<a>_trg = getTrgLit w2<a>
only_w2<a>_left = ...
```

**Doing the state update for w2<a>, though no difference, because `w2<a>` has already been a target.

```
clause = [~w1<b>_cond, ~w2<a>_trg, only_w2<a>_left]
          = ~w1<b>_T,   = ~w2<a>_T
```

All fine, `~w2<a>_trg` and `only_w2<a>_left` are obviously false again, and `~w1<b>_cond` (really `~w1<b>_T`) is false like it was in the last iteration.

Now we still have a sentence that triggers `l`, hurra!


## Conflicting version, r1-r3-l

We have applied `r1`, which has left us with the following changes:

* `getCondLit w2<a> == w2<a>_T`
* `getCondLit w1<b> == w1<b>_C == True` (the only way to go through `r1` and trigger `l`.)

Apply `r3 = REMOVE a IF (-1 b)`:

```
getContext w2<a>
          [w1<a>, w1<b>, w1<c>, w2<a>, w2<b>, w2<c>]
          (-1 b) 
  = [([w1<b>],   --match
      [     ])]  --diff empty because condition doesn't has no diff nor is cautious


let w1<b>_cond = getCondLit w1<b> --still pointing to w1<b>_C

w2<a>_trg = getTrgLit w2<a>
only_w2<a>_left = ...
```

**Doing the state update, though no difference, because `w2<a>` has already been a target.

```
clause = [~w1<b>_cond, ~w2<a>_trg, only_w2<a>_left]
           = ~w1<b>_C   = ~w2<a>_T
```

`~w2<a>_trg` and `only_w2<a>_left` still evaluate to False, so `~w1<b>_cond` is left as the only choice. But `r1` has left `w1<b>_C` as True, so this will conflict.

## Conflicting version, r3-r2-r1-l

Apply `r3 = REMOVE a IF (-1 b)`:

```
getContext w2<a>
          [w1<a>, w1<b>, w1<c>, w2<a>, w2<b>, w2<c>]
          (-1 b) 
  = [([w1<b>],   --match
      [     ])]  --diff empty because condition doesn't has no diff nor is cautious


let w1<b>_cond = getCondLit w1<b> -- pointing to w1<b>_C

w2<a>_trg = getTrgLit w2<a>
only_w2<a>_left = ...
```

**Doing the state update for `getCondLit w2<a> == w2<a>_T`, first time a target.

```
clause = [~w1<b>_cond, ~w2<a>_trg, only_w2<a>_left]
           = ~w1<b>_C  = ~w2<a>_T
```

`~w2<a>_trg` and `only_w2<a>_left` evaluate to False as always, thus `~w1<b>_cond` must hold. Now since `w1<b>_C` is False, it makes `w1<b>_T` False as well. This means that any future clause that assumes `w1<b>_T` to be True will cause a conflict.

----

#### Applying `r2` or not makes no difference. If we apply it, it will just make the update of `w1<b>_cond == w1<b>_T` explicit. But already the negation has made `~w1<b>_C` to imply `~w1<b>_T`.

Here you go anyway, just for completeness' sake. Apply `r2 = REMOVE b IF (1 a)`:


```
getContext w1<b>
          [w1<a>, w1<b>, w1<c>, w2<a>, w2<b>, w2<c>]
  	  (1 a) 
  = [([w2<a>],    --match
      [     ])]   --diff empty because condition doesn't has no diff nor is cautious


let w2<a>_cond = getCondLit w2<a> --pointing to w2<a>_T

w1<b>_trg = getTrgLit w1<b> --pointing to w1<b>_T
only_w1<b>_left = ...
```

(**Update for `getCondLit w1<b> == w1<b>_T`)

```
clause = [~w2<a>_cond, ~w1<b>_trg, only_w1<b>_left]
```

`~w2<a>_cond, only_w1<b>_left` false, `~w1<b>_trg` holds. Still going fine, we didn't remove `w2<a>` because `w1` wasn't a `b`, and we kinda "remove" `w1<b>` by having that `w1` never was a `b` in the first place.

----
### Back to the conflict!

Apply `r1 = REMOVE a IF (-1 (*) - b)`:

```
form literals, yada yada
w1_(*)_minus_b <- andl [w1<a|c>_cond, ~w1<b>_cond]

redundant update of w2<a>

clause = [~w1_(*)_minus_b,  ~w2<a>_trg,  only_w2<a>_left]
          = w1<b>_T          = ~w2<a>_T
```

Same old business, `~w2<a>_trg` and `only_w2<a>_left` are obviously false. But also `~w1_(*)_minus_b` cannot hold because `r1` has made `w1<b>_T` false by implication (or `r2` has made it false explicitly).


## Negative condition layer leaking into target layer

Let's change rules for this!
```
r1 = REMOVE a IF (-1 b) ; --condition must be true, can't remove a or have only a
r2 = REMOVE b IF (-1 b) ; --can remove b or condition not hold
l  = REMOVE a IF (-1 c) ;
```

Motivation: if `r1` forces `~w1<b>`

* r1 : `[~w1<b>_cond, ~w2<a>_trg]` (and `getCondLit w2<a>` updated)
* `~w2<a>_trg` is false. `~w1<b>_cond` must hold, binds `w1<b>_T` by implication
* r2 : `[~w1<b>_cond, ~w2<b>_trg]` (and `getCondLit w2<b>` updated)
 * With just this rule, both literals could be true. Can remove `w2<b>` or not.
 * `~w1<b>_cond` is the same lit in both clauses: this prevents scenario where r2 fired and condition held--it didn't hold last time, it cannot hold now.

Now let us add `r3 = SELECT b IF (-1 BOS)` and try to apply it:

```
[~w0<bos>_cond(==False), ~w1<b>_trg]
```

`r3` tries to select `w1<b>_T`, but `r1` has already made it false by implication. Will conflict.

## Positive condition not leaking into target layer, next rule will have to be consistent with the condition layer

```
r1 = REMOVE a IF (-1C c) ; --condition must be true, can't remove a or have only a
r2 = REMOVE b IF (-1C c) ; --can remove b or condition not hold

l =  REMOVE a IF (-1  c) ;
```

This is just to demonstrate that if `r1` has made a condition to have a definitive value, then applying `r2` cannot change it, because `~w1_unambiguously_c` will point to the same literal in both places.

With this, we want to prevent this sentence from passing:

```
"<w1>"
       c

"<w2>"
       a
       c
```


Examples
--------

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



