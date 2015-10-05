# Random ramblings and documentation of older/failed attempts

This document is for historical purposes. Methods described on this page might be working but I don't use it in SAT-CG, or it might be completely wrong and I don't use it in SAT-CG. Anyway if you want a description of what I use in SAT-CG, don't look for it here.

-------

## Low-level way of creating one variable per context

Just putting this here, switched to using SAT+ high-level functions in the code

```
analyses:
  v6 ... (7,["<la>","el",det,def,f,sg])
  v7 ... (7,["<la>","lo",prn,pro,p3,f,sg])
  v8 ... (8,["<casa>","casa",n,f,sg])
  v9 ... (8,["<casa>","casar",vblex,pri,p3,sg])
  v10 .. (8,["<casa>","casar",vblex,imp,p2,sg])
  v11 .. (9,["<,>", ",", cm])
rule: 
  SELECT:i08_s_pro PrnIndep IF (1 VerbFin)

both v9 and v10 are VerbFin, so we create these clauses:
  [False,~v9,v15]
  [False,~v10,v15]

the final lit to be used in the rule is v15:
  [~v15,~v6], [~v15,v7]

the old way would be 
  [~v9,~v6], [~v9,v7], [~v10,~v6], [~v10,v7]

Example with two conditions:

rule:
  SELECT:i02_s_n_adj n  IF (-1C Det) (NOT 1 N)

v11 is not N and v6 is a det, so we create these clauses:
 [False,~v11,v16] -- first round:   dummy && NOT 1 N => v16
 [~v16,~v6,v17]   -- second round:  v16   && -1 Det  => v17 

the final lit is v17:
 [~v17,v8], [~v17,~v9], [~v17,~v10]
``` 


## Distinguish if a reading is removed because it's the target of a rule, or a condition

This is a step towards the algorithm we actually use in Symbolic.

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


## Implementation of Careful mode




Let us look at the following rules and input sentence.

```
REMOVE (target2) IF (1 (tag)) ;
REMOVE (target1) IF (1C (tag)) ;
REMOVE (target4) IF (NOT 1 (tag)) ;
REMOVE (target3) IF (NOT 1C (tag)) ;


"<target>"
	"trg" target1        --v1
	"trg" target2        --v2
	"trg" target3        --v3
	"trg" target4        --v4
	"trg" dummy          --v5

"<cond>"
	"cond" tag           --v6
	"cond" dummy         --v7

```

Applying the `tagsMatchRule` function to `target[1-4]` will return, respectively, the following contexts to the target literals `v[1-4]`:

```
(v1, [v6])
(v2, [v6]) --tagsMatchRule doesn't care about C
(v3, [v7]) --not . tagsMatchRule 
(v4, [v7])
```

Now when we make the context variable, which to use in all clauses that have the same context. Even if `tagsMatchRule` returns the same context for (NOT) 1 and (NOT) 1C, they should have different contexts. 

```
r[1-4] := new literal

    1  tag: v6        => r1
    1C tag: v6 && ~v7 => r2
NOT 1  tag: v7 && ~v6 => r3
NOT 1C tag: ~v6       => r4
```

Then we make the following clauses:

```
r1 => ~v1
r2 => ~v2
r3 => ~v3
r4 => ~v4
```

### Problem

First try, just what I described above.
I got results like following:

```
Used clauses:
REMOVE target1  IF (1C tag )
* [~v9,~v1]
REMOVE dummy 
* [~v11,~v5]
REMOVE dummy 
* [~v11,~v7]
REMOVE target3  IF (1C tag )
* [~v13,~v3]

Unused clauses:

The following tag sequence was chosen:

"<target>"
	;  "trg" target1     --v1
	 "trg" target2       
	;  "trg" target3     --v3
	 "trg" target4       
	;  "trg" dummy       --v5
"<cond>"
	 "cond" tag          
	;  "cond" dummy      --v7
"<EOS>"
	 <<< sent


helper lits after solving:
v9  v11 v13 v15 v16 v17 v18 
0   0   0   1   1   1   1  
```

The conditions for all of the REMOVE-clauses were False (v9-v13), and applyRule clauses are true (v15-v18), thus the tags were still removed.

Second try: maximise the conditions every time when applying clauses produced by one rule.

Getting the same results, but now with the following values for the conditions:

```
helper lits after solving:
v9  v11 v13 v15 v16 v17 v18 
1   1   0   1   1   1   1
```

The order is really backwards: it should be v13 that is True, because the rule `REMOVE target3 IF (1C tag)` is applied after `REMOVE dummy`. And in any case, `target3` is removed even if the condition doesn't hold.

Third try: make implications as equivalences

Now it doesn't remove `target3` when `v13` is false. Still not good, because order is messed up.

I tried to print out all possible intermediate literals when making conditions. For instance, this is when I make the requirement for `(1C tag)`. `v6` is `tag` and `v7` is `dummy`, so v6 should be true and v7 false:

```
matchlits: [v6]
nomatchlits: [v7]
[False,~v6,v10]
[~v10,v7,v9]
```

When I was printing intermediate variables' values, `v10` could be true and `v9` false. Make equivalences there too?


## A couple of words about BARRIERs and indices

Base case: `REMOVE foo IF (1 bar)`, will match if there is

```
"<target>"
        foo
	notfoo
"<cond>"
	bar
	anything
```

With negation: `REMOVE foo IF (NOT 1 bar)`, will match both

```
"<target>"
        foo
	notfoo
"<cond>"
	notbar
```

and 

```
"<target>"
        foo
	notfoo
<EOS>
```

Let's look at these rules.
```
REMOVE:oob_not target  IF (NOT *10  tag  BARRIER barrier )
REMOVE:c_oob_not ctarget  IF (NOT *10  tag  CBARRIER barrier )
```

First of all, the sentences are not 10 words long, so we will not find a barrier after the 10th word.
The helper functions cbarrier and barrier actually work for this too. The case where `cbarinds==[]` triggers that we call `atleast n token`, and now I made a change, that if we're on negative cond *and* index is out of bounds, return `dummyTok`. Then we filter with match, and again, the negative mode makes it so that it wants something that does *not* match, which `dummyTok` does. So actually it worked pretty ok.

Except that the condition making is all fucked up now, and the clauses don't end up right. ;___;

---

