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


Implementation of Careful mode
------------------------------

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
r =: new literal

    1  tag: r => v6
    1C tag: r => v6 ; r => ~v7
NOT 1  tag: r => v7 ; r => ~v6
NOT 1C tag: r => ~v6
```
