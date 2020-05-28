---
title: Property-based Testing
date: 2020-05-28
headerImg: books.jpg
---


## Property-based Testing

Lets look at [QuickCheck][1]

        Typeclasses + Monads + "cleverness" = Automatic Testing

- Developed by [Koen Claessen][0] and [John Hughes][11] in 2000 

Ported to [40 other languages](https://en.wikipedia.org/wiki/QuickCheck) 

- [JSVerify](http://jsverify.github.io/), 
  [JUNIT-quickcheck](https://github.com/pholser/junit-quickcheck), 
  [hypothesis](https://github.com/HypothesisWorks/hypothesis)

PBT used in basically all kinds of software...

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Plan

1. **Property-based Testing**

2. Random Test Generation

3. Case-Study

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Property-based Testing

**Don't** (only) write individual *unit-tests* 

- only check particular input-output behavior of code

**Do write** *properties* desired of the functions

- *generate* random inputs 
- *run* function 
- *verify* (or rather, try to falsify) the property

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Testing with Specifications 

PBT emphasizes the importance of **specifying correct behavior** 

- Makes you think about what the code *should do*,

- Finds *corner-cases* where the specification is violated (so code or spec are fixed)

- Makes specs live on as machine-checked *documentation*.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>



## Properties

A `Property` is a function that returns a `Bool`

```haskell
prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = 
    reverse (xs ++ ys) == reverse xs ++ reverse ys
```

Looks like a _theorem_ that the programmer _believes_ is true. 

- By convention, we write the prefix `"prop_"`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## QUIZ

Consider the property `prop_revapp` 

```haskell
prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = 
    reverse (xs ++ ys) == reverse xs ++ reverse ys
```

It says *forall* `xs`, `ys`.  `reverse (xs ++ ys) == reverse xs ++ reverse ys`.

Is the property true?

**A.** Yes

**B.** No

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Question

Why specify type as `[Int] -> [Int] -> Bool`? 

* Why not write `[Int] -> [Int] -> Bool`? 


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Checking Properties

QC uses the types to generate *random inputs* 

- For that, it needs a *specific* type like `[Int]` 

- Cannot generate an input of type `[a]` 

Let's `quickCheck` it!

```haskell
>>> quickCheck prop_revapp
*** Failed! Falsifiable (after 6 tests and 9 shrinks):
[0]
[1]
```

Huh? QC gives us a _counterexample_ with `xs = [0]` and `ys == [1]`

```haskell
reverse (xs ++ ys) 
==> reverse ([0] ++ [1]) 
==> reverse ([0, 1]) 
==> [1, 0])
```

but 

```haskell
reverse xs ++ reverse ys
==> reverse [0] ++ reverse [1]
==> [0] ++ [1]
==> [0, 1]
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## EXERCISE 

Can you modify `prop_revapp` so that it is _sensible_  and _true_ ?

```haskell
prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs ++ ys) == ???
```

When you are done, we should see

```haskell
>>> quickCheck prop_revapp'
+++ OK, passed 100 tests.
```

We can run *more* tests by specifying that as a parameter

```haskell
quickCheckN n = quickCheckWith (stdArgs {maxSuccess = n})
```

Followed by

```haskell
quickCheckN 10000 prop_revapp
+++ OK, passed 10000 tests
```


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## QuickSort

Here's a simple sorting function (`quickSort`)

```haskell
qsort        :: (Ord a) => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort ls ++ [x] ++ qsort rs
  where 
    ls       = [y | y <- xs, y < x]  -- elems in xs < x 
    rs       = [z | z <- xs, z > x]  -- elems in xs > x
```

Seems to work?

```haskell
>>> [1,3..19] ++ [2,4..20]
[1,3,5,7,9,11,13,15,17,19,2,4,6,8,10,12,14,16,18,20]

>>> qsort ([1,3..19] ++ [2,4..20])
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## QuickCheck QuickSort

Lets *check* the output of `qsort` is **ordered**

```haskell
isOrdered :: (Ord a) => [a] -> Bool
isOrdered ::         (Ord a) => [a] -> Bool
isOrdered (x1:x2:xs) = x1 <= x2 && isOrdered (x2:xs)
isOrdered _          = True
```

and use it to write a property

```haskell
prop_qsort_isOrdered :: [Int] -> Bool
prop_qsort_isOrdered xs = isOrdered (qsort xs)
```

which we can check

```haskell
>>> quickCheckN 1000 prop_qsort_isOrdered
+++ OK, passed 1000 tests.
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## QUIZ 

Lets check that the *first* element of the output is the smallest

```haskell
qsort        :: (Ord a) => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort ls ++ [x] ++ qsort rs
  where 
    ls       = [y | y <- xs, y < x]  -- elems in xs < x 
    rs       = [z | z <- xs, z > x]  -- elems in xs > x

prop_qsort_min :: [a] -> Bool
prop_qsort_min xs = head (qsort xs) == minimum xs
```

What is the result of

```haskell
>>> quickCheck prop_qsort_min
```

**A.** Pass 100 tests

**B.** Fail

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Properties and Assumptions

```haskell
prop_qsort_min :: [a] -> Bool
prop_qsort_min xs = head (qsort xs) == minimum x
```

Oops!

```haskell
>>> quickCheck prop_qsort_min
*** Failed! Exception: 'Prelude.head: empty list' (after 1 test):
[]
```

`prop_qsort_min` is not true **for all** `Int` lists

- Property *only* makes sense if `xs` is not empty

Writing specifications clarifies the *assumptions* 

- under which a given piece of code is supposed to work. 

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Conditional Properties 

Lets modify `prop_qsort_min` so equality holds *if* input is non-null

```haskell
prop_qsort_nn_min    :: [Int] -> Property
prop_qsort_nn_min xs =
  not (null xs) ==> head (qsort xs) == minimum xs
```

Instead of `Bool` the function's output is `Property` 

- a special type built into the QC library

The *implies* operator `==>` is one of many 

- that allow the construction of rich properties.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## QUIZ 

Lets test that our `qsort` is *identical* to a *trusted reference implementation* 

- may be too *slow* to deploy but ok to use for checking correctness

Lets use the standard library's `Data.List.sort` function

- (_Much_ faster than ours... but just for illustration!)

```haskell
prop_qsort_sort    :: [Int] -> Bool
prop_qsort_sort xs =  qsort xs == sort xs
```

What is the result of

```haskell
qsort        :: (Ord a) => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort ls ++ [x] ++ qsort rs
  where 
    ls       = [y | y <- xs, y < x]  -- elems in xs < x 
    rs       = [z | z <- xs, z > x]  -- elems in xs > x

>>> quickCheck prop_qsort_sort
```

**A.** `OK` after 100 tests

**B.** `Failed! Falsifiable...`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Lets Check 

```haskell
>>> quickCheck prop_qsort_sort
*** Failed! Falsifiable (after 6 tests and 3 shrinks):
[-3,-3]
```

Oops? What?

```haskell
>>> sort [-3, -3]
[-3, -3]

>>> qsort [-3, -3]
[-3]
```

Ugh! So close, and yet ... Can you spot the bug in our code?

```haskell
qsort        :: (Ord a) => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort ls ++ [x] ++ qsort rs
  where 
    ls       = [y | y <- xs, y < x]  -- elems in xs < x 
    rs       = [z | z <- xs, z > x]  -- elems in xs > x
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Specifying No-Duplicates

We assumed that the input has **no duplicates**

- **Values equal to** `x` are thrown out from `ls` and `rs`

Is this a *bug*? Maybe? Maybe not?

- But at least its something we should be *aware* of!

Lets specify that a list has no-duplicates 

```haskell
noDuplicates ::(Eq a) => [a] -> Bool
noDuplicates (x:xs) = not (x `elem` xs) && noDuplicates xs
noDuplicates _      = True
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Specifying a Post-Condition

We can now check that `qsort` **outputs** a list with no-duplicates

```haskell
prop_qsort_distinct :: [Int] -> Bool 
prop_qsort_distinct xs = noDuplicates (qsort xs)  

-- >>> quickCheck prop_qsort_distinct
-- +++ OK, passed 100 tests.
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Specifying a Pre-Condition

Also, `qsort` is identical to `sort` on **inputs with no duplicates**

```haskell
prop_qsort_distinct_sort :: [Int] -> Property 
prop_qsort_distinct_sort xs = 
  (isDistinct xs) ==> (qsort xs == sort xs)

-- >>> quickCheck prop_qsort_distinct_sort
-- +++ OK, passed 100 tests.
--
```

## Plan

1. **Property-based Testing**
    - Properties are boolean-functions
    - Generate inputs, run function, check if result is `False`

2. Random Test Generation

3. Case-Study

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Plan

1. Property-based Testing
    - Properties are boolean-functions
    - Generate inputs, run function, check if result is `False`

2. **Test Generation**

3. Case-Study


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Test Generation

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Case Study: Compiler Optimizations

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>



[0]: http://www.cse.chalmers.se/~koen/
[1]: http://www.cse.chalmers.se/~rjmh/QuickCheck/
[2]: http://www.cs.york.ac.uk/fp/smallcheck/
[3]: http://video.google.com/videoplay?docid=4655369445141008672#
[4]: http://www.erlang-factory.com/upload/presentations/55/TestingErlangProgrammesforMulticore.pdf
[5]: http://en.wikipedia.org/wiki/Insertion_sort
[6]: http://hackage.haskell.org/packages/archive/QuickCheck/latest/doc/html/src/Test-QuickCheck-Gen.html#Gen
[7]: http://book.realworldhaskell.org/read/monads.html
[8]: http://book.realworldhaskell.org/read/testing-and-quality-assurance.html
[9]: http://www.haskell.org/haskellwiki/QuickCheck_as_a_test_set_generator
[10]: http://community.moertel.com/~thor/talks/pgh-pm-talk-lectrotest.pdf
[11]: http://www.cse.chalmers.se/~rjmh
