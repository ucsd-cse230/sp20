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

Lets notice something about `quickCheck`

If you run it once ...

```haskell
>>> quickCheck prop_qsort_sort
*** Failed! Falsifiable (after 6 tests and 2 shrinks):
[5,5]
```

and if you run it again ...

```haskell
>>> quickCheck prop_qsort_sort
*** Failed! Falsifiable (after 4 tests and 1 shrink):
[1,1]
```

The *falsifying tests* are different! 

How is this possible?

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

## Generators

QC defines a special *Generator* data type

```haskell
data Gen a = MkGen (StdGen -> Int -> a)
```

A `Gen a` is a function that takes as *input*

- a random number generator `StdGen`
- a "seed" `Int`

and returns as *output*

- a **value** of type `a`

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

## Creating `Gen`erators

There are some functions to *create* generators, e.g.

```haskell
choose :: (Int, Int) -> Gen Int
```

which 

- takes a pair of `(lo, hi)`

- returns a random generator for values between `lo` and `hi`

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

## Running `Gen`erators

To *execute* the `Gen` we need access to the system's "randomness"

Done via an `IO` "recipe"

```haskell
sample' :: Gen a -> IO [a]
```

Which 

- takes a `Gen`erator of `a` values and
- returns a *recipe* that produces a list of (10) `a` values

We can *run* the recipe in `ghci`

```haskell
>>> sample' (choose (0, 5))
[4,2,5,3,2,2,2,3,0,0,0]
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

## EXERCISE 

Lets write a function that returns a `Gen`erator over a list of `elements`

```haskell
elements :: [a] -> Gen a
elements = ???
```

So `elements [x0,x1,...,xn]` returns a `Gen`erator that randomly produces 
values from `x0`, `x1`, ... `xn`.

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

## PROBLEM: How to *combine* `Gen`erators?

Suppose I have a generator of positive `Int`

```haskell
pos :: Gen Int
pos = sample (0, 100)
```

How can I create a generator of a *pair* of positive `Int`s?

```haskell
posPair :: Gen (Int, Int)
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

## QUIZ 

`Gen`erator is a Monad! (you can [see details here][12])

- This will let us *combine* generators (like combining parsers...)

Which of the below implements `posPair :: Gen (Int, Int)` ?

- given `pos :: Gen Int` and `sample :: (Int, Int) -> Gen Int` 

```haskell
-- A
posPair = do { x1 <- pos; x2 <- pos; return (x1, x2) }

-- B
posPair = (pos, pos)

-- C
posPair = do { x <- pos; return (x, x) }  

-- D
posPair = Gen (4, 5)

-- E 
posPair = (sample (0, 100), sample (0, 100))
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

```haskell
posPair = do { x1 <- pos; x2 <- pos; return (x1, x2) }

-- >>> sample' posPair
-- [(29,71),(48,74),(89,53),(73,93),(0,40),(71,35),(23,69),(93,49),(59,58),(27,32),(88,45)]
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

## EXERCISE

Lets write a function that *mixes* a list of `Gen`erators

```haskell
oneOf :: [Gen a] -> Gen a
oneOf = ???
```

`oneOf [g0,g1,...,gn]` should be a generator that

- randomly selects *one of* `g0`,...`gn`

- randomly generates a value from the chosen generator

```haskell
>>> sample' (oneOf [choose (0,2), choose (10,12)])
[2,2,1,1,12,10,2,2,11,0,11]
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

`oneOf` is generalized into the `frequency` combinator

```haskell
frequency :: [(Int, Gen a)] -> Gen a
```

which builds *weighted mixtures* of individual `Gen`erators

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

## Types that can be `Gen`erated

QC has a `class` for types whose values can be randomly `Gen`erated

```haskell
class Arbitrary a where
  arbitrary :: Gen a
```

`T` is an instance of `Arbitrary` if there is `Gen T` function

- i.e. there is a generator of `T` values!

```haskell
randomThings :: (Arbitrary a) => IO [a]
randomThings = sample' arbitrary
```

Many standard types have `Arbitrary` instances

- Users write their own instances when testing their own types

```haskell
>>> randomThings :: IO [Int]
[0,-2,-2,0,-1,8,1,-14,-13,5,19]

>>> randomThings :: IO [(Int, Bool)] 
[(0,True),(1,True),(0,True),(6,False),(-5,True),(4,False),(-12,False),(-8,False),(5,False),(-9,False),(-7,False)]
-
>>> randomThings :: IO [String]
["","\a","\f","\779257W\SUBA","\84573","D\ACK\365059S","9W\554735G","g\SYN~W\62120\&4&[","\NULsc\18427fy(","Q`TI \n/TH","\461027\ESCZ`u\783094\&4B\SOHT\424692"]
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

## Plan

1. Property-based Testing
    - Properties are `Bool`ean-functions
    - Generate inputs, run function, check if result is `False`

2. Test Generation
    - `Gen a` is a monadic *generator* of `a` values
    - `Arbitrary` is a class for types with generators

3. **Case-Study**

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

Lets use QC to test *compiler optimizations*

- Learn how to *generate* structure data (*programs*)

- Learn how to *specify* fancy properties (*equivalence*)

Using the `WHILE` language from your HW assignment.

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

## WHILE: Syntax
TODO

## WHILE: Semantics
TODO

## Generating WHILE Programs
TODO

## Specification: Program Equivalence
TODO

## Checking an Optimization: Zero-Add-Elimination
TODO

## Checking an Optimization: Constant-Folding-ish
TODO

## Shrinking
TODO

## Recap: Property-Based Testing
TODO

## Recap: Property-Based Testing

1. Property-based Testing
    - Properties are `Bool`ean-functions
    - Generate inputs, run function, check if result is `False`

2. Test Generation
    - `Gen a` is a monadic *generator* of `a` values
    - `Arbitrary` is a class for types with generators

3. **Case-Study**





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
[12]: https://hackage.haskell.org/package/QuickCheck-2.14/docs/src/Test.QuickCheck.Gen.html#line-76
