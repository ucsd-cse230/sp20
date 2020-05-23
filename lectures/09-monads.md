---
title: Functors and Monads
date: 2019-06-5
headerImg: books.jpg
---

## Abstracting Code Patterns

a.k.a. **Dont Repeat Yourself**

### Lists

```haskell
data List a
  = []
  | (:) a (List a)
```

### Rendering the Values of a List

```haskell
-- >>> incList [1, 2, 3]
-- ["1", "2", "3"]

showList        :: [Int] -> [String]
showList []     =  []
showList (n:ns) =  show n : showList ns
```

### Squaring the values of a list

```haskell
-- >>> sqrList [1, 2, 3]
-- 1, 4, 9

sqrList        :: [Int] -> [Int]
sqrList []     =  []
sqrList (n:ns) =  n^2 : sqrList ns
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

## Common Pattern: `map` over a list

Refactor iteration into `mapList`

```haskell
mapList :: (a -> b) -> [a] -> [b]
mapList f []     = []
mapList f (x:xs) = f x : mapList f xs
```

Reuse `map` to implement `inc` and `sqr`

```haskell
showList xs = map (\n -> show n) xs

sqrList  xs = map (\n -> n ^ 2)  xs
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


## Trees

Same "pattern" occurs in other structures!

```haskell
data Tree a
  = Leaf
  | Node a (Tree a) (Tree a)
```

### Incrementing the values of a Tree 


```haskell
-- >>> showTree (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf))
-- (Node "2" (Node "1" Leaf Leaf) (Node "3" Leaf Leaf))

showTree :: Tree Int -> Tree String
showTree Leaf         = ???
showTree (Node v l r) = ???
```

### Squaring the values of a Tree

```haskell
-- >>> sqrTree (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf))
-- (Node 4 (Node 1 Leaf Leaf) (Node 9 Leaf Leaf))

sqrTree :: Tree Int -> Tree Int
sqrTree Leaf         = ???
sqrTree (Node v l r) = ???
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## QUIZ: `map` over a Tree

Refactor iteration into `mapTree`! What should the type of `mapTree` be?

```haskell
mapTree :: ???

showTree t = mapTree (\n -> show n) t
sqrTree  t = mapTree (\n -> n ^ 2)  t

{- A -} (Int -> Int)    -> Tree Int -> Tree Int
{- B -} (Int -> String) -> Tree Int -> Tree String
{- C -} (Int -> a)      -> Tree Int -> Tree a
{- D -} (a -> a)        -> Tree a   -> Tree a
{- E -} (a -> b)        -> Tree a   -> Tree b
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Lets write `mapTree`

```haskell
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf         = ???
mapTree f (Node v l r) = ???
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## QUIZ

Wait ... there is a common pattern across two _datatypes_

```haskell
mapList :: (a -> b) -> List a -> List b    -- List
mapTree :: (a -> b) -> Tree a -> Tree b    -- Tree
```

Lets make a `class` for it!

```haskell
class Mappable t where
  gmap :: ???
```

What type should we give to `gmap`?

```haskell
{- A -} (b -> a) -> t b    -> t a
{- B -} (a -> a) -> t a    -> t a
{- C -} (a -> b) -> [a]    -> [b]
{- D -} (a -> b) -> t a    -> t b
{- E -} (a -> b) -> Tree a -> Tree b
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

### Reuse Iteration Across Types

Haskell's libraries use the name `Functor` instead of `Mappable` 

```haskell
instance Functor [] where
  fmap = mapList

instance Functor Tree where
  fmap = mapTree
```

And now we can do

```haskell
-- >>> fmap (\n -> n + 1) (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf))
-- (Node 4 (Node 1 Leaf Leaf) (Node 9 Leaf Leaf))

-- >>> fmap show [1,2,3]
-- ["1", "2", "3"]
```

## A Type to Represent Expressions

```haskell
data Expr
  = Number Int            -- ^ 0,1,2,3,4
  | Plus   Expr Expr      -- ^ e1 + e2
  | Minus  Expr Expr      -- ^ e1 - e2
  | Mult   Expr Expr      -- ^ e1 * e2
  | Div    Expr Expr      -- ^ e1 / e2
  deriving (Show)
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Some Example Expressions 

```haskell
e1 = Plus  (Number 2) (Number 3)    -- 2 + 3
e2 = Minus (Number 10) (Number 4)   -- 10 - 4
e3 = Mult e1 e2                     -- (2 + 3) * (10 - 4)
e4 = Div  e3 (Number 3)             -- ((2 + 3) * (10 - 4)) / 3 
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>


## EXERCISE: An Evaluator for Expressions

Fill in an implementation of `eval`

```haskell
eval :: Expr -> Int
eval e = ??? 
```

so that when you're done we get

```haskell
-- >>> eval e1
-- 5 
-- >>> eval e2
-- 6
-- >>> eval e3
-- 30 
-- >>> eval e4
-- 10
```

## QUIZ

What does the following evaluate to?

```haskell
quiz = eval (Div (Number 60) (Minus (Number 5) (Number 5)))
```

**A.** `0`
**B.** `1`
**C.** Type error
**D.** Runtime exception
**E.** `NaN`

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



## To avoid crash, return a `Result`

Lets make a data type that represents `Ok` or `Error`

```haskell
data Result v 
  = Ok   v       -- ^ a "successful" result with value `v`
  | Error String -- ^ something went "wrong" with `message`
  deriving (Eq, Show)
```

## EXERCISE 

Can you implement a `Functor` instance for `Result`?

```haskell
instance Functor Result where
  fmap f (Error msg) = ???
  fmap f (Ok val)    = ???
```

When you're done you should see

```haskell
-- >>> fmap (\n -> n ^ 2) (Ok 9) 
-- Ok 81

-- >>> fmap (\n -> n ^ 2) (Error "oh no") 
-- Error "oh no"
```

## Evaluating without Crashing 

Instead of *crashing* we can make our `eval` return a `Result Int`

```haskell
eval :: Expr -> Result Int
```

- If a sub-expression has a *divide by zero* return `Error "..."`
- If all sub-expressions are *safe* then return `Ok n`


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## EXERCISE: Implement `eval` with `Result`

```haskell
eval :: Expr -> Result Int
eval (Number n)    = ?
eval (Plus  e1 e2) = ?
eval (Minus e1 e2) = ?
eval (Mult  e1 e2) = ?
eval (Div   e1 e2) = ?
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## The Good News

No nasty exceptions!

```haskell
>>> eval (Div (Number 6) (Number 2))
Ok 3

>>> eval (Div (Number 6) (Number 0))
Error "yikes dbz:Number 0"

>>> eval (Div (Number 6) (Plus (Number 2) (Number (-2))))
Error "yikes dbz:Plus (Number 2) (Number (-2))"
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## The BAD News! 

The code is **super gross**

![Escher's Staircase](https://upload.wikimedia.org/wikipedia/en/a/a3/Escher%27s_Relativity.jpg){#fig:escher .align-center width=80%}

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Lets spot a Pattern

The code is gross because we have these cascading blocks

```haskell
case e1 of
  Error err1 -> Error err1
  Ok    v1   -> case e2 of
                  Error err2 -> Error err2
                  Ok    v1   -> Ok    (v1 + v2)
```

but *look closer* ... both blocks have a **common pattern**

```haskell
case e of
  Error err -> Error err
  Value v   -> {- do stuff with v -}
```

1. Evaluate `e`
2. If the result is an `Error` then _return_ that error.
3. If the result is a `Value v` then _further process_ with `v`.

<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Lets Bottle that Pattern in Two Functions

![Bottling a Magic Pattern](/static/img/fairy.png){#fig:types .align-center width=20%}

- `>>=` (pronounced _bind_)
- `return` (pronounced _return_)

```haskell
(>>=) :: Result a -> (a -> Result b) -> Result b
(Error err) >>= _       = Error err
(Value v)   >>= process = process v

return :: a -> Result a
return v = Ok v
```

**NOTE:** `return` is **not a keyword** 

- it is the name of a function!

<br>
<br>
<br>
<br>
<br>
<br>
<br>


## A Cleaned up Evaluator

The magic bottle lets us clean up our `eval`

```haskell
eval :: Expr -> Result Int
eval (Number n)   = Ok n
eval (Plus e1 e2) = eval e1 >>= \v1 ->
                      eval e2 >>= \v2 ->
                        Ok (v1 + v2)

eval (Div e1 e2)  = eval e1 >>= \v1 ->
                      eval e2 >>= \v2 ->
                        if v2 == 0
                          then Error ("yikes dbz:" ++ show e2)
                          else Ok (v1 `div` v2)
```

**The gross _pattern matching_ is all hidden inside `>>=`**

Notice the `>>=` takes *two* inputs of type:

- `Result Int`        (e.g. `eval e1` or `eval e2`)
- `Int -> Result Int` (e.g. the _processor_ takes the `v` and does stuff with it)

In the above, the processing functions are written using `\v1 -> ...` and `\v2 -> ...`

**NOTE:** It is _crucial_ that you understand what the code above
is doing, and why it is actually just a "shorter" version of the
(gross) nested-case-of `eval`.

<br>
<br>
<br>
<br>
<br>
<br>
<br>


## A Class for `>>=`

The `>>=` operator is useful across **many** types! 

- like `fmap` or `show` or `toJSON` or `==`, or `<=` 

Lets capture it in a typeclass:

```haskell
class Monad m where
  -- (>>=)  :: Result a -> (a -> Result b) -> Result b
     (>>=)  :: m a      -> (a -> m b)      -> m b

  -- return :: a -> Result a
     return :: a -> m a
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>

## `Result` is an instance of `Monad`

Notice how the definitions for `Result` fit the above, with `m = Result` 

```haskell
instance Monad Result where
  (>>=) :: Result a -> (a -> Result b) -> Result b
  (Error err) >>= _       = Error err
  (Value v)   >>= process = process v

  return :: a -> Result a
  return v = Ok v
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Syntax for `>>=`

In fact `>>=` is *so* useful there is special syntax for it.

Instead of writing

```haskell
e1 >>= \v1 ->
  e2 >>= \v2 ->
    e3 >>= \v3 ->
      e
```

you can write

```haskell
do v1 <- e1
   v2 <- e2
   v3 <- e3
   e
```

or if you like curly-braces 

```haskell
do { v1 <- e1; v2 <- e2; v3 <- e3; e }
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Simplified Evaluator

Thus, we can further simplify our `eval` to:

```haskell
eval :: Expr -> Result Int
eval (Number n)   = return n
eval (Plus e1 e2) = do v1 <- eval e1
                       v2 <- eval e2
                       return (v1 + v2)
eval (Div e1 e2)  = do v1 <- eval e1
                       v2 <- eval e2
                       if v2 == 0
                         then Error ("yikes dbz:" ++ show e2)
                         else return (v1 `div` v2)
```

Which now produces the result

```haskell
>>> evalR exQuiz
Error "yikes dbz:Minus (Number 5) (Number 5)"
```






<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
