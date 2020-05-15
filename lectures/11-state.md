---
title: Imperative Programming with The State Monad 
date: 2019-06-5
headerImg: books.jpg
--- 


## A Tree Datatype

A tree with data at the **leaves**

```haskell
data Tree a
    = Leaf a
    | Node (Tree a) (Tree a)
    deriving (Eq, Show)
```

Here's an example `Tree Char`

```haskell
charT :: Tree Char
charT = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')
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

## Lets Work it Out!

Write a function to add a *distinct* label to each *leaf*

```haskell
label :: Tree a -> Tree (a, Int)
label = ???
```

such that 

```haskell
>>> label (Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c'))
label (Node (Node (Leaf ('a', 0)) (Leaf ('b', 1))) (Leaf ('c', 2)))
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

## Labeling a Tree

```haskell
label :: Tree a -> Tree (a, Int)
label t       = t'
  where 
      (_, t') = (helper 0 t)

helper :: Int -> (Int, Tree (a, Int))
helper n (Leaf x)   = (n+1, Leaf (x, n))
helper n (Node l r) = (n'', Node l' r')
  where 
      (n', l')      = helper n l
      (n'', r')     = helper n' r
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

Now, modify `label` so that you get new numbers for each `letter` so,

```haskell
>>> keyLabel (Node (Node (Leaf 'a') (Leaf 'b')) (Node (Leaf 'c') (Leaf 'a')))
    (Node
        (Node (Leaf ('a', 0)) (Leaf ('b', 0))) 
        (Node (Leaf ('c', 0)) (Leaf ('a', 1))))
```

That is, a _separate_ counter for each *key* `a`, `b`, `c` etc.

**HINT** Use the following `Map k v` type

```haskell
-- | The empty Map
empty :: Map k v

-- | 'insert key val m` returns a new map that extends 'm'
--   by setting `key` to `val`
insert :: k -> v -> Map k v -> Map k v

-- | 'lookupDefault def key m' returns the value of `key`
--   in `m`  or `def` if `key` is not defined
lookupDefault :: v -> k -> Map k v -> v
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

## Common Pattern?

Both the functions have a common "shape" 

```haskell
    OldInt -> (NewInt, NewTree)

    OldMap -> (NewMap, NewTree)
```

If we generally think of `Int` and `Map Char Int` as **global state** 

```haskell
    OldState -> (NewState, NewVal)
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

## State Transformers

Lets capture the above "pattern" as a type

1. A **State** Type

```haskell
type State = ... -- lets "fix" it to Int for now... 
``` 

2. A **State Transformer** Type

```haskell
data ST a = STC (State -> (State, a))
```

A *state transformer* is a function that

* takes as input an **old** `s :: State`
* returns as output a **new** `s' :: State` and **value** `v :: a`

![](/static/img/monad1.png){#fig:ST .align-center width=80%}


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

## Executing Transformers

Lets write a function to _execute_ an `ST a`

```haskell
exec :: State -> ST a -> a
exec = ???
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

What is the value of `quiz` ?

```haskell
st :: St [Int]
st = STC (\n -> (n+3, [n, n+1, n+2]))

quiz = exec 100 st
```

**A.** `103`

**B.** `[100, 101, 102]`

**C.** `(103, [100, 101, 102])`

**D.** `[0, 1, 2]`

**E.** Type error


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

## Lets Make State Transformer a Monad!

```haskell
instance Monad ST where
    return :: a -> ST a
    return = returnST

    (>>=)  :: ST a -> (a -> ST b) -> ST b
    (>>=) = bindST
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

## EXERCISE: Implement `returnST`!

What is a valid implementation of `returnST`?

```haskell
type State = Int
data ST a  = STC (State -> (State, a))

returnST :: a -> ST a
returnST = ???
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

## What is `returnST` *doing* ? 

`returnST v` is a *state transformer* that ... ???

<br>
<br>
<br>

(Can someone suggest an explanation in English?)

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

## HELP 

Now, lets implement `bindST`!

```haskell
type State = Int

data ST a  = STC (State -> (State, a))

bindST :: ST a -> (a -> ST b) -> ST b
bindST = ???
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

## What is `returnST` *doing* ? 

`returnST v` is a *state transformer* that ... ???

<br>
<br>
<br>

(Can someone suggest an explanation in English?)

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


## What is `returnST` *doing* ? 

`returnST v` is a *state transformer* that ... ???

<br>
<br>
<br>

(Can someone suggest an explanation in English?)

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


## `bindST` lets us **sequence** state transformers


`st >>= f` 

1. Applies transformer `st` to an initial state `s`
    - to get output `s'` and value `x` 

2. Then applies function `f` to the resulting value `x`
    - to get a _second_ transformer

3. The _second_ transformer is applied to `s'`
    - to get final `s''` and value `y` 

**OVERALL:** Transform `s` to `s''` and produce value `y`     

![](/static/img/monad4.png)

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

## Lets Implement a Global Counter

The (counter) `State` is an `Int`

```haskell
type State = Int
```

A function that _increments_ the counter to _return_ the `next` `Int`.

```haskell
next :: ST Int
next = STC (\old -> let new = old + 1 in (new, old))
```

`next` is a *state transformer* that that returns `Int` values 

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

Recall that

```haskell
exec :: State -> ST a -> a
exec s (STC st) = snd (st s)

next :: ST Int
next = STC (\n -> (n+1, n))
```

What does `quiz` evaluate to?

```haskell
quiz = exec 100 next
```

**A.** `100`

**B.** `101`

**C.** `0`

**D.** `1`

**E.** `(101, 100)`

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

Recall the definitions

```haskell
exec :: State -> ST a -> a
exec s (STC st) = snd (st s)

next :: ST Int
next = STC (\n -> (n+1, n))
```

Now suppose we have

```haskell
wtf1 = ST Int
wtf1 = next >>= \n ->
         return n
```

What does `quiz` evaluate to?

```haskell
quiz = exec 100 wtf1
```

**A.** `100`

**B.** `101`

**C.** `0`

**D.** `1`

**E.** `(101, 100)`

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

Consider a function `wtf2` defined as

```haskell
wtf2 = next >>= \n1 ->
         next >>= \n2 ->
           next >>= \n3 ->
             return [n1, n2, n3]
```

What does `quiz` evaluate to?

```haskell
quiz = exec 100 wtf
```

**A.** Type Error!

**B.** [100, 100, 100]

**C.** [0, 0, 0]

**D.** [100, 101, 102]

**E.** [102, 102, 102]

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

## Chaining Transformers

`>>=` lets us *chain* transformers into *one* big transformer!

So we can define a function to _increment the counter by 3_

```haskell
-- Increment the counter by 3
next3 :: ST [Int, Int]
next3 = next >>= \n1 ->
          next >>= \n2 ->
            next >>= \n3 ->
                return [n1,n2,n3]
```

And then sequence it _twice_ to get

```haskell
next6 :: ST [Int]
next6 = next3 >>= \ns_1_2_3 ->
          next3 >>= \ns_4_5_6 ->
            return (ns_123 ++ ns_4_5_6)
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Lets `do` the above examples

Remember, `do` is just nice syntax for the above!

```haskell
-- Increment the counter by 3
next3 :: ST [Int, Int]
next3 = do
  n1 <- next
  n2 <- next
  n3 <- next
  return [n1,n2,n3]
```

And then sequence it _twice_ to get

```haskell
next6 :: ST [Int]
next6 = do
  ns_123 <- next3
  ns_456 <- next3
  return (ns_123 ++ ns_4_5_6)
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

## Labeling a Tree with a "Global Counter"

Lets **rewrite** our `Tree` labeler with `ST`

```haskell
helperS :: Tree a -> ST (Tree (a, Int))
helperS = ???
```

<br>
<br>
<br>
<br>
<br>
<br>

### Wow, compare to the old code! 

```haskell
helper :: Int -> (Int, Tree (a, Int))
helper n (Leaf x)   = (n+1, Leaf (x, n))
helper n (Node l r) = (n'', Node l' r')
  where 
      (n', l')      = helper n l
      (n'', r')     = helper n' r
```

Avoid worrying about propagating the "right" counters 

- Automatically handled by `ST` monad instance! 

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Executing the Transformer

In the **old** code we _called_ the helper with an _initial_ counter  `0`

```haskell
label :: Tree a -> Tree (a, Int)
label t       = t'
  where
      (_, t') = helper 0 t
```

In the **new** code what should we do?

```haskell
helperS :: Tree a -> ST (Tree (a, Int))
helperS = ...

labelS :: Tree a -> Tree (a, Int)
labelS = ???
```

Now, we should be able to `exec` the `labelS` transformer

```haskell
>>> labelS (Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c'))
(Node (Node (Leaf ('a', 0)) (Leaf ('b', 1))) (Leaf ('c', 2)))
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

## How to implement `keyLabel`?

So far, we _hardwired_ an `Int` counter as our `State`

```haskell
type State = Int

data ST a  = STC (State -> (State, a))
```

Have to _reimplement_ the monad if we want a _different_ state?

- e.g. `Map Char Int` to implement `keyLabel`

**Don't Repeat Yourself!**

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

## A Generic State Transformer

Don't have _separate_ types for `IntList` and `CharList` 

- Define a generic list `[a]` where `a` is a _type parameter_

- Instantiate `a` to get `[Int]` and `[Char]`

Similarly, reuse `ST` with a **type** parameter!

```haskell
data ST s a = STC (s -> (s, a))
```

- **State** is represented by type `s`
- **Return Value** is the type `a` (as before).

Lets make the above a(n instance of) `Monad` 

```haskell
instance Monad (ST s) where
  return x = STC (\s -> (s, x))
  st >>= f = STC (\s -> let (s', x) = apply st s 
                        in apply (f x) s')

apply :: ST s a -> s -> (s, a)
apply (STC f) s = f s

exec :: ST s a -> s -> a
exec st s    = v 
  where 
      (s',v) = apply st s
```

(*exactly* the same code as `returnST` and `bindST`)

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

## Lets implement `keyLabel`

```haskell
label :: Tree a -> Tree (a, Int)
label t       = t'
  where
      (_, t') = helper 0 t
```

In the **new** code what should we do?

```haskell
keyHelperS :: Tree Char -> ST (Tree (Char, Int))
keyHelperS = ...

keyLabelS :: Tree Char -> Tree (Char, Int)
keyLabelS t = exec ( ) 
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


## Generically Getting and Setting State 

Accessing and Updat