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
>>> label (Node (Node (Leaf 'a') (Leaf 'b')) (Node (Leaf 'c') (Leaf 'a')))
    (Node
        (Node (Leaf ('a', 0)) (Leaf ('b', 0))) 
        (Node (Leaf ('c', 0)) (Leaf ('a', 1))))
```

That is, a _separate_ counter for each `a`, `b`, `c` etc.

**HINT** Use the following `Map k v` type

```haskell
empty         :: M.Map Char Int
lookupDefault :: Int -> Char -> M.Map Char Int -> Int
insert        :: Char -> Int -> M.Map Char Int -> M.Map Char Int
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


## Global Counter