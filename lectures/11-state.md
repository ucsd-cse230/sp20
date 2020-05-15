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

* takes as input an **old** `State`
* returns as output a **new** `State` and **value** `a`