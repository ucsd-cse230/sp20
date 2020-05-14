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
