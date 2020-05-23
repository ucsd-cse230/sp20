---
title: Mixing Monads
date: 2020-05-28
headerImg: books.jpg
--- 

## Monads Can Be Used for Many Things!

* Partial Functions
* Global Variables
* Parsing
* Exceptions
* Test Generation
* Concurrency 
* ... 

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

## Exception Handling 

Recall our expressions with division

```haskell
data Expr
  = Number Int            -- ^ 0,1,2,3,4
  | Plus   Expr Expr      -- ^ e1 + e2
  | Minus  Expr Expr      -- ^ e1 - e2
  | Mult   Expr Expr      -- ^ e1 * e2
  | Div    Expr Expr      -- ^ e1 / e2
  deriving (Show)
```

We had a **potentially crashing** evaluator

```haskell
eval :: Expr -> Int
eval (Number n)    = n
eval (Plus  e1 e2) = eval e1   +   eval e2
eval (Minus e1 e2) = eval e1   -   eval e2
eval (Mult  e1 e2) = eval e1   *   eval e2
eval (Div   e1 e2) = eval e1 `div` eval e2

-- >>> eval (Div (Val 10) (Minus (Number 5) (Number 5)))
-- Exception: Divide by zero
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

## We defined a `Result` type

```haskell
data Result a = Ok a | Err String
```

made it a `Monad` 

```haskell
instance Monad Result where
  return x      = Ok x
  (Ok v)  >>= f = f v
  (Err s) >>= _ = s
```

and then we can write

```haskell
eval :: Expr -> Result Int
eval (Number n)    = return n
eval (Plus  e1 e2) = do {n1 <- eval e1; n2 <- eval e2; return (n1   +   n2) } 
eval (Minus e1 e2) = do {n1 <- eval e1; n2 <- eval e2; return (n1   -   n2) } 
eval (Mult  e1 e2) = do {n1 <- eval e1; n2 <- eval e2; return (n1   *   n2) } 
eval (Div   e1 e2) = do { n1 <- eval e1; 
                          n2 <- eval e2; 
                          if n2 /= 0 
                            then return (n1 `div` n2) 
                            else Err ("DBZ: " ++ show e2)
                        } 
```

which doesn't crash but returns an `Err`

```haskell
>>> eval (Div (Val 10) (Minus (Number 5) (Number 5)))
Err "DBZ: Minus (Number 5) (Number 5)"
```

and when it succeeds it returns an `Ok`

```haskell
>>> eval (Div (Val 10) (Plus (Number 5) (Number 5)))
Ok 1
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

## Generalizing `Result` to `Either`

The *standard library* generalizes the `Result` type to `Either` 

```haskell
data Result   a = Err String | Ok a 

data Either e a = Left e     | Right a
```

* `Err s`    becomes `Left s`
* `Ok v`     becomes `Right v`
* `Result a` becomes `Either String a`

(But we can data _other_ than `String` in the `Left` values)



<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## EXERCISE: Generalizing `Result` Monad to `Either` Monad

Lets translate the old `Monad` instance for `Result`

```haskell
instance Monad Result where

  -- return :: a -> Result a
  return x      = Ok x

  -- (>>=) :: Result a -> (a -> Result b) -> Result b
  (Ok v)  >>= f = f v
  (Err s) >>= _ = s
```

into a `Monad` instance for `Either`

```haskell
instance Monad (Either e) where
  -- return :: a -> Either e a
  return x        = ???

  -- (>>=) :: Either e a -> (a -> Either e b) -> Either e b
  (Right v) >>= f = ???  
  (Left  s) >>= _ = ??? 
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


## QUIZ

What does `quiz` evaluate to?

```haskell
eval :: Expr -> Either Expr Int
eval (Number n)    = return n
eval (Plus  e1 e2) = do { n1 <- eval e1; n2 <- eval e2; return (n1   +   n2) } 
eval (Minus e1 e2) = do { n1 <- eval e1; n2 <- eval e2; return (n1   -   n2) } 
eval (Mult  e1 e2) = do { n1 <- eval e1; n2 <- eval e2; return (n1   *   n2) } 
eval (Div   e1 e2) = do { n1 <- eval e1; n2 <- eval e2; 
                          if n2 /= 0 
                            then return (n1 `div` n2) 
                            else Left e2
                        } 

quiz = eval (Div (Val 10) (Minus (Number 5) (Number 5)))
```

**A.** `Err "DBZ: Minus (Number 5) (Number 5)"`
**B.** `Left "DBZ: Minus (Number 5) (Number 5)"`
**C.** Run-time Exception 
**D.** `Minus (Number 5) (Number 5)`
**E.** `Left (Minus (Number 5) (Number 5))`

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

- Try-Catch 
  - WithDefault Expr Int
- Yay, monads = exceptions!

## `Either` is an **exception-handling** Monad! 


## Profiling with the ST Monad

- "Profiling"
- ST 
- Yay, monads = global state!

## Mixing Monads

What if I want exceptions AND state?

## Transformers

* Step 1: **Specifying** Monads with Extra Features
* Step 2: **Using**      Monads with Extra Features
* Step 3: **Creating**   Monads with Extra Features 

## FIXME

![](/static/img/lec-tx-transforming.png)

TRANSFORMER
 = Monad -> Monad


