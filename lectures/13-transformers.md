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
  | Div    Expr Expr      -- ^ e1 / e2
  deriving (Show)
```

We had a **potentially crashing** evaluator

```haskell
eval :: Expr -> Int
eval (Number n)    = n
eval (Plus  e1 e2) = eval e1   +   eval e2
eval (Div   e1 e2) = eval e1 `div` eval e2

-- >>> eval (Div (Val 10) (Plus (Number 5) (Number (-5))))
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
  (Err s) >>= _ = Err s
```

and then we can write

```haskell
eval :: Expr -> Result Int
eval (Number n)    = return n
eval (Plus  e1 e2) = do {n1 <- eval e1; n2 <- eval e2; return (n1   +   n2) } 
eval (Div   e1 e2) = do { n1 <- eval e1; 
                          n2 <- eval e2; 
                          if n2 /= 0 
                            then return (n1 `div` n2) 
                            else Err ("DBZ: " ++ show e2)
                        }
```

which doesn't crash but returns an `Err`

```haskell
>>> eval (Div (Number 10) (Plus (Number 5) (Number (-5))))
Err "DBZ: Plus (Number 5) (Number (-5))"
```

and when it succeeds it returns an `Ok`

```haskell
>>> eval (Div (Number 10) (Plus (Number 5) (Number (-5))))
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


We can rewrite `eval` to return an `Either` 

```haskell
eval :: Expr -> Either Expr Int
eval (Number n)    = return n
eval (Plus  e1 e2) = do n1 <- eval e1 
                        n2 <- eval e2
                        return (n1+n2)
eval (Div   e1 e2) = do n1 <- eval e1 
                        n2 <- eval e2
                        if n2 /= 0 
                          then return (n1 `div` n2) 
                          else Left e2
```

What does `quiz` evaluate to?

```haskell
quiz = eval (Div (Val 10) (Plus (Number 5) (Number (-5))))
```

**A.** `Err "DBZ: Plus (Number 5) (Number (-5))"`

**B.** `Left "DBZ: Plus (Number 5) (Number (-5))"`

**C.** Run-time Exception 

**D.** `Plus (Number 5) (Number (-5))`

**E.** `Left (Plus (Number 5) (Number (-5)))`


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## `Either` is an **Exception** Monad! 

What can you do with exceptions?

1. `throwError` an exception (with some value) ... 

2. `catchError` an exception (and use its value) ...

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## 1. `throw`ing an Exception

We can simply define 

```haskell
throw :: e -> Either e a
throw exn = Left exn
``` 

and now _voila_

```haskell
eval :: Expr -> Either Expr Int
eval (Number n)    = return n
eval (Plus  e1 e2) = do n1 <- eval e1 
                        n2 <- eval e2
                        return (n1 + n2)
eval (Div   e1 e2) = do n1 <- eval e1 
                        n2 <- eval e2
                        if n2 /= 0 
                          then return (n1 `div` n2) 
                          else throw e2
```

*Exactly* the same evaluator 

- Result is a `Left` ==> an *exception* came all the way to the top.

- `Either` monad ensures the "exception" shoots to the top! 

```haskell
>>> eval (Div (Numer 10) (Plus (Number 5) (Number (-5))))
Left (Minus (Number 5) (Number 5))
```

No further evaluation happens after a `throw` because ???

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## `catch`ing an exception

How to _catch_ an exception?

Lets change our `Expr` type to 

```haskell
data Expr
  = Number  Int            -- ^ 0,1,2,3,4
  | Plus    Expr Expr      -- ^ e1 + e2
  | Try     Expr Int       
  deriving (Show)
```

Informally, `try e n` evaluates to `e` but 

- if `e` is undefined due to *divide-by-zero* 

- then evaluate to `n`

```haskell
eval :: Expr -> Either Expr Int
eval (Number n)    = return n
eval (Plus  e1 e2) = do n1 <- eval e1 
                        n2 <- eval e2
                        return (n1+n2)
eval (Div   e1 e2) = do n1 <- eval e1 
                        n2 <- eval e2
                        if n2 /= 0 
                          then return (n1 `div` n2) 
                          else throw e2
eval (Try e n)     = catch (eval e) (\_ -> return n)
```

## QUIZ 

What should the _type_ of `catch` be?

**A.** `Either e a -> (a -> Either e b) -> Either e b`

**B.** `Either e a -> (e -> Either e b) -> Either e b`

**C.** `Either e a -> (e -> Either e a) -> Either e a`

**D.** `Either e a -> Either e a -> Either e a`

**E.** `Either e a -> Either e b -> Either e b`


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Implementing `catch`

Lets implement the `catch` function!

```haskell
catch :: Either e a -> (e -> Either e a) -> Either e a
catch (Left  e) handler = ???
catch (Right a) handler = ???
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

```haskell
catch :: Either e a -> (e -> Either e a) -> Either e a
catch (Left  e) handle  = ???
catch (Right a) handler = ???

eval :: Expr -> Either Expr Int
eval (Number n)    = return n
eval (Plus  e1 e2) = do n1 <- eval e1 
                        n2 <- eval e2
                        return (n1+n2)
eval (Div   e1 e2) = do n1 <- eval e1 
                        n2 <- eval e2
                        if n2 /= 0 
                          then return (n1 `div` n2) 
                          else throw e2
eval (Try e n)     = catch (eval e) (\_ -> return n)

e1  = Div (Number 10) (Plus (Number 5) (Number (-5)))
e1' = Try e1 7

quiz = eval (Try e1 7)
```

What does `quiz` evaluate to?

**A.** `Right 7`

**B.** `Left 7`

**C.** `Right 0`

**D.** `Left 0`

**E.** `Left (Plus (Number 5) (Number (-5)))`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## `Either` is an **Exception** Monad! 

1. `throw` an exception (with some value) ... 

2. `catch` an exception (and use its value) ...

```haskell
throw :: e -> Either e a
throw e = Left e

catch :: Either e a -> (e -> Either e a) -> Either e a
catch (Left  e) handle = handle e
catch (Right e) _      = Right  e
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

## Monads Can Be Used for Many Things!

* Partial Functions
* Global State 
* Parsing
* Exceptions
* Test Generation
* Concurrency 
* ... 

... but what if I want *Exceptions* **and** *Global State* ?

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Mixing Monads

What if I want *Exceptions* **and** *Global State* ?

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Profiling with the ST Monad

- "Profiling"
- ST 
- Yay, monads = global state!

## Transformers

* Step 1: **Specifying**   Monads with Extra Features

* Step 2: **Implementing** Monads with Extra Features 

## FIXME

![](/static/img/lec-tx-transforming.png)

TRANSFORMER
 = Monad -> Monad


