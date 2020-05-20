---
title: Parser Combinators
date: 2020-05-22
headerImg: books.jpg
--- 

## Before we continue ... 

A Word from the Sponsor! 

			Don't Fear Monads

They are just a versatile abstraction, like `map` or `fold`.

## Parsers 

A _parser_ is a function that 

- converts _unstructured_ data (e.g. `String`, array of `Byte`,...) 
- into _structured_ data (e.g. JSON object, Markdown, Video...)

```haskell
type Parser = String -> StructuredObject
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

## Every large software system contains a Parser


| **System**    | **Parses**            |
|:--------------|:----------------------|
| Shell Scripts | Command-line options  |
| Browsers      | HTML                  |
| Games         | Level descriptors     |
| Routers       | Packets               |
| Netflix       | Video                 | 
| Spotify       | Audio, Playlists...   | 

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## How to build Parsers?

Two standard methods 

### Regular Expressions 

- Doesn't really scale beyond simple things 
- No nesting, recursion

### Parser Generators 

1. Specify *grammar* via rules

```haskell
Expr : Var            { EVar $1       }
     | Num            { ENum $1       }
     | Expr Op Expr   { EBin $1 $2 $3 }
     | '(' Expr ')'   { $2            }
     ;
```

2. Tools like `yacc`, `bison`, `antlr`, `happy` 
  - convert *grammar* into *executable function* 

<br>
<br>
<br>
<br>
<br>
<br>


## Grammars Don't Compose! 

If we have *two* kinds of structured objects `Thingy` and `Whatsit`. 

```haskell
Thingy : rule 	{ action } 
;

Whatsit : rule  { action }
;
``` 

To parse *sequences* of `Thingy` and `Whatsit` we must *duplicate* the rules

```haskell
Thingies : Thingy Thingies  { ... } 
           EmptyThingy      { ... }
;

Whatsits : Whatsit Whatsits { ... }
           EmptyWhatsit     { ... }
;
```

No nice way to _reuse_ the sub-parsers for `Whatsit` and `Thingy` :-(

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## A New Hope: Parsers as Functions

Lets think of parsers directly **as functions** that 

- **Take** as input a `String`
- **Convert** a part of the input into a `StructuredObject`
- Return the **remainder** unconsumed to be parsed _later_

```haskell
data Parser a = P (String -> (a, String))
```

A `Parser a` 

- Converts a _prefix_ of a `String` 
- Into a _structured_ object of type `a` and 
- Returns the _suffix_ `String` unchanged

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Parsers Can Produce Many Results

Sometimes we want to parse a `String` like

```haskell
"2 - 3 - 4"
```

into a **list** of possible results

```haskell
[(Minus (Minus 2 3) 4),   Minus 2 (Minus 3 4)]
```

So we generalize the `Parser` type to

```haskell
data Parser a = P (String -> [(a, String)])
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

## EXERCISE 

Given the definition 

```haskell
data Parser a = P (String -> [(a, String)])
```

Implement a function

```haskell
runParser :: Parser a -> String -> [(a, String)]
runParser p s = ???
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

Given the definition 

```haskell
data Parser a = P (String -> [(a, String)])
```

Which of the following is a valid `Parser Char`

- that returns the **first** `Char` from a string (if one exists)

```haskell
-- A
oneChar = P (\cs -> head cs)

-- B
oneChar = P (\cs -> case cs of 
                      []   -> [('', []) 
                      c:cs -> (c, cs))

-- C
oneChar = P (\cs -> (head cs, tail cs))

-- D
oneChar = P (\cs -> [(head cs, tail cs)])

-- E
oneChar = P (\cs -> case cs of
                      [] -> [] 
                      cs -> [(head cs, tail cs)])
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

## Lets Run Our First Parser!

```haskell
>>> runParser oneChar "hey!"
[('h', "ey")]

>>> runParser oneChar "yippee"
[('y', "ippee")]

>>> runParser oneChar ""
[]
```

**Failure** to parse means result is an **empty** list!

<br>
<br>
<br>
<br>
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

Your turn: Write a parser to grab **first two chars** 

```haskell
twoChar :: Parser (Char, Char)
twoChar = P (\cs -> ???) 
```

When you are done, we should get

```haskell
>>> runParser twoChar "hey!"
[(('h', 'e'), "y!")]

>>> runParser twoChar "h"
[]
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


## QUIZ

Ok, so recall 

```haskell
twoChar :: Parser (Char, Char)
twoChar  = P (\cs -> case cs of
                       c1:c2:cs' -> [((c1, c2), cs')]
                       _         -> [])
``` 

Suppose we had some `foo` such that `twoChar'` was equivalent to `twoChar`  

```haskell
twoChar' :: Parser (Char, Char)
twoChar' = foo oneChar oneChar 
```

What must the type of `foo` be?

**A.** `Parser (Char, Char)` 

**B.** `Parser Char -> Parser (Char, Char)`

**C.** `Parser a -> Parser a -> Parser (a, a)` 

**D.** `Parser a -> Parser b -> Parser (a, b)` 

**E.** `Parser a -> Parser (a, a)` 

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## EXERCISE: A `forEach` Loop

Lets write a function

```haskell
forEach :: [a] -> (a -> [b]) -> [b]
forEach xs f = ???
```

such that we get the following behavior

```haskell
>>> forEach [] (\i -> [i, i + 1])
[]

>>> forEach [10,20,30] (\i -> [show i, show (i+1)])
["10", "11", "20", "21", "30", "31"]
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

## QUIZ 

What does `quiz` evaluate to?

```haskell
quiz = forEach [10, 20, 30] (\i -> 
         forEach [0, 1, 2] (\j -> 
           [i + j] 
         )
       )
```

**A.** `[10,20,30,0,1,2]`

**B.** `[10,0,20,1,30,2]`

**C.** `[[10,11,12], [20,21,22] [30,31,32]]`

**D.** `[10,11,12,20,21,22,30,31,32]`

**E.** `[32]`

<br>
<br>
<br>
<br>
<br>
<br>
<br>




## A `pairP` Combinator

Lets implement the above as `pairP`

```haskell
forEach :: [a] -> (a -> [b]) -> [b]
forEach xs f = concatMap f xs 

pairP :: Parser a -> Parser b -> Parser (a, b)
pairP aP bP = P (\s -> forEach (runParser aP s) (\(a, s') ->
                         forEach (runParser bP s') (\(b, s'') -> 
                           ((a, b), s'')
                       )
                ) 
```

Now we can write 

```haskell
twoChar = pairP oneChar oneChar
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

What does `quiz` evaluate to?


```haskell
twoChar = pairP oneChar oneChar

quiz    = runParser twoChar "h" 
```

**A.** `[((`h`, `h`), "")]`

**B.** `[(`h`, "")]`

**C.** `[("", "")]`

**D.** `[]`

**E.** Run-time exception

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Does the `Parser a` type remind you of something?

Lets implement the above as `pairP`

```haskell
data Parser a = P (String -> [(a, String)])

data ST s a   = S (s -> (a, s))
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

## `Parser` is a Monad!

Like a state transformer, [`Parser` is a monad!][2]

We need to implement two functions

```haskell
returnP :: a -> Parser a

bindP   :: Parser a -> (a -> Parser b) -> Parser b
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

## QUIZ

Which of the following is a valid implementation of `returnP`

```haskell
data Parser a = P (String -> [(a, String)])

returnP   :: a -> Parser a

returnP a = P (\s -> [])          -- A

returnP a = P (\s -> [(a, s)])    -- B

returnP a = P (\s -> (a, s))      -- C

returnP a = P (\s -> [(a, "")])   -- D

returnP a = P (\s -> [(s, a)])    -- E
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

**HINT:** `return a` should just 

- "produce" the parse result `a` and 
- leave the string unconsumed.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Bind 

Next, lets implement `bindP` 

  - we almost saw it as `pairP`

```haskell
bindP :: Parser a -> (a -> Parser b) -> Parser b
bindP aP fbP = P (\s -> 
  forEach (runParser aP s) (\(a, s') -> 
    forEach (runParser (fbP a) s') (\(b, s'') ->
      [(b, s'')]
    )   
  )
)
```

The function 

- Builds the `a` values out of `aP` (using `runParser`)
- Builds the `b` values by calling `fbP a` on the _remainder_ string `s'` 
- Returns `b` values and the remainder string `s''` 

![](/static/img/bind-0.png)


## The `Parser` Monad

We can now make `Parser` an instance of `Monad`

```haskell
instance Monad Parser where
  (>>=)  = bindP
  return = returnP
```

![](https://www.artgroup.com/assets/img/products/WDC44013)

And now, let the *wild rumpus start!*

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Parser Combinators

Lets write lots of *high-level* operators to **combine** parsers!

Here's a cleaned up `pairP` 

```haskell
pairP :: Parser a -> Parser b -> Parser (a, b)
pairP aP bP = do 
  a <- aP
  b <- bP
  return (a, b)
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

## A Failure Parser

Surprisingly useful, always _fails_ 

- i.e. returns `[]` no successful parses 

```haskell
failP :: Parser a
failP = P (\_ -> [])
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

## QUIZ

Consider the parser 

```haskell
satP :: (Char -> Bool) -> Parser Char
satP p = do 
  c <- oneChar
  if p c then return c else failP
```

What is the value of

```haskell
quiz1 = runParser (\c -> c == 'h') "hellow"
quiz2 = runParser (\c -> c == 'h') "yellow"
```

|       | `quiz1`           | `quiz2`              | 
|------:|------------------:|---------------------:| 
| **A** | `[]`              | `[]`                 |
| **B** | `[('h', "ellow")]`| `[('y', "ellow")]`   |
| **C** | `[('h', "ellow")]`| `[]`                 |
| **D** | `[]`              | `[('y', "ellow")]`   |


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Parsing Alphabets and Numerics 

We can now use `satP` to write 

```haskell
-- parse ONLY the Char c
char :: Parser Char
char c = satP (\c' -> c == c')

-- parse ANY ALPHABET 
alphaCharP :: Parser Char
alphaCharP = satP isAlpha

-- parse ANY NUMERIC DIGIT
digitCharP :: Parser Char
digitCharP = satP isDigit
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

## QUIZ 

We can parse a single `Int` digit

```haskell
digitIntP :: Parser Int
digitIntP = do
  c <- digitCharP      -- parse the Char c
  return (read [c])   -- convert Char to Int
```

What is the result of 

```haskell
quiz1 = runParser digitIntP "92"
quiz2 = runParser digitIntP "cat"
```

|       | `quiz1`           | `quiz2`          | 
|------:|------------------:|-----------------:| 
| **A** | `[]`              | `[]`             |
| **B** | `[('9', "at")]`   | `[('c', "at")]`  |
| **C** | `[(9, "at")]`     | `[]`             |
| **D** | `[]`              | `[('c', "at")]`  |

<br>
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

Write a function 

```haskell 
strP :: String -> Parser String 
strP s = -- parses EXACTLY the String s and nothing else
```

when you are done, we should get the following behavior


```haskell
>>> dogeP = strP "doge"

>>> runParser dogeP "dogerel"
[("doge", "rel")]

>>> runParser dogeP "doggoneit"
[]
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

## QUIZ: A Choice Combinator

Lets write a combinator that **chooses** between two sub-parsers

```haskell
chooseP :: Parser a -> Parser a -> Parser a
chooseP p1 p2 = -- produce an 'a' 
                -- IF p1 OR p2 produce an 'a' 
```

`chooseP p1 p2` should _produce_ a succesful parse if `p1` _OR_ `p2` succeeds. 

e.g. `chooseP` lets us build a parser that produces an alphabet _OR_ a numeric character

```haskell
alphaNumChar :: Parser Char
alphaNumChar = chooseP alphaChar digitChar
```

Which should produce 

```haskell
>>> doParse alphaNumChar "cat"
[('c', "at")]

>>> doParse alphaNumChar "2cat"
[('2', "cat")]

>>> doParse alphaNumChar "230"
[('2', "30")]
```

```haskell
-- a 
chooseP p1 p2 = do xs <- p1
                   ys <- p2
                   return (x1 ++ x2) 
-- b
choose p1 p2  = do xs <- p1 
                   case xs of 
                     [] -> p2 
                     _  -> return xs

-- c
chooseP p1 p2 = P (\cs -> runParser p1 cs ++ runParser p2 cs)

-- d
chooseP p1 p2 = P (\cs -> case runParser p1 cs of
                            [] -> runParser p2 cs
                            rs -> rs)
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



## QUIZ 

Here's a parser that grabs the first `n` characters

```haskell
grabN :: Int -> Parser String
grabN n 
  | n <= 0    = return ""
  | otherwise = do {c <- oneChar; cs <- grabN (n-1); return (c:cs) }

grab2or4 = chooseP (grabN 2) (grabN 4)

quiz = runParser grab2or4 "mickeymouse"
```

**A.** `[]`

**B.** `[("mi","ckeymouse")]`

**C.** `[("mick","eymouse")]`

**D.** `[("mi","ckeymouse"),("mick","eymouse")]`

**E.** `[("mick","eymouse"), ("mi","ckeymouse")]`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Choice Combinator

Crucially if _both_ succeed, we end up with _all_ the results

```haskell
chooseP :: Parser a -> Parser a -> Parser a
p1 `chooseP` p2 = P (\cs -> doParse p1 cs ++ doParse p2 cs)
```

and only one result if thats possible

```haskell
>>> runParser grab2or4 "mic"
[("mi","c")]

>>> runParser grab2or4 "m"
[]
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

## A Simple Expression Parser

Even with the rudimentary parsers we have at our disposal, we can start
doing some rather interesting things. For example, here is a little
calculator. First, we parse the operation

\begin{code}
intOp      = plus `chooseP` minus `chooseP` times `chooseP` divide 
  where 
    plus   = char '+' >> return (+)
    minus  = char '-' >> return (-)
    times  = char '*' >> return (*)
    divide = char '/' >> return div
\end{code}

**DO IN CLASS** 
Can you guess the type of the above parser?


Next, we can parse the expression

\begin{code}
calc = do x  <- digitInt
          op <- intOp
          y  <- digitInt 
          return $ x `op` y
\end{code}

which, when run, will both parse and calculate

~~~~~{.haskell}
ghci> doParse calc "8/2"
[(4,"")]

ghci> doParse calc "8+2cat"
[(10,"cat")]

ghci> doParse calc "8/2cat"
[(4,"cat")]

ghci> doParse calc "8-2cat"
[(6,"cat")]

ghci> doParse calc "8*2cat"
[(16,"cat")]
~~~~~

[2]: http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf
[3]: http://www.haskell.org/haskellwiki/Parsec
[4]: http://www.cse.chalmers.se/~nad/publications/danielsson-parser-combinators.html
[5]: http://portal.acm.org/citation.cfm?doid=1706299.1706347
