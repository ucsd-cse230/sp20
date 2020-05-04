---
title: Haskell Crash Course Part III
headerImg: sea.jpg
---

## Writing Applications

Lets write the classic "Hello world!" program.

For example, in Python you may write:

```python
def main():
    print "hello, world!"

main()
```

and then you can run it:

```sh
$ python hello.py
hello world!
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


## Haskell is a **Pure** language. 

Not a _value_ judgment, but a precise _technical_ statement:

**The "Immutability Principle":**

- A function must _always_ return the same output for a given input

- A function's behavior should _never change_

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## No Side Effects

![](/static/img/trinity.png){#fig:types .align-center width=60%}

Haskell's most radical idea: `expression =*> value`

- When you evaluate an expression you get a value and 

- **Nothing else happens**

Specifically, evaluation must not have an **side effects**

- _change_ a global variable or

- _print_ to screen or

- _read_ a file or

- _send_ an email or

- _launch_ a missile.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## But... how to write "Hello, world!"

But, we **want** to ...

- **print** to screen
- **read** a file
- **send** an email

Thankfully, you _can_ do all the above via a very clever idea: `Recipe`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Recipes

[This analogy is due to Joachim Brietner][brietner]

Haskell has a special type called `IO` -- which you can think of as `Recipe` 

```haskell
type Recipe a = IO a
```

A _value_ of type `Recipe a`

- is a **description** of a *computation* that can have *side-effects* 

- which **when executed** performs some effectful I/O operations 

- to **produce** a value of type `a`.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Recipes have No Side Effects

A value of type `Recipe a` is

- A **description** of a computation that can have side-effects 

![Cake vs. Recipe](/static/img/cake.png){#fig:types .align-center width=60%}

**(L)** chocolate _cake_, **(R)** a _sequence of instructions_ on how to make a cake.

They are different (_Hint_: only one of them is delicious.)

Merely having a `Recipe Cake` has no effects! The recipe

- Does not make your oven _hot_

- Does not make your your floor _dirty_

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Only One Way to Execute Recipes 

Haskell looks for a special value

```haskell
main :: Recipe ()
```

The value associated with `main` is handed to the **runtime system and executed**

![Baker Aker](/static/img/baker-aker.jpg){#fig:types .align-center width=70%}

The Haskell runtime is a _master chef_ who is the only one allowed to cook!

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## How to write an App in Haskell

Make a `Recipe ()` that is handed off to the master chef `main`.

- `main` can be arbitrarily complicated

- composed of **smaller** sub-recipes

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>



## A Recipe to Print to Screen

```haskell
putStrLn :: String -> Recipe ()
```

The function `putStrLn`

- takes as input a `String`
- returns as output a `Recipe ()`

`putStrLn msg` is a `Recipe ()` 
- _when executed_ prints out `msg` on the screen.

```haskell
main :: Recipe ()
main = putStrLn "Hello, world!"
```

... and we can compile and run it

```sh
$ ghc --make hello.hs
$ ./hello
Hello, world!
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## QUIZ: How to Print Multiple Things?

Suppose I want to print _two_ things e.g.

```sh
$ ghc --make hello.hs
$ ./hello2
Hello! 
World!
```

Can we try to compile and run this:

```haskell
main = (putStLn "Hello!", putStrLn "World!")
```

**A.** Yes!

**B.** No, there is a type error!

**C.** No, it compiles but produces a different result!


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## A *Collection* of Recipes 

Is just ... a *collection* of Recipes!

```haskell
recPair :: (Recipe (), Recipe ())
recPair = (putStrLn "Hello!", putStrLn "World!")

recList :: [Recipe ()]
recList = [putStrLn "Hello!", putStrLn "World!"]
```

... we need a way to **combine** recipes!


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Combining? Just `do` it!

We can _combine_ many recipes into a single one using a `do` block

```haskell
foo :: Recipe a3
foo = do r1       -- r1 :: Recipe a1
         r2       -- r2 :: Recipe a2
         r3       -- r3 :: Recipe a3
```

(or if you *prefer* curly braces to indentation)

```haskell
foo = do { r1;    -- r1 :: Recipe a1
           r2;    -- r2 :: Recipe a2
           r3     -- r3 :: Recipe a3
         }
```

The `do` block combines sub-recipes `r1`, `r2` and `r3` into a *new* recipe that

- Will execute each sub-recipe in *sequence* and
- Return the value of type `a3` produced by the last recipe `r3`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Combining? Just `do` it!

So we can write

```haskell
main = do putStrLn "Hello!"
          putStrLn "World!"
```

or if you prefer

```haskell
main = do { putStrLn "Hello!"; 
            putStrLn "World!" 
          }
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## EXERCISE: Combining Many Recipes

Write a function called `sequence` that 

* Takes a _list_ of recipes `[r1,...,rn]` as input and
* Returns a _single_ recipe equivalent to `do {r1; ...; rn}`

```haskell
sequence :: [Recipe a] -> Recipe a
sequence rs = ???
```

When you are done you should see the following behavior

```haskell
-- Hello.hs

main = sequence [putStrLn "Hello!", putStrLn "World!"] 
```

and then 

```sh
$ ghc --make Hello.hs
$ ./hello
Hello! 
World!
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Using the Results of (Sub-) Recipes

Suppose we want a function that **asks** for the user's name

```sh
$ ./hello
What is your name? 
Ranjit             # <<<<< user enters
Hello Ranjit!
```

We can use the following sub-recipes

```haskell
-- | read and return a line from stdin as String
getLine  :: Recipe String       

-- take a string s, return a recipe that prints  s 
putStrLn :: String -> Recipe () 
```

But how to

- *Combine* the two sub-recipes while
- *Passing* the result of the first sub-recipe to the second.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Naming Recipe Results via "Assignment"

You can write

```haskell
x <- recipe
```

to *name* the result of executing `recipe`

- `x` can be used to refer to the result in _later_ code

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Naming Recipe Results via "Assignment"

Lets, write a function that *asks* for the user's name

```haskell 
main = ask 

ask :: Recipe ()
ask = do name <- getLine; 
         putStrLn ("Hello " ++ name ++ "!")
```

Which produces the desired result

```sh
$ ./hello
What is your name? 
Ranjit             # user enters
Hello Ranjit!
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## EXERCISE 

Modify the above code so that the program _repeatedly_ 
asks for the users's name _until_ they provide a _non-empty_ string.

```haskell
-- Hello.hs 

main = repeatAsk

repeatAsk :: Recipe ()
repeatAsk = _fill_this_in


isEmpty :: String -> Bool
isEmpty s = length s == 0
```

When you are done you should get the following behavior

```sh
$ ghc --make hello.hs

$ ./hello
What is your name? 
# user hits return
What is your name? 
# user hits return
What is your name? 
# user hits return
What is your name? 
Ranjit  # user enters
Hello Ranjit!
```

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## EXERCISE 

Modify your code to _also_ print out a **count** in the prompt

```sh
$ ghc --make hello.hs

$ ./hello
(0) What is your name? 
                          # user hits return
(1) What is your name? 
                          # user hits return
(2) What is your name? 
                          # user hits return
(3) What is your name? 
Ranjit                    # user enters
Hello Ranjit!
```

## That's all about IO

You should be able to implement `build` from `Directory.hs`

Using these library functions imported at the top of the file

```haskell
import System.FilePath   (takeDirectory, takeFileName, (</>))
import System.Directory  (doesFileExist, listDirectory)
```

The functions are

- `takeDirectory`
- `takeFileName` 
- `(</>)`
- `doesFileExist`
- `listDirectory`

`hoogle` the documentation to learn about how to use them.

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

<!-- 
## QUIZ: Combining Recipes

Next, lets write a program that prints multiple things:

```haskell
main :: IO ()
main = combine (putStrLn "Hello,") (putStrLn "World!")

-- putStrLn :: String -> Recipe ()
-- combine  :: ???
```

What must the _type_ of `combine` be?

```haskell
{- A -} combine :: ()        -> ()        -> ()
{- B -} combine :: Recipe    -> Recipe    -> Recipe ()
{- C -} combine :: Recipe () -> Recipe () -> Recipe () 
{- D -} combine :: Recipe a  -> Recipe a  -> Recipe a
{- E -} combine :: Recipe a  -> Recipe b  -> Recipe b
```

<br>
<br>
<br>
<br>

## Using Intermediate Results

Next, lets write a program that

1. **Asks** for the user's `name` using

```haskell
    getLine :: Recipe String
```

2. **Prints** out a greeting with that `name` using

```haskell
    putStrLn :: String -> Recipe ()
```

**Problem:** How to pass the **output** of _first_ recipe into the _second_ recipe?

<br>
<br>
<br>
<br>

## QUIZ: Using Yolks to Make Batter

Suppose you have two recipes

```haskell
crack     :: Recipe Yolk
eggBatter :: Yolk -> Recipe Batter
```

and we want to get 

```haskell
mkBatter :: Recipe Batter
mkBatter = crack `combineWithResult` eggBatter
```

What must the type of `combineWithResult` be?

```haskell
{- A -} Yolk -> Batter -> Batter
{- B -} Recipe Yolk -> (Yolk  -> Recipe Batter) -> Recipe Batter
{- C -} Recipe a    -> (a     -> Recipe a     ) -> Recipe a
{- D -} Recipe a    -> (a     -> Recipe b     ) -> Recipe b
{- E -} Recipe Yolk -> (Yolk  -> Recipe Batter) -> Recipe ()
```

<br>
<br>
<br>
<br>

## Looks Familar

Wait a bit, the signature looks familiar!

```haskell
combineWithResult :: Recipe a -> (a -> Recipe b) -> Recipe b
```

Remember this

```haskell
(>>=)             :: Result a -> (a -> Result b) -> Result b
```

## `Recipe` is an instance of `Monad`

In fact, in the standard library

```haskell
instance Monad Recipe where
  (>>=) = {-... combineWithResult... -}
```

So we can put this together with `putStrLn` to get:

```haskell
main :: Recipe ()
main = getLine >>= \name -> putStrLn ("Hello, " ++ name ++ "!")
```

or, using `do` notation the above becomes

```haskell
main :: Recipe ()
main = do name <- getLine
          putStrLn ("Hello, " ++ name ++ "!")
```

**Exercise** 

1. _Compile_ and run to make sure its ok!
2. _Modify_ the above to repeatedly ask for names.
3. _Extend_ the above to print a "prompt" that tells you how many iterations have occurred.

<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Monads are Amazing

Monads have had a _revolutionary_ influence in PL, well beyond Haskell, some recent examples

- **Error handling** in `go` e.g. [1](https://speakerdeck.com/rebeccaskinner/monadic-error-handling-in-go)  and [2](https://www.innoq.com/en/blog/golang-errors-monads/)

- **Asynchrony** in JavaScript e.g. [1](https://gist.github.com/MaiaVictor/bc0c02b6d1fbc7e3dbae838fb1376c80) and [2](https://medium.com/@dtipson/building-a-better-promise-3dd366f80c16)

- **Big data** pipelines e.g. [LinQ](https://www.microsoft.com/en-us/research/project/dryadlinq/) and [TensorFlow](https://www.tensorflow.org/)

<br>
<br>
<br>
<br>
<br>
<br>
<br>


## A Silly App to End CSE 130

Lets write an app called [moo](/static/raw/moo.hs) inspired by [cowsay](https://medium.com/@jasonrigden/cowsay-is-the-most-important-unix-like-command-ever-35abdbc22b7f)

**A Command Line App**

![`moo`](/static/img/moo1.png){#fig:types .align-center width=70%}

**`moo` works with pipes**

![Thanks, and good luck for the final!](/static/img/moo2.png){#fig:types .align-center width=70%}

![](/static/img/moo3.png){#fig:types .align-center width=70%}

```sh
$ ./moo Jhala, y u no make final easy!

 --------------------------------
< Jhala, y u no make final easy! >
 --------------------------------
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||
```

or even using unix pipes

```txt
$ ./moo Thats all folks, thanks!

 ------------------------------------
< 00-intro.pdf 01-lambda.pdf         >
< 03-datatypes.pdf 04-hof.pdf        >
< 05-environments.pdf 06-parsing.pdf >
< 07-classes.pdf 08-monads.pdf       >
 ------------------------------------
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||
```

Thats all, folks.

-->


[brietner]: https://www.seas.upenn.edu/~cis194/fall16/lectures/06-io-and-monads.html