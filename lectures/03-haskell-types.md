---
title: Haskell Crash Course Part II
headerImg: sea.jpg
---

## Recap: Haskell Crash Course II

![](/static/img/trinity.png){#fig:types .align-center width=60%}

- Core program element is an **expression**
- Every _valid_ expression has a **type** (determined at compile-time)
- Every _valid_ expression reduces to a _value_ (computed at run-time)

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Recap: Haskell 

**Basic values & operators**

- `Int`, `Bool`, `Char`, `Double` 
- `+`, `-`, `==`, `/=`

**Execution / Function Calls**

- Just *substitute equals by equals*

**Producing Collections**

- Pack data into *tuples* & *lists*

**Consuming Collections**

- Unpack data via *pattern-matching*

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Next: Creating and Using New Data Types

1. `type` Synonyms: *Naming* existing types

2. `data` types: *Creating* new types

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Type Synonyms

Synonyms are just names ("aliases") for existing types

- think `typedef` in `C`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## A type to represent `Circle`

A tuple `(x, y, r)` is a *circle* with center at `(x, y)` and radius `r`

```haskell
type Circle = (Double, Double, Double)
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

## A type to represent `Cuboid`

A tuple `(length, depth, height)` is a *cuboid*

```haskell
type Cuboid = (Double, Double, Double)
```

![](https://upload.wikimedia.org/wikipedia/commons/d/dc/Cuboid.png){#fig:cuboid .align-center width=60%}


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Using Type Synonyms

We can now use synonyms by creating values of the given types

```haskell
circ0 :: Circle 
circ0 = (0, 0, 100)  -- ^ circle at "origin" with radius 100

cub0 :: Cuboid
cub0 = (10, 20, 30)  -- ^ cuboid with length=10, depth=20, height=30 
```

And we can write functions over synonyms too

```haskell
area :: Circle -> Double
area (x, y, r) = pi * r * r  

volume :: Cuboid -> Double
volume (l, d, h) = l * d * h 
```

We should get this behavior

```haskell
>>> area circ0 
31415.926535897932

>>> volume cub0
6000
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

Suppose we have the definitions

```haskell
type Circle = (Double, Double, Double)
type Cuboid = (Double, Double, Double)

circ0 :: Circle 
circ0 = (0, 0, 100)  -- ^ circle at "origin" with radius 100

cub0 :: Cuboid
cub0 = (10, 20, 30)  -- ^ cuboid with length=10, depth=20, height=30 

area :: Circle -> Double
area (x, y, r) = pi * r * r  

volume :: Cuboid -> Double
volume (l, d, h) = l * d * h
```

What is the result of

```haskell
>>> volume circ0
```

**A.** `0`

**B.** Type error

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Beware!

Type Synonyms 

* Do not _create_ new types

* Just _name_ existing types

And hence, synonyms 

* Do not prevent _confusing_ different values 

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


## Creating *New* Data Types 

We can avoid mixing up by creating _new_ `data` types

```haskell
-- | A new type `CircleT` with constructor `MkCircle`
data CircleT = MkCircle Double Double Double     

-- | A new type `CuboidT` with constructor `MkCuboid`
data CuboidT = MkCuboid Double Double Double
```

### Constructors are the **only way** to create values

- `MkCircle` creates `CircleT`

- `MkCuboid` creates `CuboidT`

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

Suppose we create a new type with a `data` definition

```haskell
-- | A new type `CircleT` with constructor `MkCircle`
data CircleT = MkCircle Double Double Double     
```

What is the **type of** the `MkCircle` _constructor_?

**A.** `MkCircle :: CircleT`

**B.** `MkCircle :: Double -> CircleT`

**C.** `MkCircle :: Double -> Double -> CircleT`

**D.** `MkCircle :: Double -> Double -> Double -> CircleT`

**E.** `MkCircle :: (Double, Double, Double) -> CircleT`

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Constructing Data

Constructors let us *build* values of the new type 

```haskell
circ1 :: CircleT 
circ1 = MkCircle 0 0 100  -- ^ circle at "origin" w/ radius 100

cub1 :: Cuboid
cub1 = MkCuboid 10 20 30  -- ^ cuboid w/ len=10, dep=20, ht=30 
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

Suppose we have the definitions

```haskell
data CuboidT = MkCuboid Double Double Double     

type Cuboid  = (Double, Double, Double)

volume :: Cuboid -> Double
volume (l, d, h) = l * d * h
```

What is the result of

```haskell
>>> volume (MkCuboid 10 20 30)
```

**A.** `6000`

**B.** Type error

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Deconstructing Data

Constructors let us *build* values of new type ... but how to *use* those values?

How can we implement a function

```haskell
volume :: Cuboid -> Double
volume c = ???
```

such that

```haskell
>>> volume (MkCuboid 10 20 30)
6000
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


## Deconstructing Data by Pattern Matching

Haskell lets us *deconstruct* data via pattern-matching

```haskell
volume :: Cuboid -> Double
volume c = case c of
             MkCuboid l d h -> l * d * h
```

`case e of Ctor x y z -> e1` is read as as 

**IF** 
  - `e` evaluates to a value that *matches the pattern* `Ctor vx vy vz` 

**THEN** 
  - evaluate `e1` after naming `x := vx`, `y := vy`, `z := vz` 

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## Pattern matching on Function Inputs

Very common to do matching on function inputs

```haskell
volume :: Cuboid -> Double
volume c = case c of 
            MkCuboid l d h -> l * d * h

area :: Circle -> Double
area a  = case a of 
            MkCircle x y r -> pi * r * r
```

So Haskell allows a nicer syntax: *patterns in the arguments*

```haskell
volume :: Cuboid -> Double
volume (MkCuboid l d h) = l * d * h

area :: Circle -> Double
area (MkCircle x y r) = pi * r * r
```

Nice syntax *plus* the compiler saves us from *mixing up* values! 

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

## But ... what if we _need_ to mix up values?

Suppose I need to represent a *list of shapes*

- Some `Circle`s
- Some `Cuboid`s

What is the problem with `shapes` as defined below?

```haskell
shapes = [circ1, cub1]
```

Where we have defined

```haskell
circ1 :: CircleT 
circ1 = MkCircle 0 0 100  -- ^ circle at "origin" with radius 100

cub1 :: Cuboid
cub1 = MkCuboid 10 20 30  -- ^ cuboid with length=10, depth=20, height=30 
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

## Problem: All list elements must have the _same_ type

**Solution???**

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>




## QUIZ: Variant (aka Union) Types

Lets create a _single_ type that can represent _both_ kinds of shapes!

<br>

```haskell
data Shape 
  = MkCircle Double Double Double   -- ^ Circle at x, y with radius r 
  | MkCuboid Double Double Double   -- ^ Cuboid with length, depth, height
```

<br>

What is the type of `MkCircle 0 0 100` ? 

**A.** `Shape`

**B.** `Circle`

**C.** `(Double, Double, Double)`

<br>
<br>
<br>
<br>
<br>
<br>

## Each *Data Constructor* of `Shape` has a different type 

When we define a data type like the below

```haskell
data Shape 
  = MkCircle  Double Double Double   -- ^ Circle at x, y with radius r 
  | MkCuboid  Double Double Double   -- ^ Cuboid with length, depth, height
```

We get *multiple constructors* for `Shape`

```haskell
MkCircle :: Double -> Double -> Double -> Shape
MkCuboid :: Double -> Double -> Double -> Shape
```

## Now we can create collections of `Shape`

Now we can define

```haskell
circ2 :: Shape
circ2 = MkCircle 0 0 100  -- ^ circle at "origin" with radius 100

cub2 :: Shape 
cub2 = MkCuboid 10 20 30  -- ^ cuboid with length=10, depth=20, height=30 
```

and then define collections of `Shape`s

```haskell
shapes :: [Shape]
shapes = [circ1, cub1]
```

<br>
<br>
<br>
<br>
<br>
<br>

## EXERCISE 

Lets define a type for 2D shapes

```haskell
data Shape2D 
  = MkRect Double Double -- ^ 'MkRect w h' is a rectangle with width 'w', height 'h'
  | MkCirc Double        -- ^ 'MkCirc r' is a circle with radius 'r'
  | MkPoly [Vertex]      -- ^ 'MkPoly [v1,...,vn]' is a polygon with vertices at 'v1...vn'

type Vertex = (Double, Double)
```

Write a function to compute the `area` of a `Shape2D` 

```haskell
area2D :: Shape2D -> Double
area2D s = ???
```

**HINT**

![Area of a polygon](/static/img/area-polygon.png){#fig:polygon .align-center width=60%}

You may want to use this helper that computes the area of a triangle at `v1`, `v2`, `v3`

```haskell
areaTriangle :: Vertex -> Vertex -> Vertex -> Double
areaTriangle v1 v2 v3 = sqrt (s * (s - s1) * (s - s2) * (s - s3))
  where 
      s  = (s1 + s2 + s3) / 2
      s1 = distance v1 v2 
      s2 = distance v2 v3
      s3 = distance v3 v1

distance :: Vertex -> Vertex -> Double
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)
```
