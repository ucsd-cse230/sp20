---
title: Lecture Notes
headerImg: books.jpg
---

## Videos

This class will have zoom lectures with live coding. 

The lectures will be recorded and available on [CANVAS](https://canvas.ucsd.edu/courses/12823). 

## Topics, Notes and Code

| **Date**   | **Topic**                       | **Notes**                 | **Nodes**          |
|:----------:|:--------------------------------|:--------------------------|:-------------------|
| *3/30*     | Intro                           | [pdf][00-intro]           |                    |
| *3/30*     | Lambda Calculus                 | [html][00-lambda]         | [pdf][01-lambda-A] |
| *4/3*      | ""                              |                           | [pdf][01-lambda-B] |
| *4/6*      | ""                              |                           | [pdf][01-lambda-C] |
|            | From LC to Haskell              | [pdf][01-haskell]         |               |
|            | Higher-Order Functions          | [html][02-hofs]           |               | 
|            | Polymorphism & Typeclasses      | [html][03-classes]        |               |
|            | Functors & Applicatives         | [html][04-functors]       |               | 
|            | Monads                          | [html][05-monads]         |               |
|            | Parser Combinators              | [html][06-parsers]        |               |
|            | Testing                         | [html][07-testing]        |               | 
|            | Concurrency & Parallelism       | [html][08-parconc]        |               | 
|            | Types & Inference               | [html][09-types]          |               |            
|            | Refinement Types                | [html][10-refinements]    |               |          
|            | Proofs as Programs              | [html][11-proofs]         |               | 



<!-- 
## Topics 

Week 1
- Intro
- LC 

Week 2
- Haskell 101
- HOFs

Week 3
- Type-Classes 
- Ord, Monoid, Functor

Week 4
- Applicative
- Monads (Maybe, Either, State, List)

Week 5
- Parser combinators
- Testing

Week 6
- Parallelism
- Concurrency 

Week 7
- Verification & Refinement Types

Week 8
- Termination
- Proofs


## Assignments 

- TIC-TAC-TOE
  * AI 
  
- SNAKE 
  * 1-Player (AI)
  * 2-Player (AI)
  * Server

230-sp20-00-lambda
230-sp20-01-shapes
230-sp20-02-while 
230-sp20-03-bst
230-sp20-04-liquid




00-lambda 
00 - (elsa from CSE 130)
01 - Haskell (shapes, fractal-gloss, recursion, file-find-IO)
02 - (from wi16 - skip BST)
03 - (from wi16 + full BST - circuit)
04 - (from wi16 + BST-BAL? + get-set-kv-list-proof?)

TODO
   - snake AI in 01?

projects
   - yesod/grader
   - yesod/photo-share
   - ghcjs/invaders
   - ghcjs/pong
   - brick/invaders
   - brick/pong
   - ???



**Date**     **Topic**                                          **Notes**
--------     ----------------------------------------------     --------------------------
   *4/1*     Introduction & The Lambda Calculus                 [pdf][pdf-intro] [pdf][pdf-lambda] [notes][notes1]
   *1/7*     From LC to Haskell                                 [pdf][pdf-haskell]  [hs][hs1]
  *1/14*     Higher-Order Functions                             [html][lec2]  [hs][lhs2]
  *1/21*     Polymorphism & Typeclasses                         [html-A][lec3] [html-B][lec4]  [hs-B][lhs3] [hs-B][lhs4] 
  *1/28*     Functors                                           [html][lec7]  [hs][lhs7]
  *1/22*     Monads                                             [html][lec7]  [hs][lhs7]
  *2/10*     Parser Combinators                                 [html][lec9]  [hs][lhs9] 
  *2/17*     Testing                                            [html][lec10] [hs][lhs10]
  *2/19*     Concurrency                                        [pdf][pdf13]  [hs][lhs13] [html][lec13]
  *2/24*     Lambda Calculus                                    [pdf][pdf14]
   *3/3*     Hindley-Milner                                     [html][lec15] [hs][lhs15]
   *3/5*     Refinement Types                                   [html][lecLH] [pdf][pdfLH] [VM](https://piazza.com/class/i4kkvjdaoqj7aj?cid=11)

----------------------------------------------------------------------------------
-->

[00-intro]: static/raw/lec-intro.pdf
[00-lambda]: lectures/01-lambda.html
[01-haskell]: static/raw/lec-haskell.pdf
[01-lambda-A]: static/raw/01-lambda-A.pdf
[01-lambda-B]: static/raw/01-lambda-B.pdf
[01-lambda-C]: static/raw/01-lambda-C.pdf

[02-hofs]: lectures/00-intro.html
[03-classes]: lectures/00-intro.html
[04-functors]: lectures/00-intro.html
[05-monads]: lectures/00-intro.html
[06-parsers]: lectures/00-intro.html
[07-testing]: lectures/00-intro.html
[08-parconc]: lectures/00-intro.html
[09-types]: lectures/00-intro.html
[10-refinements]: lectures/00-intro.html
[11-proofs]: lectures/00-intro.html 


[pdf-intro]: static/lec-intro-2x2.pdf 
[pdf-lambda]: static/lec-lambda-2x2.pdf
[pdf-haskell]: static/lec-haskell-2x2.pdf

[notes1]: https://piazza.com/class/ij0wjmlgp4r1gp?cid=7
[hs1]:  static/lec-intro.hs 
[lhs1]: static/lec-intro.lhs

[lec2]: lectures/lec-higher-order-1.html
[lhs2]: lectures/lec-higher-order-1.lhs
[lec2s]: slides/lec-higher-order.lhs.slides.html

[lec3]: lectures/lec-higher-order-2.html
[lhs3]: lectures/lec-higher-order-2.lhs
[lec3s]: slides/lec-polymorphism.lhs.slides.html

[lec4]: lectures/lec-typeclasses.html
[lhs4]: lectures/lec-typeclasses.lhs

[lec7]: lectures/lec-monads.html
[lhs7]: lectures/lec-monads.lhs

[lec9]: lectures/lec-parsers.html
[lhs9]: lectures/lec-parsers.lhs

[lec10]: lectures/lec-quickcheck.html
[lhs10]: lectures/lec-quickcheck.lhs

[pdf13]: static/lec-stm-2x2.pdf
[lec13]: lectures/lec-stm.html
[lhs13]: lectures/lec-stm.lhs


[lec15]: lectures/lec-inference.html
[lhs15]: lectures/lec-inference.lhs

[lecLH]: http://ucsd-progsys.github.io/liquidhaskell-tutorial/
[pdfLH]: http://ucsd-progsys.github.io/liquidhaskell-tutorial/book.pdf


<!--
[lec11]: lectures/lec-transformers.html
[lhs11]: lectures/lec-transformers.lhs

[lec5]: lectures/lec-animation.html
[lhs5]: lectures/lec-animation.lhs

[lec6]: lectures/lec-reactive.html
[lhs6]: lectures/lec-reactive.lhs

[lec12]: slides/lec-parallel.markdown.slides.html
[cod12]: https://github.com/ranjitjhala/par-tutorial
-->







