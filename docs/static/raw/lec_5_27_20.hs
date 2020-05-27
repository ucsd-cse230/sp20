{-# LANGUAGE DeriveFunctor #-}

module Lec_5_27_20 where

import Data.Char

-------------------------------------------------------------------------------
-- | A Type for Representing Parsers
-------------------------------------------------------------------------------

data Parser a = P (String -> [(a, String)])
  deriving (Functor) 

runParser :: Parser a -> String -> [(a, String)]
runParser (P f) s = f s

-------------------------------------------------------------------------------
-- | `Parser` is a `Monad`
-------------------------------------------------------------------------------

forEach :: [a] -> (a -> [b]) -> [b]
forEach []     f = []
forEach (x:xs) f = f x ++ forEach xs f 

example = forEach [1,2,3] (\n -> 
            forEach [0, 1, 2] (\m -> 
              [n * 100 + m]  
            )
          )

-- >>> example
-- [100,101,102,200,201,202,300,301,302]
--

retP :: a -> Parser a
retP x = P (\s -> [(x, s)])

bindP :: Parser a -> (a -> Parser b) -> Parser b
bindP pa f = P (\s ->
  forEach (runParser pa s) (\(va, s1) -> 
    forEach (runParser (f va) s1) (\(vb, s2) ->
      [(vb, s2)]
      ) 
    ) 
  )

instance Monad Parser where
  return = retP
  (>>=) = bindP

-------------------------------------------------------------------------------
-- | Combining Parsers
-------------------------------------------------------------------------------

oneChar :: Parser Char
oneChar = P (\cs -> case cs of
                      [] -> [] 
                      (c:cs') -> [(c, cs')])

pairP :: Parser a -> Parser b -> Parser (a, b)
pairP pa pb = do 
  va <- pa
  vb <- pb
  return (va, vb)

p3 :: Parser a -> Parser (a, a, a)
p3 p = do
  x1 <- p
  x2 <- p
  x3 <- p
  return (x1, x2, x3)

-- >>> runParser (p3 oneChar) "fr"
-- []
--
-- >>> runParser (pairP oneChar oneChar) "f"
-- []






























instance Applicative Parser where
  pure x    = P (\s -> [(x, s)])
  fP <*> vP = P (\s -> forEach (runParser fP s) (\(f, s') ->
                         forEach (runParser vP s') (\(v, s'') -> 
                            [(f v, s'')]
                         )
                       )
                )

