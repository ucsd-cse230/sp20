module Lec_4_22_20 where

firstElem :: a -> [a] -> a
firstElem d []      = d 
firstElem d (x : _) = x


secondElem :: a -> [a] -> a
secondElem d []      = d
secondElem d (_:[])  = d
secondElem d (_:x:_) = x

