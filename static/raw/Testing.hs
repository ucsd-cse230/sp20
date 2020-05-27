
{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, TypeSynonymInstances #-}

module Testing where 

import Test.QuickCheck hiding ((===))
import Control.Monad
import Data.List
import qualified Data.Map as M 
import Control.Monad.State hiding (when)
import Control.Applicative ((<$>))
import Test.QuickCheck


incr :: Int -> Int
incr x = x + 1

prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs ++ ys) == reverse xs ++ reverse ys

-- >>> quickCheck prop_revapp
-- *** Failed! Falsifiable (after 6 tests and 9 shrinks):
-- [0]
-- [1]
--
prop_revapp' :: [Int] -> [Int] -> Bool
prop_revapp' xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

-- >>> quickCheckN 500 prop_revapp'
-- +++ OK, passed 500 tests.
--

quickCheckN n = quickCheckWith (stdArgs { maxSuccess = n } )


qsort        :: (Ord a) => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort ls ++ [x] ++ qsort rs
  where 
    ls       = [y | y <- xs, y < x]  -- elems in xs < x 
    rs       = [z | z <- xs, z > x]  -- elems in xs > x

ls :: [Int]
ls = [1,3..19] ++ [2,4..20]

-- >>> ls
-- [1,3,5,7,9,11,13,15,17,19,2,4,6,8,10,12,14,16,18,20]
-- >>> qsort ls
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]

isOrdered :: (Ord a) => [a] -> Bool
isOrdered (x:y:zs) = x <= y && isOrdered (y:zs)
isOrdered _        = True

prop_qsort_isOrdered :: [Int] -> Bool
prop_qsort_isOrdered xs = isOrdered (qsort xs)

-- >>> quickCheckN 1000 prop_qsort_isOrdered 
-- +++ OK, passed 1000 tests.
--


prop_qsort_min :: [Int] -> Bool
prop_qsort_min xs = head (qsort xs) == minimum xs

-- >>> quickCheck prop_qsort_min
-- *** Failed! Exception: 'Prelude.head: empty list' (after 1 test):
-- []
--

prop_qsort_nn_min    :: [Int] -> Property
prop_qsort_nn_min xs =
  not (null xs) ==> head (qsort xs) == minimum xs

-- >>> quickCheck prop_qsort_nn_min
-- +++ OK, passed 100 tests.
--

