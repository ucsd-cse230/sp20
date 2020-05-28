
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

prop_qsort_sort    :: [Int] -> Bool
prop_qsort_sort xs =  qsort xs == sort xs

-- >>> quickCheck prop_qsort_sort
-- *** Failed! Falsifiable (after 6 tests and 3 shrinks):
-- [-3,-3]

noDuplicates ::(Eq a) => [a] -> Bool
noDuplicates (x:xs) = not (x `elem` xs) && noDuplicates xs
noDuplicates _      = True

prop_qsort_distinct :: [Int] -> Bool 
prop_qsort_distinct xs = noDuplicates (qsort xs)  

-- >>> quickCheck prop_qsort_distinct
-- +++ OK, passed 100 tests.
--

prop_qsort_distinct_sort :: [Int] -> Property 
prop_qsort_distinct_sort xs = (noDuplicates xs) ==> (qsort xs == sort xs)

-- >>> quickCheck prop_qsort_distinct_sort
-- +++ OK, passed 10000 tests.
--
-- >>> quickCheck prop_qsort_sort
-- *** Failed! Falsifiable (after 6 tests and 2 shrinks):
-- [5,5]
-- >>> quickCheck prop_qsort_sort
-- *** Failed! Falsifiable (after 4 tests and 1 shrink):
-- [1,1]
--


-- >>> sample' (choose (0, 5))
-- [4,2,5,3,2,2,2,3,0,0,0]
--

pos = choose (0, 100)

posPair = do
  x1 <- pos
  x2 <- pos
  return (x1, x2)

-- >>> sample' posPair
-- [(29,71),(48,74),(89,53),(73,93),(0,40),(71,35),(23,69),(93,49),(59,58),(27,32),(88,45)]
--

oneOf :: [Gen a] -> Gen a
oneOf gs = do
  g <- elements gs
  x <- g
  return x

-- >>> sample' (oneOf [choose (0,2), choose (10,12)])
-- [2,2,1,1,12,10,2,2,11,0,11]
--

randomThings :: (Arbitrary a) => IO [a]
randomThings = sample' arbitrary

-- >>> randomThings :: IO [[Int]]
-- [[],[],[-4,-1],[5,1,0,3,4,3],[0,8,2,3,8,4,3],[-6,-1,8,1,-5,-10,7,2],[-10,11,-6,-6,-12],[5,13,-8,-14,0,-1,-14,-9,10,-10,12,0,14,-4],[-5,8,-9,-12],[-8,-3,-2,12,7,-1,6,3,17,-14,12],[]]
--
-- >>> randomThings :: IO [Bool]
-- [True,True,False,True,False,True,True,True,True,True,True]
--

-- >>> randomThings :: IO [String]
-- ["","\a","\f","\779257W\SUBA","\84573","D\ACK\365059S","9W\554735G","g\SYN~W\62120\&4&[","\NULsc\18427fy(","Q`TI \n/TH","\461027\ESCZ`u\783094\&4B\SOHT\424692"]
--


-- >>> randomThings :: IO [(Int, Bool)] 
-- [(0,True),(1,True),(0,True),(6,False),(-5,True),(4,False),(-12,False),(-8,False),(5,False),(-9,False),(-7,False)]
--

