-- DO NOT MODIFY THIS FILE

import qualified Assignment2

-- if the following line causes an error, type (at the terminal prompt):
--   cabal update
--   cabal install QuickCheck

import Test.QuickCheck -- see https://hackage.haskell.org/package/QuickCheck for
                       -- documentation if you want to write your own tests

-- Tests

-- Exercise 1
-- no automated tests

-- Exercise 2

_ = Assignment2.sumSquareDiff :: Integer -> Integer

sumSquareDiff :: Integer -> Integer
sumSquareDiff n =
  (sum [1..n])^2 - sum [ x^2 | x <- [1..n] ]

prop_Exercise2 n = Assignment2.sumSquareDiff n == sumSquareDiff n

-- Exercise 3

_ = Assignment2.isMatch :: String -> String -> Bool

isMatch :: String -> String -> Bool
isMatch s       ('*' : p) = isMatch s p || (not $ null s) && isMatch (tail s) ('*' : p)
isMatch (_ : s) ('?' : p) = isMatch s p
isMatch (c : s) (d : p)   = c==d && isMatch s p
isMatch "" "" = True
isMatch _ _ = False

prop_Exercise3 s p = Assignment2.isMatch s p == isMatch s p

-- main

main = do
  putStrLn "Exercise 1: (no automated tests)"
  putStrLn "Exercise 2:"
  quickCheck prop_Exercise2
  putStrLn "Exercise 3:"
  quickCheck prop_Exercise3
