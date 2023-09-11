-- DO NOT MODIFY THIS FILE

import qualified Assignment2

-- if the following line causes an error, type (at the terminal prompt):
--   cabal update
--   cabal install QuickCheck

import Test.QuickCheck -- see https://hackage.haskell.org/package/QuickCheck for
                       -- documentation if you want to write your own tests

import Control.Monad(liftM)

import qualified Data.List

-- Tests

-- Exercise 1
-- no automated tests

-- Exercise 2

prop_Exercise2 = not Assignment2.answer21 && Assignment2.answer22 && not Assignment2.answer23 && Assignment2.answer24 && not Assignment2.answer25

-- Exercise 3

_ = Assignment2.sumSquareDiff :: Integer -> Integer

sumSquareDiff :: Integer -> Integer
sumSquareDiff n =
  (sum [1..n])^2 - sum [ x^2 | x <- [1..n] ]

prop_Exercise3 n = n < 1 || Assignment2.sumSquareDiff n == sumSquareDiff n

-- Exercise 4

_ = Assignment2.inter :: [Integer] -> [Integer] -> [Integer]

inter :: [Integer] -> [Integer] -> [Integer]
inter = Data.List.intersect

prop_Exercise4 s1 s2 =
  let s1' = Data.List.nub s1
      s2' = Data.List.nub s2
  in
    Data.List.sort (Assignment2.inter s1' s2') == Data.List.sort (inter s1' s2')

-- Exercise 5

_ = Assignment2.isMatch :: String -> String -> Bool

isMatch :: String -> String -> Bool
isMatch s       ('*' : p) = isMatch s p || (not $ null s) && isMatch (tail s) ('*' : p)
isMatch (_ : s) ('?' : p) = isMatch s p
isMatch (c : s) (d : p)   = c==d && isMatch s p
isMatch "" "" = True
isMatch _ _ = False

newtype Exercise5String = Exercise5String String

instance Show Exercise5String where
  show (Exercise5String s) = show s

instance Arbitrary Exercise5String where
  arbitrary = liftM Exercise5String $ frequency
                                        [ (1, arbitrary)
                                        , (9, listOf $ elements ['a','b','c','*','?'])
                                        ]

prop_Exercise5 (Exercise5String s) (Exercise5String p) = Assignment2.isMatch s p == isMatch s p

-- main

main = do
  putStrLn "Exercise 1: (no automated tests)"
  putStrLn "Exercise 2:"
  quickCheck prop_Exercise2
  putStrLn "Exercise 3:"
  quickCheck prop_Exercise3
  putStrLn "Exercise 4:"
  quickCheck prop_Exercise4
  putStrLn "Exercise 5:"
  quickCheck (withMaxSuccess 1000 prop_Exercise5)
