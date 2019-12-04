module Day04
    ( day04a
    , day04b
    )
where

-- https://adventofcode.com/2019/day/4

import           Data.Digits
import           Data.List

nodown :: [Int] -> Bool
nodown l@(_ : xs) = all (uncurry (<=)) $ zip l xs

goodPass :: Int -> Bool
goodPass pass = numSeq digs && nodown digs
  where
    digs = digits 10 pass
    numSeq l@(_ : xs) = any (uncurry (==)) $ zip l xs

goodPass2 :: Int -> Bool
goodPass2 pass = numSeq digs && nodown digs
  where
    digs = digits 10 pass
    numSeq l@(_ : xs) = any (\x -> length x == 2) $ group l

day04a :: IO ()
day04a = print $ length (filter goodPass [125730 .. 579381])

day04b :: IO ()
day04b = print $ length (filter goodPass2 [125730 .. 579381])
