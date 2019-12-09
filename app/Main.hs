module Main where

import           System.Environment
import qualified Data.Map.Lazy                 as M
import           Data.Maybe

import           Day01
import           Day02
import           Day03
import           Day04
import           Day05
import           Day06
import           Day07
import           Day08

dayMap :: M.Map String (IO ())
dayMap = M.fromList
    [ ("day01a", day01a)
    , ("day01b", day01b)
    , ("day02a", day02a)
    , ("day02b", day02b)
    , ("day03a", day03a)
    , ("day03b", day03b)
    , ("day04a", day04a)
    , ("day04b", day04b)
    , ("day05a", day05a)
    , ("day05b", day05b)
    , ("day06a", day06a)
    , ("day06b", day06b)
    , ("day07a", day07a)
    , ("day07b", day07b)
    , ("day08a", day08a)
    , ("day08b", day08b)
    ]

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
        then putStrLn "Usage: aoc2019 <day>"
        else fromMaybe (putStrLn "not a known day!")
            $ M.lookup (head args) dayMap


