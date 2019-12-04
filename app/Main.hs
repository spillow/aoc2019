module Main where

import           System.Environment
import qualified Data.Map.Lazy                 as M
import           Data.Maybe

import           Day01
import           Day02
import           Day03

dayMap :: M.Map String (IO ())
dayMap = M.fromList
    [ ("day01a", day01a)
    , ("day01b", day01b)
    , ("day02a", day02a)
    , ("day02b", day02b)
    , ("day03a", day03a)
    , ("day03b", day03b)
    ]

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
        then putStrLn "Usage: aoc2019 <day>"
        else fromMaybe (putStrLn "not a known day!")
            $ M.lookup (head args) dayMap


