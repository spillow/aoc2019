module Day02
    ( day02a
    , day02b
    )
where

-- https://adventofcode.com/2019/day/2

import qualified Data.Vector.Unboxed.Mutable   as M
import           Utils
import           Data.List.Split
import           Text.Read
import           System.FilePath
import           Control.Monad.Primitive        ( PrimState )
import           Control.Monad
import           Data.List
import           Intcode

noinput = []

day02a :: IO ()
day02a = readAndProcessProg ("inputs" </> "day02_input.txt") run
  where
    run prog = do
        (pos0,_) <- runProg prog [(1, 12), (2, 2)] noinput 0
        print pos0

day02b :: IO ()
day02b = readAndProcessProg ("inputs" </> "day02_input.txt") run
  where
    run prog = do
        result <- exec prog
        case result of
            Nothing              -> error "Not found!"
            Just (noun, verb, _) -> print $ 100 * noun + verb
    exec prog = do
        let vals = [ (noun, verb) | noun <- [0 .. 99], verb <- [0 .. 99] ]
        results <- forM vals $ \(noun, verb) -> do
            (pos0,_) <- runProg prog [(1, noun), (2, verb)] noinput 0
            return (noun, verb, pos0)
        return $ find (\(_, _, result) -> result == 19690720) results
