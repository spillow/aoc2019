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

type Program = [Int]

parseProgram :: String -> Either String Program
parseProgram s = mapM readEither (splitOn "," s)

execute :: M.MVector (PrimState IO) Int -> IO ()
execute vec = go 0
  where
    procOp vec fn ip = do
        ip1 <- M.read vec (ip + 1)
        ip2 <- M.read vec (ip + 2)
        ip3 <- M.read vec (ip + 3)
        x   <- M.read vec ip1
        y   <- M.read vec ip2
        M.write vec ip3 (fn x y)
    go :: Int -> IO ()
    go ip = do
        opcode <- M.read vec ip
        case opcode of
            1  -> procOp vec (+) ip >> go (ip + 4)
            2  -> procOp vec (*) ip >> go (ip + 4)
            99 -> return ()
            _  -> error "unknown opcode!"

-- given a program and a collection of initialization values
-- e.g., [(1,12),(2,2)] would do vec[1] = 12, vec[2] = 2,
-- return the value of address 'n' at program termination
runProg :: Program -> [(Int, Int)] -> Int -> IO Int
runProg prog inits retAddr = do
    vec <- initVec prog
    initMemLocs vec inits
    execute vec
    M.read vec retAddr
    where initMemLocs vec = mapM_ (uncurry (M.write vec))

readAndProcessProg :: FilePath -> (Program -> IO ()) -> IO ()
readAndProcessProg path procFn = do
    text <- readFile path
    case parseProgram text of
        Left  x   -> print x
        Right ops -> procFn ops

day02a :: IO ()
day02a = readAndProcessProg ("inputs" </> "day02_input.txt") run
  where
    run prog = do
        pos0 <- runProg prog [(1, 12), (2, 2)] 0
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
            pos0 <- runProg prog [(1, noun), (2, verb)] 0
            return (noun, verb, pos0)
        return $ find (\(_, _, result) -> result == 19690720) results
