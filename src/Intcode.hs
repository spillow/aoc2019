module Intcode where

import           Data.List.Split
import           Text.Read
import           Control.Monad.Primitive        ( PrimState )
import qualified Data.Vector.Unboxed.Mutable   as M
import           Utils
import           Data.Digits

type Program = [Int]
type Inputs = [Int]
type ProgVec = M.MVector (PrimState IO) Int

parseProgram :: String -> Either String Program
parseProgram s = mapM readEither (splitOn "," s)

execute :: ProgVec -> Inputs -> IO ()
execute vec = go 0
  where
    getParam :: ProgVec -> Int -> Int -> IO Int
    getParam vec loc mode = do
        x <- M.read vec loc
        case mode of
            -- position mode
            0 -> M.read vec x
            -- immediate mode
            1 -> return x
    writeOut vec slot val = do
        addr <- M.read vec slot
        M.write vec addr val
    procOp vec fn ip (mode1,mode2) = do
        x <- getParam vec (ip + 1) mode1
        y <- getParam vec (ip + 2) mode2
        writeOut vec (ip + 3) (fn x y)
    readInput vec ip input mode = writeOut vec (ip + 1) input
    dispVal vec ip mode = do
        x <- getParam vec (ip + 1) mode
        print x
    jumpIfCond cond vec ip (mode1,mode2) = do
        num <- getParam vec (ip + 1) mode1
        newIP <- getParam vec (ip + 2) mode2
        if cond num then
            return newIP
        else
            return $ ip + 3
    checkCond cond trueVal falseVal vec ip (mode1,mode2) = do
        x <- getParam vec (ip + 1) mode1
        y <- getParam vec (ip + 2) mode2
        let val = if cond x y then trueVal else falseVal
        writeOut vec (ip + 3) val
    go :: Int -> Inputs -> IO ()
    go ip inputs = do
        currNum <- M.read vec ip
        let m1:m2:_ = drop 2 (reverse $ digits 10 currNum) ++ repeat 0
        let opcode = currNum `mod` 100
        let (currInput : restInputs) = inputs
        case opcode of
            1  -> procOp vec (+) ip (m1,m2)        >> go' (ip + 4)
            2  -> procOp vec (*) ip (m1,m2)        >> go' (ip + 4)
            3  -> readInput vec ip currInput m1    >> go  (ip + 2) restInputs
            4  -> dispVal   vec ip           m1    >> go' (ip + 2)
            -- jump-if-true x y => (x != 0) ? ip=y : ip+3
            5  -> jumpIfCond (/= 0) vec ip (m1,m2) >>= go'
            -- jump-if-false x y => (x == 0) ? ip=y : ip+3
            6  -> jumpIfCond (== 0) vec ip (m1,m2) >>= go'
            -- less than x y z => (x < y) ? z=1 : z=0
            7  -> checkCond (<) 1 0 vec ip (m1,m2) >> go' (ip + 4)
            -- equals x y z => (x == y) ? z=1 : z=0
            8  -> checkCond (==) 1 0 vec ip (m1,m2) >> go' (ip + 4)
            -- halt
            99 -> return ()
            _  -> error $ "unknown opcode!: " ++ show opcode
        where go' = flip go inputs

-- given a program and a collection of initialization values
-- e.g., [(1,12),(2,2)] would do vec[1] = 12, vec[2] = 2,
-- return the value of address 'retAddr' at program termination
runProg :: Program -> [(Int, Int)] -> Inputs -> Int -> IO Int
runProg prog inits inputs retAddr = do
    vec <- initVec prog
    initMemLocs vec inits
    execute vec inputs
    M.read vec retAddr
    where initMemLocs vec = mapM_ (uncurry (M.write vec))

readAndProcessProg :: FilePath -> (Program -> IO ()) -> IO ()
readAndProcessProg path procFn = do
    text <- readFile path
    case parseProgram text of
        Left  x   -> print x
        Right ops -> procFn ops
