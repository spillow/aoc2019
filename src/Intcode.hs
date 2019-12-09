module Intcode where

import           Data.List.Split
import           Text.Read
import           Control.Monad.Primitive        ( PrimState )
import qualified Data.Vector.Unboxed.Mutable   as M
import           Utils
import           Data.Digits

type Program = [Int]
type Inputs = [Int]
type Output = Int
type Outputs = [Output]
type ProgVec = M.MVector (PrimState IO) Int
type IP = Int
type Pause = Bool

data IntcodeState = IntcodeState { ip :: IP
                                 , inputs :: Inputs
                                 , output :: Output
                                 , relativeBase :: Int
                                 } deriving (Show)

mkInitState :: IntcodeState
mkInitState =
    IntcodeState { ip = 0, inputs = [], output = 0, relativeBase = 0 }

parseProgram :: String -> Either String Program
parseProgram s = mapM readEither (splitOn "," s)

execute :: ProgVec -> IntcodeState -> IO (Maybe IntcodeState)
execute vec = go
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
    procOp vec fn ip (mode1, mode2) = do
        x <- getParam vec (ip + 1) mode1
        y <- getParam vec (ip + 2) mode2
        writeOut vec (ip + 3) (fn x y)
    readInput vec ip (input : restInputs) = do
        writeOut vec (ip + 1) input
        return restInputs
    dispVal vec ip = getParam vec (ip + 1)
    jumpIfCond cond vec ip (mode1, mode2) = do
        num   <- getParam vec (ip + 1) mode1
        newIP <- getParam vec (ip + 2) mode2
        if cond num then return newIP else return $ ip + 3
    checkCond cond trueVal falseVal vec ip (mode1, mode2) = do
        x <- getParam vec (ip + 1) mode1
        y <- getParam vec (ip + 2) mode2
        let val = if cond x y then trueVal else falseVal
        writeOut vec (ip + 3) val
    go :: IntcodeState -> IO (Maybe IntcodeState)
    go state@IntcodeState { ip = ip, inputs = inputs } = do
        currNum <- M.read vec ip
        let m1 : m2 : _ = drop 2 (reverse $ digits 10 currNum) ++ repeat 0
        let opcode      = currNum `mod` 100
        case opcode of
            1 -> procOp vec (+) ip (m1, m2) >> go state { ip = ip + 4 }
            2 -> procOp vec (*) ip (m1, m2) >> go state { ip = ip + 4 }
            3 ->
                readInput vec ip inputs
                    >>= (\x -> go state { ip = ip + 2, inputs = x })
            4 -> dispVal vec ip m1 >>= handleOut
            -- jump-if-true x y => (x != 0) ? ip=y : ip+3
            5 ->
                jumpIfCond (/= 0) vec ip (m1, m2)
                    >>= (\x -> go $ state { ip = x })
            -- jump-if-false x y => (x == 0) ? ip=y : ip+3
            6 ->
                jumpIfCond (== 0) vec ip (m1, m2)
                    >>= (\x -> go $ state { ip = x })
            -- less than x y z => (x < y) ? z=1 : z=0
            7  -> checkCond (<) 1 0 vec ip (m1, m2) >> go state { ip = ip + 4 }
            -- equals x y z => (x == y) ? z=1 : z=0
            8  -> checkCond (==) 1 0 vec ip (m1, m2) >> go state { ip = ip + 4 }
            -- halt
            99 -> return Nothing
            _  -> error $ "unknown opcode!: " ++ show opcode
        where handleOut out = return $ Just state { ip = ip + 2, output = out }

-- given a program and a collection of initialization values
-- e.g., [(1,12),(2,2)] would do vec[1] = 12, vec[2] = 2,
-- return the value of address 'retAddr' at program termination
runProg :: Program -> [(Int, Int)] -> Inputs -> Int -> IO (Int, Outputs)
runProg prog inits inputs retAddr = do
    vec <- initVec prog
    initMemLocs vec inits
    states <- runWhileJust (mkInitState { inputs = inputs }) $ execute vec
    val    <- M.read vec retAddr
    return (val, [ out | IntcodeState { output = out } <- states ])
    where initMemLocs vec = mapM_ (uncurry (M.write vec))

runProgVec :: ProgVec -> IntcodeState -> [(Int, Int)] -> IO (Maybe IntcodeState)
runProgVec vec state inits = do
    initMemLocs vec inits
    execute vec state
    where initMemLocs vec = mapM_ (uncurry (M.write vec))

readAndProcessProg :: FilePath -> (Program -> IO ()) -> IO ()
readAndProcessProg path procFn = do
    text <- readFile path
    case parseProgram text of
        Left  x   -> print x
        Right ops -> procFn ops
