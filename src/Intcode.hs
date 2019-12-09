module Intcode where

import           Data.List.Split
import           Text.Read
import           Control.Monad.Primitive        ( PrimState )
import qualified Data.Vector.Unboxed.Mutable   as M
import           Utils
import           Data.Digits
import           Data.Int
import           Debug.Trace

type Program = [Int64]
type Inputs = [Int64]
type Output = Int64
type Outputs = [Output]
type ProgVec = M.MVector (PrimState IO) Int64
type IP = Int
type MemType = Int64
type Mode = Int64

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
    getParam :: ProgVec -> Int -> IntcodeState -> Mode -> IO MemType
    getParam vec loc IntcodeState { relativeBase = relBase } mode = do
        x <- M.read vec loc
        case mode of
            -- position mode
            0 -> M.read vec (fromIntegral x)
            -- immediate mode
            1 -> return x
            -- relative mode
            2 -> M.read vec $ fromIntegral x + relBase
    setParam :: ProgVec -> Int -> MemType -> IntcodeState -> Mode -> IO ()
    setParam vec slot val IntcodeState{relativeBase = relBase} mode = do
        addr <- M.read vec slot
        case mode of
            -- position mode
            0 -> M.write vec (fromIntegral addr) val
            -- immediate mode
            1 -> error "immediate not allowed during set!"
            -- relative mode
            2 -> M.write vec (fromIntegral addr + relBase) val
    procOp vec fn state@IntcodeState { ip = ip } (mode1, mode2, mode3) = do
        x <- getParam vec (ip + 1) state mode1
        y <- getParam vec (ip + 2) state mode2
        setParam vec (ip + 3) (fn x y) state mode3
    readInput vec ip (input : restInputs) state mode = do
        setParam vec (ip + 1) input state mode
        return restInputs
    dispVal vec ip = getParam vec (ip + 1)
    jumpIfCond cond vec state@IntcodeState { ip = ip } (mode1, mode2) = do
        num   <- getParam vec (ip + 1) state mode1
        newIP <- getParam vec (ip + 2) state mode2
        if cond num then return (fromIntegral newIP) else return $ ip + 3
    checkCond cond trueVal falseVal vec state@IntcodeState { ip = ip } (mode1, mode2, mode3)
        = do
            x <- getParam vec (ip + 1) state mode1
            y <- getParam vec (ip + 2) state mode2
            let val = if cond x y then trueVal else falseVal
            setParam vec (ip + 3) val state mode3
    updateRelativeBase vec state@IntcodeState {ip = ip, relativeBase = relBase} mode = do
        x <- getParam vec (ip + 1) state mode
        return state{ip = ip + 2, relativeBase = relBase + fromIntegral x}
    go :: IntcodeState -> IO (Maybe IntcodeState)
    go state@IntcodeState { ip = ip, inputs = inputs } = do
        --let _ = trace $ "state: " ++ show state
        currNum <- M.read vec (fromIntegral ip)
        let m1 : m2 : m3 : _ = drop 2 (reverse $ digits 10 currNum) ++ repeat 0
        let opcode      = currNum `mod` 100
        case opcode of
            1 -> procOp vec (+) state (m1, m2, m3) >> go state { ip = ip + 4 }
            2 -> procOp vec (*) state (m1, m2, m3) >> go state { ip = ip + 4 }
            3 ->
                readInput vec ip inputs state m1
                    >>= (\x -> go state { ip = ip + 2, inputs = x })
            4 -> dispVal vec ip state m1 >>= handleOut
            -- jump-if-true x y => (x != 0) ? ip=y : ip+3
            5 ->
                jumpIfCond (/= 0) vec state (m1, m2)
                    >>= (\x -> go $ state { ip = x })
            -- jump-if-false x y => (x == 0) ? ip=y : ip+3
            6 ->
                jumpIfCond (== 0) vec state (m1, m2)
                    >>= (\x -> go $ state { ip = x })
            -- less than x y z => (x < y) ? z=1 : z=0
            7 -> checkCond (<) 1 0 vec state (m1, m2, m3)
                >> go state { ip = ip + 4 }
            -- equals x y z => (x == y) ? z=1 : z=0
            8 -> checkCond (==) 1 0 vec state (m1, m2, m3)
                >> go state { ip = ip + 4 }
            -- adjust relative base
            9 -> updateRelativeBase vec state m1 >>= go
            -- halt
            99 -> return Nothing
            _  -> error $ "unknown opcode!: " ++ show opcode
        where handleOut out = return $ Just state { ip = ip + 2, output = out }

-- given a program and a collection of initialization values
-- e.g., [(1,12),(2,2)] would do vec[1] = 12, vec[2] = 2,
-- return the value of address 'retAddr' at program termination
runProg :: Program -> [(Int, MemType)] -> Inputs -> Int -> IO (MemType, Outputs)
runProg prog inits inputs retAddr = do
    vec <- initVec prog
    initMemLocs vec inits
    states <- runWhileJust (mkInitState { inputs = inputs }) $ execute vec
    val    <- M.read vec retAddr
    return (val, [ out | IntcodeState { output = out } <- states ])
    where initMemLocs vec = mapM_ (uncurry (M.write vec))

runProg' :: Program -> Inputs -> IO Outputs
runProg' prog inputs = do
    (_, outputs) <- runProg prog [] inputs 0
    return outputs

runProgVec
    :: ProgVec -> IntcodeState -> [(Int, MemType)] -> IO (Maybe IntcodeState)
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
