module Day09
    ( day09a
    , day09b
    )
where

import Intcode
import System.FilePath

runWithInput :: MemType -> Program -> IO ()
runWithInput input = run
    where run prog = runProg' fatProg [input] >>= print
            where fatProg = prog ++ replicate 100000 0

day09a :: IO ()
day09a = readAndProcessProg ("inputs" </> "day09_input.txt") $ runWithInput 1

day09b :: IO ()
day09b = readAndProcessProg ("inputs" </> "day09_input.txt") $ runWithInput 2
