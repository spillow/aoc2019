module Day05(day05a, day05b, day05atest) where

import           System.FilePath
import           Intcode
import           Control.Monad

noMemInit = []

acUnitID = [1]
thermRadID = [5]

day05a :: IO ()
day05a = readAndProcessProg ("inputs" </> "day05_input.txt") run
  where
    run prog = void $ runProg prog noMemInit acUnitID 0

day05atest :: Int -> IO ()
day05atest idx = do
    text <- readFile ("inputs" </> "day05_input.txt")
    let (Right prog) = parseProgram text
    print (prog !! idx)

day05b :: IO ()
day05b = readAndProcessProg ("inputs" </> "day05_input.txt") run
  where
    run prog = void $ runProg prog noMemInit thermRadID 0