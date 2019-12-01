module Day01
    ( day01a
    , day01b
    )
where

import           System.FilePath
import           Utils

fuelCalc :: Integer -> Integer
fuelCalc x = (x `div` 3) - 2

calcTotal :: (Integer -> Integer) -> IO ()
calcTotal calcFn = do
    let path = "inputs" </> "day01_input.txt"
    text <- readFile path
    case parseIntegers text of
        Left  x    -> print x
        Right vals -> print $ sum (map calcFn vals)

day01a :: IO ()
day01a = calcTotal fuelCalc

day01b :: IO ()
day01b = calcTotal recFuelCalc
  where
    recFuelCalc x = if fuel < 0 then 0 else fuel + recFuelCalc fuel
        where fuel = fuelCalc x
