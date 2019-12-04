module Day03
    ( day03a
    , day03b
    )
where

import           Data.List.Split
import           Text.Read
import qualified Data.Set                      as S
import           Utils
import           System.FilePath
import           Data.Either
import           Data.Foldable
import           Data.List (elemIndex)
import           Data.Maybe

type Point = (Int, Int)
type Path = [Point]

origin = (0,0)

parseWireDescL :: String -> Path
parseWireDescL s = fst $ foldl parse ([origin], origin) (splitOn "," s)
  where
    parse :: (Path, Point) -> String -> (Path, Point)
    parse (s, (x,y)) str = case str of
        'U' : xs -> (s ++ [ (x, w) | w <- [y+1 .. y + read xs]], (x, y + read xs))
        'D' : xs -> (s ++ [ (x, w) | w <- [y-1,y-2 .. y - read xs]], (x, y - read xs))
        'L' : xs -> (s ++ [ (w, y) | w <- [x-1,x-2 .. x - read xs]], (x - read xs, y))
        'R' : xs -> (s ++ [ (w, y) | w <- [x+1 .. x + read xs]], (x + read xs, y))
        _ -> error "unknown direction!"

parseWireDesc :: String -> S.Set Point
parseWireDesc = S.fromList . parseWireDescL

manhattanDist :: Point -> Point -> Int
manhattanDist (x, y) (z, w) = abs (x - z) + abs (y - w)

day03a :: IO ()
day03a = do
    let path = "inputs" </> "day03_input.txt"
    text <- readFile path
    let (Right [first, second]) = parseLines (Right . parseWireDesc) text
    let intersections = S.delete origin $ S.intersection first second
    print $ manhattanDist origin (minimumBy ord intersections)
    where ord p1 p2 = manhattanDist origin p1 `compare` manhattanDist origin p2

circuitDelay :: Path -> Point -> Int
circuitDelay path point = fromJust $ elemIndex point path

day03b :: IO ()
day03b = do
    let path = "inputs" </> "day03_input.txt"
    text <- readFile path
    let (Right [first, second]) = parseLines (Right . parseWireDescL) text
    let intersections = S.delete origin $ S.intersection (S.fromList first) (S.fromList second)
    let minDelayIntersection = minimumBy (ord first second) intersections
    print $ circuitDelay first minDelayIntersection + circuitDelay second minDelayIntersection
    where ord first second p1 p2 = delay p1 `compare` delay p2
            where delay p = circuitDelay first p + circuitDelay second p
