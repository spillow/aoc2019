module Day10
  ( day10a
  , day10b
  )
where

import           Data.Vector                    ( Vector
                                                , (!)
                                                , (!?)
                                                , fromList
                                                )
import qualified Data.Vector                   as V
import           System.FilePath
import           Data.List

type FieldVec = Vector (Vector Char)

getFrac :: (Int, Int) -> (Int, Int)
getFrac (n, d) = (n `div` d', d `div` d') where d' = gcd n d

readVal :: FieldVec -> Int -> Int -> Char
readVal vec x y = (vec ! y) ! x

getFieldInfo :: IO (FieldVec, [(Int, Int)], (Int, Int))
getFieldInfo = do
  let path = "inputs" </> "day10_input.txt"
  text <- readFile path
  let field        = fromList $ map fromList (lines text)
  let (rows, cols) = (V.length field, V.length $ field ! 0)
  let asteroids =
        [ (x, y)
        | y <- [0 .. rows - 1]
        , x <- [0 .. cols - 1]
        , readVal field x y == '#'
        ]
  return (field, asteroids, (rows, cols))

day10a :: IO ()
day10a = do
  (field, asteroids, _) <- getFieldInfo
  let counts = [ (p, countAsteroids field asteroids p) | p <- asteroids ]
  print $ maximumBy fn counts
  where fn (_, c1) (_, c2) = c1 `compare` c2

countAsteroids :: FieldVec -> [(Int, Int)] -> (Int, Int) -> Int
countAsteroids field asteroids p@(x1, y1) = length $ findIndices hits asteroids
 where
  hits p2@(x2, y2) = (p /= p2) && walkTill p diff p2
   where
    improper@(dx', dy') = (x2 - x1, y2 - y1)
    diff                = getFrac improper
  walkTill (x1, y1) diff@(dx, dy) p2@(x2, y2)
    | (x3, y3) == p2             = True
    | readVal field x3 y3 == '#' = False
    | otherwise                  = walkTill (x3, y3) diff p2
    where (x3, y3) = (x1 + dx, y1 + dy)

start@(startx, starty) = (19, 14)

day10b :: IO ()
day10b = do
  (_, asteroids, _) <- getFieldInfo
  let right = sortBy cmpFn
        $ filter (\(x, y) -> x >= startx && (x, y) /= start) asteroids
  let left = sortBy cmpFn $ filter (\(x, y) -> x < startx) asteroids
  let clock    = groupThem right ++ groupThem left
  let ordering = concat $ transpose clock
  let (px, py) = ordering !! 199
  print $ 100 * px + py
 where
  groupThem l = map (sortBy dist) $ groupBy
    (\(x1, y1) (x2, y2) ->
      getFrac (x1 - startx, y1 - starty) == getFrac (x2 - startx, y2 - starty)
    )
    l
   where
    dist p1 p2 = man p1 `compare` man p2
    man (x, y) = abs (x - startx) + abs (y - starty)
  cmpFn :: (Int, Int) -> (Int, Int) -> Ordering
  cmpFn (x1', y1') (x2', y2') =
    (fromIntegral y1 / fromIntegral x1)
      `compare` (fromIntegral y2 / fromIntegral x2)
   where
    (x1, y1) = getFrac (x1' - startx, y1' - starty)
    (x2, y2) = getFrac (x2' - startx, y2' - starty)
