module Day11
    ( day11a
    , day11b
    )
where

import           System.FilePath
import           Intcode
import           Utils
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Int
import qualified Data.Vector.Mutable           as MV
import           Control.Monad.Primitive       (PrimState)
import           Control.Monad

type Point = (Int, Int)
type VecOff = (Int, Int)
type Color = Int64
type Turn = Int64
type ColorMap = M.Map Point Color
type ImgVec = MV.MVector (PrimState IO) (MV.MVector (PrimState IO) Int64)

black = 0
white = 1

turnLeft = 0 :: Turn
turnRight = 1 :: Turn

write2 :: ImgVec -> Int -> Int -> Int64 -> IO ()
write2 vec x y val = do
    vec' <- MV.read vec y
    MV.write vec' x val

read2 :: ImgVec -> Int -> Int -> IO Int64
read2 vec x y = do
    vec' <- MV.read vec y
    MV.read vec' x

initBackground :: Color -> Int -> Int -> IO ImgVec
initBackground color width height = do
    vec <- MV.new height
    rows <- replicateM height (MV.new width)
    mapM_ (insert vec) (zip [0 ..] rows)
    mapM_ fill rows
    return vec
    where insert vec (i, v) = MV.write vec i v
          fill vec = mapM_ (uncurry (MV.write vec)) (zip [0 ..] (replicate width color))

data Orientation = UpDir | DownDir | LeftDir | RightDir deriving (Eq)

getColor :: ColorMap -> Point -> Color
getColor map p = fromMaybe black (M.lookup p map)

addP :: Point -> VecOff -> Point
addP (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

getNewPos :: Point -> Orientation -> Turn -> (Point, Orientation)
getNewPos p o t = case o of
    UpDir    | t == turnLeft  -> (addP p (-1, 0), LeftDir)
             | t == turnRight -> (addP p (1, 0),  RightDir)
    DownDir  | t == turnLeft  -> (addP p (1, 0),  RightDir)
             | t == turnRight -> (addP p (-1, 0), LeftDir)
    LeftDir  | t == turnLeft  -> (addP p (0, -1), DownDir)
             | t == turnRight -> (addP p (0, 1),  UpDir)
    RightDir | t == turnLeft  -> (addP p (0, 1),  UpDir)
             | t == turnRight -> (addP p (0, -1), DownDir)

mapRun :: Program -> Color -> IO ColorMap
mapRun prog initColor = do
    let fatProg = prog ++ replicate 100000 0
    let initPos = (0, 0)
    vec <- initVec fatProg
    go (M.singleton initPos initColor) vec mkInitState initPos UpDir
  where
    go
        :: ColorMap
        -> ProgVec
        -> IntcodeState
        -> Point
        -> Orientation
        -> IO ColorMap
    go map vec state pos orientation = do
        let currColor = getColor map pos
        maybeState' <- execute vec state { inputs = [currColor] }
        case maybeState' of
            Nothing -> return map
            Just state'@IntcodeState { output = color } -> do
                (Just state''@IntcodeState { output = turn }) <- execute
                    vec
                    state'
                let (newPos, newOrient) = getNewPos pos orientation turn
                go (M.insert pos color map) vec state'' newPos newOrient

runner1 :: Program -> IO ()
runner1 prog = do
    colorMap <- mapRun prog black
    print $ M.size colorMap

render :: ColorMap -> IO ()
render colorMap = do
    let keys = M.keys colorMap
    let minX = minimum [x | (x, _) <- keys]
    let maxX = maximum [x | (x, _) <- keys]
    let minY = minimum [y | (_, y) <- keys]
    let maxY = maximum [y | (_, y) <- keys]
    let (width, height) = (maxX - minX + 1, maxY - minY + 1)
    imgVec <- initBackground black width height
    let (diffX, diffY) = (-minX, -minY)
    let offsets = [ ((x + diffX, y + diffY), v) | ((x, y), v) <- M.assocs colorMap ]
    mapM_ (\((x, y), v) -> write2 imgVec x y v) offsets
    forM_ (reverse [0 .. height - 1]) $ \y -> do
        forM_ [0 .. width - 1] $ \x ->
            emitVal imgVec x y
        print "\n"

emitVal vec x y = do
    val <- read2 vec x y
    if val == black
        then putStr "."
        else if val == white then putStr "x" else error "unknown val?"

runner2 :: Program -> IO ()
runner2 prog = do
    colorMap <- mapRun prog white
    render colorMap

day11a :: IO ()
day11a = do
    let path = "inputs" </> "day11_input.txt"
    readAndProcessProg path runner1

day11b :: IO ()
day11b = do
    let path = "inputs" </> "day11_input.txt"
    readAndProcessProg path runner2
