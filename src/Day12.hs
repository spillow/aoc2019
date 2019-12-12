module Day12
    ( day12a
    , day12b
    )
where

import qualified Data.Set                      as S
import           Data.List

type Point = (Int, Int, Int)
type Velocity = (Int, Int, Int)

data Moon = Moon { pos :: Point, vel :: Velocity } deriving (Eq, Ord, Show)
data MoonDim = MoonDim { pos' :: Integer, vel' :: Integer } deriving (Eq, Ord)

class (Ord a) => Object a where
    updateVelocity :: a -> a -> a
    updatePosition :: a -> a
    potEnergy      :: a -> Integer
    kinEnergy      :: a -> Integer

instance Object Moon where
    updateVelocity m1@Moon { pos = (x, y, z), vel = vel1 } Moon { pos = (x', y', z') }
        = m1 { vel = vel1 `add3` velOff }
        where velOff = (x `cmpVal` x', y `cmpVal` y', z `cmpVal` z')
    updatePosition m1@Moon { pos = pos, vel = vel } =
        m1 { pos = pos `add3` vel }
    potEnergy Moon { pos = (x, y, z) } = toInteger $ abs x + abs y + abs z
    kinEnergy Moon { vel = (x, y, z) } = toInteger $ abs x + abs y + abs z

instance Object MoonDim where
    updateVelocity m1@MoonDim { pos' = x, vel' = vel1 } MoonDim { pos' = x' } =
        m1 { vel' = vel1 + velOff }
        where velOff = x `cmpVal` x'
    updatePosition m1@MoonDim { pos' = pos, vel' = vel } =
        m1 { pos' = pos + vel }
    potEnergy MoonDim { pos' = x } = toInteger $ abs x
    kinEnergy MoonDim { vel' = x } = toInteger $ abs x

mkMoon :: Point -> Moon
mkMoon p = Moon { pos = p, vel = (0, 0, 0) }

mkMoon' :: Integer -> MoonDim
mkMoon' p = MoonDim { pos' = p, vel' = 0 }

cmpVal :: (Integral a) => a -> a -> a
cmpVal a b | a > b     = -1
           | a < b     = 1
           | otherwise = 0

add3 :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
add3 (x, y, z) (x', y', z') = (x + x', y + y', z + z')

runStep :: (Object a) => a -> [a] -> a
runStep m = updatePosition . foldl' updateVelocity m

totEnergy :: (Object a) => a -> Integer
totEnergy m = potEnergy m * kinEnergy m

positions :: [Point]
positions = [(-4, 3, 15), (-11, -10, 13), (2, 2, 18), (7, -1, 0)]

testPositions :: [Point]
testPositions = [(-8, -10, 0), (5, 5, 10), (2, -7, 3), (9, -8, -3)]

updateMoons :: (Object a) => [a] -> [a]
updateMoons ms = map (`runStep` ms) ms

day12a :: IO ()
day12a = print $ sum $ map totEnergy $ iterate updateMoons moons !! 1000
    where moons = map mkMoon positions

cycleLength :: (Object a) => [a] -> Integer
cycleLength moons = go S.empty moons 0
  where
    go s moons cnt = if S.member moons s
        then cnt
        else go (S.insert moons s) (updateMoons moons) (cnt + 1)

day12b :: IO ()
day12b = print $ lcm (lcm lenX lenY) lenZ
  where
    moonsX = map (mkMoon' . toInteger) [ x | (x, _, _) <- positions ]
    moonsY = map (mkMoon' . toInteger) [ y | (_, y, _) <- positions ]
    moonsZ = map (mkMoon' . toInteger) [ z | (_, _, z) <- positions ]
    lenX   = cycleLength moonsX
    lenY   = cycleLength moonsY
    lenZ   = cycleLength moonsZ
