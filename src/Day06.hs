module Day06
    ( day06a
    , day06b
    )
where

-- https://adventofcode.com/2019/day/6

import qualified Data.Map                      as M
import           Data.List.Split
import           System.FilePath
import           Utils
import           Data.Maybe
import           Data.Either

type Node = String

type OrbitMap = M.Map Node [Node]

children :: Node -> OrbitMap -> [Node]
children root orbitMap = fromMaybe [] (M.lookup root orbitMap)

-- find the nearest common ancestor of two nodes
nca :: OrbitMap -> Node -> Node -> Node -> Maybe Node
nca orbitMap root n1 n2 = case go root of
    Left  _ -> Nothing
    Right x -> Just x
  where
    go :: Node -> Either (Maybe Node) Node
    go root | (root == n1 || root == n2) && foundNode = Right root
            | root == n1 || root == n2 = Left (Just root)
            | foundNCA                 = Right $ head (rights walked)
            | length foundNodes == 2   = Right root
            | foundNode                = Left $ head foundNodes
            | otherwise                = Left Nothing
      where
        walked     = map go (children root orbitMap)
        foundNodes = filter isJust $ lefts walked
        foundNode  = not $ null foundNodes
        foundNCA   = not $ null (rights walked)

depth :: OrbitMap -> Node -> Node -> Maybe Int
depth orbitMap root n = go 0 root
  where
    go depth' root = if root == n
        then Just depth'
        else case filter isJust $ map (go (depth' + 1)) children' of
            []      -> Nothing
            (x : _) -> x
        where children' = children root orbitMap

getOrbits :: String -> [(Node, [Node])]
getOrbits s = fromRight (error "parse issue?") $ parseLines parse s
    where parse s = Right (a, [b]) where [a, b] = splitOn ")" s

checksum :: Node -> OrbitMap -> Int
checksum node orbitMap = go 0 node
  where
    go :: Int -> Node -> Int
    go depth node = depth + sum (map (go (depth + 1)) children')
        where children' = children node orbitMap

day06a :: IO ()
day06a = do
    text <- readFile ("inputs" </> "day06_input.txt")
    let orbitMap = M.fromListWith (++) $ getOrbits text
    print $ checksum "COM" orbitMap

day06b :: IO ()
day06b = do
    text <- readFile ("inputs" </> "day06_input.txt")
    let orbitMap = M.fromListWith (++) $ getOrbits text
    let nca'     = fromMaybe (error "no nca?") $ nca orbitMap "COM" "YOU" "SAN"
    print $ "nca is: " ++ nca'
    let youDepth =
            fromMaybe (error "YOU not found?") $ depth orbitMap nca' "YOU"
    let sanDepth =
            fromMaybe (error "SAN not found?") $ depth orbitMap nca' "SAN"
    print $ "YOU depth: " ++ show youDepth
    print $ "SAN depth: " ++ show sanDepth
    print $ youDepth + sanDepth - 2
