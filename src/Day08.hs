{-# LANGUAGE ViewPatterns #-}

module Day08
    ( day08a
    , day08b
    )
where

import qualified Data.ByteString               as B
import           System.FilePath
import           Data.List
import           Data.Word8

type FlatImage = B.ByteString
type FlatLayer = B.ByteString
type Layer = [B.ByteString]

partitionToLayers :: FlatImage -> Int -> [FlatLayer]
partitionToLayers (B.length -> 0) _ = []
partitionToLayers bstr layerSize =
    B.take layerSize bstr : partitionToLayers (B.drop layerSize bstr) layerSize

partitionToImage :: B.ByteString -> (Int, Int) -> [Layer]
partitionToImage bstr (width, height) = map getRect layers
  where
    layers  = partitionToLayers bstr (width * height)
    getRect = (`partitionToLayers` width)

render :: [FlatLayer] -> FlatLayer
render []      = B.empty
render layers' = foldl comb x layers
  where
    layers@(x : _) = reverse layers'
    comb :: FlatLayer -> FlatLayer -> FlatLayer
    comb a b = B.pack (B.zipWith combPixel a b)
    combPixel :: Word8 -> Word8 -> Word8
    combPixel back front = if front == _2 then back else front

getLayers :: IO [FlatLayer]
getLayers = do
    text <- B.readFile $ "inputs" </> "day08_input.txt"
    let (width, height) = (25, 6)
    let layers = partitionToLayers text (width * height)
    return layers

day08a :: IO ()
day08a = do
    layers <- getLayers
    let minLayer        = minimumBy numZeros layers
    print minLayer
    print $ B.count _1 minLayer * B.count _2 minLayer
  where
    numZeros a b = zeros a `compare` zeros b
    zeros = B.count _0

day08b :: IO ()
day08b = do
    layers <- getLayers
    let width = 25
    let combLayer = render layers
    let img = partitionToLayers combLayer width
    print img
