module Utils where

import           System.FilePath
import           System.IO
import           Text.Read
import qualified Data.Vector.Unboxed.Mutable   as M
import           Control.Monad.Primitive        ( PrimMonad
                                                , PrimState
                                                )

type ParseFn a = (String -> Either String a)

parseLines :: ParseFn a -> String -> Either String [a]
parseLines parse path = mapM parse (lines path)

parseIntegers :: String -> Either String [Integer]
parseIntegers = parseLines readEither

initVec :: (PrimMonad m, M.Unbox a) => [a] -> m (M.MVector (PrimState m) a)
initVec xs = do
    vec <- M.new (length xs)
    mapM_ (insert vec) (zip [0 ..] xs)
    return vec
    where insert vec (i, v) = M.write vec i v
