module Utils where

import           System.FilePath
import           System.IO
import           Text.Read

type ParseFn a = (String -> Either String a)

parseLines :: ParseFn a -> String -> Either String [a]
parseLines parse path = mapM parse (lines path)

parseIntegers :: String -> Either String [Integer]
parseIntegers = parseLines readEither

