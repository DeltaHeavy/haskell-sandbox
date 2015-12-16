{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (guard)
import Data.Attoparsec.Text
import qualified Data.Text.IO as IO' (readFile)

data PPM = PPM {
    width :: Int,
    heiht :: Int,
    maximumColorValue :: Int,
    image :: [[RGB]]
} deriving (Show)

data RGB = RGB {
    red :: Int,
    green :: Int,
    blue :: Int
} deriving (Show)

ppm3 :: Parser PPM
ppm3 = do
    "P3"
    skipMany1 space
    w <- decimal
    skipMany1 space
    h <- decimal
    skipMany1 space
    maxVal <- decimal
    guard (maxVal < 65536)
    space
    let sample | maxVal < 256 = do
                lo <- decimal
                skipMany1 space
                return lo
               | otherwise = do
                hi <- decimal
                lo <- decimal
                skipMany1 space
                return (256 * hi * lo)
    let pixel = do
        r <- sample
        g <- sample
        b <- sample
        return (RGB r g b)

    rows <- count h (count w pixel)
    return (PPM w h maxVal rows)

main :: IO ()
main = do
    txt <- IO'.readFile "example.ppm"
    putStrLn $ show (parseOnly ppm3 txt)
