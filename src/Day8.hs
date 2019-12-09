module Main (main) where

import Common (inputSingle)
import Data.Foldable (minimumBy)
import Data.List (intercalate, transpose)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)

imgWidth :: Int
imgWidth = 25

imgHeight :: Int
imgHeight = 6

imgSize :: Int
imgSize = imgWidth * imgHeight

main :: IO ()
main = do
  img <- parseImg <$> inputSingle 8

  print (part1 img)
  putStrLn (part2 img)

part1 :: Image -> Int
part1 img = ones * twos
  where
  leastZerosLayer = minimumBy (comparing (length . filter (== Black))) img
  ones = length (filter (== White) leastZerosLayer)
  twos = length (filter (== Transparent) leastZerosLayer)

part2 :: Image -> String
part2 = imgToText . render

parseImg :: String -> Image
parseImg = chunksOf imgSize . map parsePixel

parsePixel :: Char -> Pixel
parsePixel '0' = Black
parsePixel '1' = White
parsePixel '2' = Transparent
parsePixel c   = error ("unknown pixel: " <> show c)

type Image = [Layer]
type Layer = [Pixel]

data Pixel = Black | White | Transparent
  deriving (Eq, Ord, Show)

render :: Image -> [Pixel]
render = map (head . filter (/= Transparent)) . transpose

imgToText :: [Pixel] -> String
imgToText = intercalate "\n" . chunksOf imgWidth . map pixelToText
  where
  pixelToText :: Pixel -> Char
  pixelToText Black = ' '
  pixelToText White = 'X'
  pixelToText Transparent = ' '
