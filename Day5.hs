module Day5 where

import Data.List.Split (splitOn)
import qualified Data.MultiSet as MS
import Data.List (delete)

getInput :: FilePath -> IO [((Int, Int), (Int, Int))]
getInput path = do
  fileStr <- readFile path
  let
    ls = lines fileStr
  pure $ readLine <$> ls

readLine :: String -> ((Int, Int), (Int, Int))
readLine str = tupleFromList points
  where
    splitStr = splitOn " -> " str
    readPoint s = tupleFromList $ read <$> splitOn "," s
    points = readPoint <$> splitStr

-- Will blow up
tupleFromList [a,b] = (a,b)

linePoints :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
linePoints ((a1, a2), (b1, b2))
  -- horizontal
  | a1 == b1 = [(a1, x) | x <- [min a2 b2.. max a2 b2]]
  -- vertical
  | a2 == b2 = [(x, a2) | x <- [min a1 b1.. max a1 b1]]
  -- diagonal down
  | a1 - b1 == a2 - b2 =
    [(min a1 b1 + i, min a2 b2 +i) | i <- [0..abs $ a1 - b1]]
  -- diagonal up
  | a1 - b1 == - (a2 - b2) =
    [(min a1 b1 + i, max a2 b2 - i) | i <- [0..abs $ a1 - b1]]
  | otherwise = []

findMultiples :: Eq a => Ord a => [a] -> [a]
findMultiples points = fst <$> filter (\(p, occ) -> occ > 1) occurs
  where
    occurs = MS.toOccurList $ MS.fromList points

main :: IO ()
main = do
  lineEnds <- getInput "./inputs/day5"

  let
    allPoints = concat [linePoints ends | ends <- lineEnds]
    multipleCount = length $ findMultiples allPoints
  print multipleCount
