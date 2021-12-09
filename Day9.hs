module Day9 where
import Data.List.Extra (splitOn)
import qualified Data.Array as A
import Debug.Trace (trace)
import Data.List (nub, sort)

getInput :: FilePath -> IO [[Int]]
getInput path = do
  fileStr <- readFile path
  let
    ls = lines fileStr
  pure $ map ( read . (:[])) <$> ls


pointsAround :: (Int, Int) -> [(Int, Int)]
pointsAround (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

valsAround :: [[Int]] -> (Int, Int) -> [Int]
valsAround map p = valAt map <$> pointsAround p

valAt :: [[Int]] -> (Int, Int) -> Int
valAt m (x,y) = if outside then 10 else m !! y !! x
  where
    w = length $ head m
    h = length m
    outside = x < 0 || x >= w || y < 0 || y >= h

-- Star 1
lowPoint :: [[Int]] -> (Int, Int) -> Bool
lowPoint m p = all (valAt m p <) $ valsAround m p

lowPoints :: [[Int]] -> [(Int, Int)]
lowPoints m = filter (lowPoint m) allPoints
  where
    allPoints = [(x,y) | x <- [0..w-1], y <- [0..h-1]]
    w = length $ head m
    h = length m

solve1 :: [[Int]] -> Int
solve1 m = sum $ (+1) . valAt m <$> lowPoints m

-- Star 2
higherNeighs :: [[Int]] -> (Int, Int) -> [(Int, Int)]
higherNeighs m p = filter higherBasinVal (pointsAround p)
  where
    higherBasinVal p' = valAt m p' < 9 && valAt m p' > valAt m p

flowOneUp :: [[Int]] -> [(Int, Int)] -> [(Int, Int)]
flowOneUp m ps = nub $ concat $ ps : (higherNeighs m <$> ps)

flowUp :: [[Int]] -> [(Int, Int)] -> [(Int, Int)]
flowUp m ps =
  if ps == next then ps
  else flowUp m next

  where next = flowOneUp m ps

solve2 :: [[Int]] -> Int
solve2 m = product $ take 3 $ reverse $ sort $ length <$> basins
  where
    basins = flowUp m . (:[]) <$> lowPoints m

main :: IO ()
main = do
  input <- getInput "./inputs/day9"
  print $ solve1 input
  print $ solve2 input
