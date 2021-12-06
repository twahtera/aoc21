module Day3 where

import Debug.Trace (trace)

getInput :: FilePath -> IO ([[Int]], Int)
getInput path = do
  str <- readFile path
  let
    ls = lines str
  pure (readLine <$> ls, length ls)

  where
    readLine :: String -> [Int]
    readLine = map (read. pure)

not' :: Int -> Int
not' 0 = 1
not' _ = 0

bitFlip :: [Int] -> [Int]
bitFlip = map not'

-- kinda ill defined problem, what if there are as many?
getMostCommonBit :: [Int] -> Int
getMostCommonBit bs =
  if sum bs >= divmodsum
  then 1
  else 0
  where
    divmodsum = (uncurry (+)) $ divMod (length bs) 2

toDec :: [Int] -> Int
toDec bs = sum $ (\(a,b) -> a*2^b) <$> zip bs [length bs - 1, length bs -2 .. 0]

-- Unsafe, will blow up if following rows are shorter than the first
transpose :: [[a]] -> [[a]]
transpose rs = [[r !! i | r <- rs ]  | i <- [0..colNum - 1]]
  where
    colNum = length $ head rs

-- star 2 stuff
generatorRating :: ([Int] -> Int) -> [[Int]] -> [Int]
generatorRating f = generatorRating' f 0

generatorRating' :: ([Int] -> Int) -> Int -> [[Int]] -> [Int]
generatorRating' f i left
  | length left <= 1 = head left
  | otherwise = generatorRating' f (i+1) matching
    where
      colToConsider = transpose left !! i
      firstDigit = f colToConsider
      matching = filter ((== firstDigit) . (!! i)) left

main :: IO ()
main = do
  (input, lines) <- getInput "./inputs/day3"
  let
    cols = transpose input
    gammaBin = getMostCommonBit <$> cols
    gamma = toDec gammaBin
    epsilon = toDec $ bitFlip gammaBin

  putStrLn "star 1:"
  print gamma
  print epsilon
  print $ gamma*epsilon

  let
    o2gen = generatorRating getMostCommonBit input
    co2scrub = generatorRating (not' . getMostCommonBit) input
  putStrLn "star 2:"
  print $ toDec o2gen
  print $ toDec co2scrub
  print $ toDec o2gen * toDec co2scrub
