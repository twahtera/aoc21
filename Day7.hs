module Day7 where
import Data.List.Split
import Data.List (sort)
import Data.List.Extra (minimumOn)

getInput :: FilePath -> IO [Int]
getInput path = do
  fileStr <- readFile path
  pure $ read <$> splitOn "," fileStr

--getMedian :: [Int] -> Int
getMedian is = sort is !! (length is `div` 2)

--fuelCost :: [Int] -> Int
fuelCost is = sum $ map (abs . (-) (getMedian is)) is


--Star 2
getAverage :: [Int] -> Int
getAverage is = round $ fromIntegral (sum is) / fromIntegral (length is)

getFuelCost2 :: [Int] -> Int
getFuelCost2 is = sum $ map (getCost . f) is
  where
    avg = getAverage is
    getCost d = (d+1) * d `div` 2
    f i = abs $ i - 5

costWithMid' m is = sum $ map (getCost . f) is
  where
    getCost d = (d+1) * d `div` 2
    f i = abs $ i - m



getAverage' is = sum is / fromIntegral (length is)

getFuelCost2' :: [Double] -> Double
getFuelCost2' is = sum $ map (getCost . f) is
  where
    avg = getAverage' is
    getCost d = (d+1) * d / 2
    f i = abs $ i - 5

-- brutes
costWithMid m is = sum $ map (getCost . f) is
  where
    getCost d = (d+1) * d / 2
    f i = abs $ i - m

bruteMin :: [Int] -> Int
bruteMin ls = minimum $ (`costWithMid'` ls) <$> [1 .. length ls]

printCrap :: [Double] -> IO ()
printCrap l = do
  let
    avg = getAverage' l
    max = maximum l
    costs = flip costWithMid l <$> [1.. max]
    minCost = minimumOn snd $ zip [1..max] costs

  putStrLn $ "average: " <> show (getAverage' l)
  putStrLn $ "sum: " <> show (sum l)
  putStrLn $ "length: " <> show (length l)
  --putStrLn $ "sum divmod length: " <> show (sum l `divMod` fromIntegral length l)
  putStrLn $ "minCost: " <> show minCost
  print $ zip [1..max] costs

main = do
  input <- getInput "./inputs/day7"
  -- Star 1
  print $ fuelCost input
  -- Star 2
  print $ bruteMin input
