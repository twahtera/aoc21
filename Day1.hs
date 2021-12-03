module Day1 (main) where

f :: [Integer] -> [Bool]
f (a:b:tail) = (a<b) : f (b:tail)
f _ = []


g :: [Integer] -> [Bool]
g (i:is) = uncurry (<) <$> zip (i:is) is
g _ = []

getInput :: FilePath -> IO [Integer]
getInput filename = do
  fileStr <- readFile filename
  return $ read <$> lines fileStr

window3 :: [Integer] -> [Integer]
window3 (a:b:c:tail) = (a+b+c) : window3 (b:c:tail)
window3 _ = []

main = do
  input <- getInput "./inputs/day1"

  putStrLn $ show $ length $ filter id $ f input
  putStrLn $ show $ length $ filter id $ g input

  putStrLn $ show $ length $ filter id $ f $ window3 input
  putStrLn $ show $ length $ filter id $ g $ window3 input
