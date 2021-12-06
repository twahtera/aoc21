module Day6 where
import Data.List.Split
import Data.List.NonEmpty (group)
import qualified Data.List.NonEmpty as NEL
import Data.List (sort)
import Data.List.Utils (countElem)

getInput :: FilePath -> IO [Int]
getInput path = do
  fileStr <- readFile path
  let
    split = splitOn "," fileStr
  pure $ read <$> split

mkFishState :: [Int] -> [Int]
mkFishState fishList = map (`countElem` fishList) [0..8]

nextState :: [Int] -> [Int]
nextState state = updateAt 6 (+ head state) $ rotateList state

rotateList :: [a] -> [a]
rotateList [] = []
rotateList (x:xs) = xs ++ [x]

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt i f as = take i as ++ [f $ as !! i] ++ drop (i+1) as

main :: IO ()
main = do
  input <- getInput "./inputs/day6"
  let
    star1 = sum $ last $ take 81 $ iterate nextState $ mkFishState input
    star2 = sum $ last $ take 257 $ iterate nextState $ mkFishState input
  print star1
  print star2
