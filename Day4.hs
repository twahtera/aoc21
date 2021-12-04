module Day4 (main) where

import Day3 (transpose)
import Data.List.Split
import Debug.Trace (trace)
import Data.List (find)

getInput :: FilePath -> IO ([Int], [[[Int]]])
getInput path = do
  fileStr <- readFile path

  let
    ls = lines fileStr

    numbers = read <$> splitOn "," (head ls)

    boardLines = drop 2 ls
    boardStrs = splitOn [""] boardLines

    slnt = (map . map) words boardStrs
    bslnt = (map . map . map) read slnt

  pure (numbers, bslnt)

win :: [[Int]] -> [Int] -> Bool
win board nums = any rowFull board || any rowFull boardT
  where
    boardT = transpose board
    rowFull r = and $ flip elem nums <$> r

score :: [[Int]] -> [Int] -> Int
score board nums = last nums * sum (filter (`notElem` nums) $ concat board)

play :: [[[Int]]] -> [Int] -> Int -> Int
play boards nums round =
  case winner of
    Just w -> score w curNums
    Nothing -> play boards nums (round+1)
  where
    curNums = take round nums
    winner = find (`win` curNums) boards

-- star 2
playToLose :: [[[Int]]] -> [Int] -> Int -> Int
playToLose boards nums round =
  case loser of
    Just l -> play [l] nums 1
    Nothing -> playToLose boards nums (round+1)
  where
    curNums = take round nums
    winners = filter (`win` curNums) boards
    loserExists = length winners == (length boards - 1)
    loser =
      if loserExists
      then find (not . (`win` curNums)) boards
      else Nothing

main :: IO ()
main = do
  (nums, boards) <- getInput "./inputs/day4"
  let
    winnerScore = play boards nums 1
    loserScore = playToLose boards nums 1
  print winnerScore
  print loserScore
