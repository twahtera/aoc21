{-# LANGUAGE LambdaCase #-}

module Day2 (main) where

import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Data.Maybe (fromJust)
import GHC.List (foldl')

data Movement = Forward Integer
              | Down Integer
              | Up Integer

getInput :: FilePath -> IO [Movement]
getInput path = do
  fileStr <- readFile path
  -- Just crash on invalid input
  pure $ fromJust <$> parseMovement <$> lines fileStr

parseMovement :: String -> Maybe Movement
parseMovement str =
  case spl of
    (dirStr : amount : [])
      | dirStr == "forward" -> Forward <$> readMaybe amount
      | dirStr == "down" -> Down <$> readMaybe amount
      | dirStr == "up" -> Up <$> readMaybe amount
    otherwise -> Nothing

  where
    spl = splitOn " " str

toTuple :: Movement -> (Integer, Integer, Integer)
toTuple m = case m of
  Forward x -> (x, 0, 0)
  Down x -> (0, x, 0)
  Up x -> (0, 0, x)

-- Star 1 stuff
addTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer) -> (Integer, Integer, Integer)
addTriple (a,b,c) (x,y,z) = (a+x, b+y, c+z)

getSums :: [Movement] -> (Integer, Integer, Integer)
getSums ms = foldr (\move acc -> acc `addTriple` toTuple move) (0,0,0) ms

-- Stuff for star 2
-- Horizontal, Depth, Aim
updateState :: (Integer, Integer, Integer) -> Movement -> (Integer, Integer, Integer)
updateState (h, d, a) m = case m of
  Down x -> (h, d, a+x)
  Up x -> (h, d, a-x)
  Forward x -> (h+x, d+a*x, a)

doMovements :: [Movement] -> (Integer, Integer, Integer)
doMovements ms = foldl' (\acc move-> acc `updateState` move) (0,0,0) ms

main :: IO ()
main = do
  input <- getInput "./inputs/day2"
  let
    (forward, down, up) = getSums input
    depth = down-up
    prod = forward * depth

  putStrLn $ "forward: " ++ show forward
  putStrLn $ "depth: " ++ show depth
  putStrLn $ "prod: " ++ show prod -- Answer for star 1

  -- Star 2 answer
  let
    (h, d, a) = doMovements input
  putStrLn $ show (h,d,a)
  putStrLn $ "star 2 answer: " ++ show (h*d)
