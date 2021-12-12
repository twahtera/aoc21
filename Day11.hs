{-# LANGUAGE MultiWayIf #-}
module Day11 where

import Data.Map.Strict (Map, (!), fromList)
import qualified Data.Map.Strict as Map
import Debug.Trace (trace)
import Data.List.Extra (maximumOn)
import Data.List (intersperse)
import Data.List

type Point = (Int, Int)
type GameMap = Map Point Int

getInput :: FilePath -> IO GameMap
getInput path = do
  fileStr <- readFile path
  let
    ls = lines fileStr
    ints = map (read . (:[])) <$> ls
    points = [(x,y) | x<- [0..length (head ints)-1], y <- [0..length ints-1]]
  pure $ Map.fromList [((x,y), ints !! y !! x) | (x,y) <- points]

pointsAround :: Point -> [Point]
pointsAround (x,y) = [(a,b) | a <- [x-1..x+1], b <- [y-1..y+1], not (a == x && b == y)]

-- flash a point whose value is >= 9. Set it to -999 so it won't be
-- flashed again this turn
flashPoint :: GameMap -> Point -> GameMap
flashPoint gm p = Map.insert p (-9) updateNeighbours
  where
    updateNeighbour n m = Map.adjust (+1) n m
    updateNeighbours = foldr updateNeighbour gm (pointsAround p)

flashOne :: GameMap -> Maybe GameMap
flashOne gm = if max > 9
              then Just $ flashPoint gm maxKey
              else Nothing
  where
    (maxKey, max) = findMax gm

findMax :: GameMap -> (Point, Int)
findMax gm = maximumOn snd $ Map.assocs gm

iterateM :: (a -> Maybe a) -> a -> [a]
iterateM f a = case f a of
                 Just a' -> a' : iterateM f a'
                 Nothing -> []

playRound :: GameMap -> (GameMap, Int)
playRound gm = (newState, length flashes)
  where
    increasedEnergy = fmap (+1) gm
    flashes = iterateM flashOne increasedEnergy
    newState = (\a -> if a < 0 then 0 else a) <$> last (increasedEnergy:flashes)

rounds :: (GameMap, Int) -> [(GameMap, Int)]
rounds = iterate (playRound . fst)

showMap :: Int -> Int -> GameMap -> String
showMap x y gm = '\n' : intercalate "\n" [concat [ showVal $ gm ! (a,b) | a <-[0..x-1]] | b <- [0..y-1]]
  where
    showVal v = if v < 0 then "." else pure $ "0123456789abcdefghijklmnopqrstuvwxyz" !! v

-- Star 2
allZero :: GameMap -> Bool
allZero m = 0 == sum (Map.elems m)

main :: IO ()
main = do
  input <- getInput "./inputs/day11"
  let
    rs = rounds (input,0)
    flashes = snd <$> rs
  print $ sum $ take 100 $ tail flashes
  print $ findIndex allZero $ map fst rs
