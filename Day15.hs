module Day15 where

import Data.Map.Strict (Map, fromList)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable (minimumBy)
import Data.List.Extra (minimumOn)

type GameMap = (Map Point Int, Point)
type Point = (Int, Int)

readInput :: String -> GameMap
readInput str = (rm, (w-1,h-1))
  where
    listSquare = map (read . pure) <$> lines str
    h = length listSquare
    w = length (head listSquare)
    rm = fromList [((x,y), listSquare !! y !! x) | y <- [0..h-1], x <- [0..w-1]]

risk :: GameMap -> Point -> Int
risk (rm, _) p = fromMaybe 999 $ Map.lookup p rm

neighbours :: GameMap -> Point -> [Point]
neighbours (_, (maxX, maxY)) (x,y) = filter (\(a,b) -> 0 <= a  && a <= maxX && 0 <= b && b <= maxY) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

dijkstra :: GameMap -> Point -> Point -> Set Point -> Set Point -> Map Point Int -> Map Point Int
dijkstra gm goal current front visited dists =
  if current == goal
  then dists
  else dijkstra gm goal nextPoint newFront newVisited newDists

  where
    costHere = fromMaybe 999999 $ Map.lookup current dists
    neighs = neighbours gm current
    newVisited = Set.insert current visited
    newFront = Set.delete current front `Set.union` Set.difference (Set.fromList neighs) visited
    neighDists = Map.fromList $ (\p -> (p, distTo p)) <$> neighs
    newDists = Map.union neighDists dists
    distTo p =
      case Map.lookup p dists of
        Just cost -> min cost $ costHere + risk gm p
        Nothing -> costHere + risk gm p
    frontDists = Set.map (\p -> (p, fromMaybe 999999 $ Map.lookup p dists)) newFront
    nextPoint = fst $ minimumOn snd $ Set.toList frontDists

distanceTo :: GameMap -> Point -> Int
distanceTo gm goal = fromMaybe (-999) $ Map.lookup goal dmap
  where
    dmap = dijkstra gm goal (0,0) Set.empty Set.empty $ Map.singleton (0,0) 0

-- Star 2
fullMap :: GameMap -> GameMap
fullMap gm@(m, (x,y)) = (m', (x', y'))
  where
    x' = (x+1)*5-1
    y' = (y+1)*5-1
    points = [(a,b) | a <- [0..x], b <- [0..y]]
    incN a b = foo
      where
        foo = ((a + b - 1 ) `mod` 9) + 1
    copies (a,b) = [((a+i*(x+1), b+j*(y+1)), incN (risk gm (a,b)) (i+j)) | i <- [0..4], j <- [0..4]]
    m' = Map.fromList $ concat $ copies <$> points

main :: IO ()
main = do
  inputStr <- readFile "./inputs/day15"
  let
    (gm, end) = readInput inputStr

  print $ distanceTo (gm, end) end

  let
    gm'@(_, end') = fullMap (gm,end)
    l = [risk gm' (p,1) | p <- [0..49]]
  print l
  print (end, end')
  print $ distanceTo gm' end'
  pure ()
