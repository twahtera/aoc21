module Day19 where

import Numeric.LinearAlgebra
    ( (><), (#>), disp, fromList, Matrix, Vector, ident, (!) )
import qualified Numeric.LinearAlgebra as NAL

import Data.List.Extra (splitOn)
import Data.Set (Set, union, intersection)
import qualified Data.Set as Set
import Data.List (find)
import Data.Maybe (catMaybes)
import Data.Foldable ( minimumBy, maximumBy )
import Data.Function (on)

readInput :: String -> [Set (Vector Double)]
readInput s = Set.fromList <$> (map . map) readPoints probes'
  where
    ls = lines s
    probes = splitOn [""] ls
    probes' = tail <$> probes
    readPoints l = fromList $ read <$> splitOn "," l

-- Rotate 90 degrees counter clockwise along a unit axis
rx :: Matrix Double
rx = (3><3)
     [ 1, 0, 0,
       0, 0,-1,
       0, 1, 0 ]

ry :: Matrix Double
ry = (3><3)
     [ 0, 0, 1,
       0, 1, 0,
      -1, 0, 0 ]

rz :: Matrix Double
rz = (3><3)
     [ 0,-1, 0,
       1, 0, 0,
       0, 0, 1 ]

rotations :: Set (Vector Double) -> [Set (Vector Double)]
rotations p = (\r -> Set.map (r #>) p) <$> rotations
  where
    rotations = [ident 3 <> rx' <> r| rx' <- rxs, r <- rys ++ rzs  ]

    rxs = take 4 $ iterate (rx <>) $ ident 3
    rys = take 4 $ iterate (ry <>) $ ident 3
    rzs = [ident 3 <> rz, rz <> rz <> rz]

-- translate points p2s
translations :: Set (Vector Double) -> Set (Vector Double) -> [(Set (Vector Double), Vector Double)]
translations p1s p2s = do
  p1 <-Set.toList p1s
  p2 <- Set.toList p2s

  pure (Set.map ((p1 - p2) +) p2s, p1 - p2)

overlap :: Set (Vector Double) -> Set (Vector Double) -> Bool
overlap p1 p2 = 12 <= length (p1 `intersection` p2)

merge :: Set (Vector Double) -> Set (Vector Double) -> Maybe (Set (Vector Double), Vector Double)
merge points p =
  case mergeable of
    Just (newPoints, shift) -> Just (points `union` newPoints, shift)
    Nothing -> Nothing
  where
    rs = rotations p
    ts = concat $ translations points <$> rs
    mergeable = find (overlap points . fst) ts

mergeOne :: Set (Vector Double) -> [Set (Vector Double)] -> [Set (Vector Double)] -> (Set (Vector Double), Vector Double, [Set (Vector Double)])
mergeOne cur not [] = (cur, fromList [], not)
mergeOne cur not (ps:left) =
  case merge cur ps of
    Just (merged, shift) -> (merged, shift, not++left)
    Nothing -> mergeOne cur (ps:not) left

mergeAll :: Set (Vector Double) -> [Set (Vector Double)] -> [Vector Double] -> (Set (Vector Double), [Vector Double])
mergeAll initial [] shifts = (initial, shifts)
mergeAll initial ps shifts = mergeAll next rest (shift:shifts)
  where
    (next, shift, rest) = mergeOne initial [] ps

star2 :: [Vector Double] -> Double
star2 p = maximum [manhattan a b | a <- p, b <- p]

manhattan :: Vector Double -> Vector Double -> Double
manhattan p1 p2 = abs (p1 ! 0 - p2 ! 0) + abs (p1 ! 1 - p2 ! 1) + abs (p1 ! 2 - p2 ! 2)

main :: IO ()
main = do
  inputStr <- readFile "./inputs/day19"
  let
    input = readInput inputStr
    first = head input
    rest = tail input
    merged = mergeAll first rest []
  print $ length $ fst merged
  print $ star2 $ snd merged
