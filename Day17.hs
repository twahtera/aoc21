module Day17 where
import Data.List ( elemIndex, inits )
import Data.Maybe (fromJust)
import Data.List.Extra (tails)

import Data.Set (Set, fromList, difference)
import qualified Data.Set as Set

readInput :: String -> ((Int, Int), (Int, Int))
readInput s = ((read x1s, read x2s), (read y1s, read y2s))
  where
    s' = drop 15 s
    (x1s, s'') = splitAtElem '.' s'
    (x2s, s''') = splitAtElem ',' $ drop 2 s''
    (y1s, s'''') = splitAtElem '.' $ drop 4 s'''
    y2s = drop 2 s''''

splitAtElem :: Eq a => a -> [a] -> ([a], [a])
splitAtElem e s = splitAt (fromJust $ elemIndex e s) s

star1 :: ((Int, Int), (Int, Int)) -> Int
star1 (_, (y1, y2)) = (vel+1) * vel `div` 2
  where
    vel = max (maxVelToP y1) (maxVelToP y2)
    maxVelToP p =
      if p > 0
      then p+1
      else (-p) - 1

-- Star 2
xVels :: (Int, Int) -> [Int]
xVels (a,b) = [minVel..b]
  where
    a' = fromIntegral a
    minVel = floor $ sqrt (1 - 4 * ((-2)*a')) / 2

yVels :: (Int, Int) -> [Int]
yVels range@(y1, y2) = [y1..vel]
  where
    vel = max (maxVelToP y1) (maxVelToP y2)
    maxVelToP p =
      if p > 0
      then p+1
      else (-p) - 1

between :: (Int, Int) -> Int -> Bool
between (a,b) n = a' <= n && n <= b'
  where
    a' = min a b
    b' = max a b

pointInTarget :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
pointInTarget (xrange, yrange) (x, y) = between xrange x && between yrange y

pointSequence :: ((Int, Int), (Int, Int)) -> (Int, Int) -> [(Int, Int)]
pointSequence (xrange, yrange) (xVel, yVel) = zip xDists yDists
  where
    xVels = [xVel, xVel-1 .. 0] ++ repeat 0
    yVels = [yVel, yVel-1 ..]

    xDists = takeWhile (<= snd xrange) $ sum <$> drop 1 (inits xVels)
    yDists = takeWhile (>= fst yrange) $ sum <$> drop 1 (inits yVels)

velsHitTarget :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
velsHitTarget target vels = any (pointInTarget target) $ pointSequence target vels

star2 :: ((Int, Int), (Int, Int)) -> Int
star2 target@(xrange, yrange) = length $ filter (velsHitTarget target) vels
  where
    vels = [(x,y) | x <- xVels xrange, y <- yVels yrange]

main :: IO ()
main = do
  inputStr <- readFile "./inputs/day17"
  let input = readInput inputStr
  print $ star1 input
  print $ star2 input
