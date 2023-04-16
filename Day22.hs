module Day22 where
import Data.List.Split
import Data.Set (Set, fromList, empty, difference)
import Utils
import Data.Foldable (foldl')

type Point = (Int, Int, Int)
type Range = (Int, Int)
type Cuboid = (Range, Range, Range)
type Instruction = (Bool, Cuboid)

readInput :: String -> [Instruction]
readInput str = readInstruction <$> ls
  where
    ls = lines str

readInstruction :: String -> Instruction
readInstruction str = (status, cuboid)
  where
    parts = words str
    status =
      case head parts of
        "on" -> True
        _ -> False
    cuboid = readCuboid $ parts !! 1

readCuboid :: String -> Cuboid
readCuboid str = (ranges !! 0, ranges !! 1, ranges !! 2)
  where
    ranges = readRange <$> splitOn "," str

readRange :: String -> Range
readRange str = (read $ strs !! 0, read $ strs !! 1)
  where
    strs = splitOn ".." $ drop 2 str


clipRange :: Range -> Range
clipRange (f, t) = (max (-50) f, min 50 t)

clipCuboid :: Cuboid -> Cuboid
clipCuboid (x,y,z) = (clipRange x, clipRange y, clipRange z)

cuboidPoints :: Cuboid -> Set Point
cuboidPoints c = fromList [(x,y,z) | x <- [x1..x2], y <- [y1..y2], z <- [z1..z2]]
  where
    ((x1, x2), (y1, y2), (z1, z2)) = clipCuboid c

applyInstruction :: Instruction -> Set Point -> Set Point
applyInstruction (state, cuboid) pointSet =
  if state
  then pointSet ∪ cuboidPoints cuboid
  else pointSet `difference ` cuboidPoints cuboid

boot :: [Instruction] -> Set Point
boot = foldl' (flip applyInstruction) empty

-- Star 2
overlap :: Cuboid -> Cuboid -> Bool
overlap c1 c2@((a1, a2), (b1, b2), (c1,c2))
  = any $ (flip inCuboid) <$> [(a,b,c) | x <- [a1, a2], b <- [b1,b2], c <- [c1,c2]]
  || overlap c2 c1

-- Split two cuboids into non-overlapping parts
-- splitCuboids :: Cuboid -> Cuboid -> [Cuboid]
-- splitCuboids c1 c2 =
--   if | overlap c1 c2 ==

inRange :: Int -> Range -> Bool
inRange a (b, c) = b <= a && a <= c

inCuboid :: Point -> Cuboid -> Bool
inCuboid (x, y, z) (xr, yr, zr) = inRange x xr && inRange y yr && inRange z zr

main :: IO ()
main = do
  inputStr <- readFile "./inputs/day22"
  let
    input = readInput inputStr
    cubesOn = boot input
  print $ length cubesOn
