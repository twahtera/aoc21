module Day20 where
import Data.Set (Set)
import qualified Data.Set as Set
import Utils
import Data.List.Split
import Data.Foldable (minimumBy, maximumBy)
import Data.Function (on)
import Day3 (toDec)
import Data.List (intercalate)

type Point = (Int, Int)
type Image = (Set Point, (Point, Point),Bool)
type Algorithm = [Bool]


readInput :: String -> (Algorithm, Image)
readInput str = (alg, (pointSet, bounds, False))
  where
    (algStr:imageStr:_) = splitOn [""] $ lines str
    alg = (== '#') <$> head algStr
    pointSet = Set.fromList [(x,y) | x <- [0..length (head imageStr) - 1], y <- [0..length imageStr - 1], imageStr !! y !! x == '#']
    bounds = imageBounds pointSet

imageBounds :: Set Point -> (Point, Point)
imageBounds img = ((minX,minY), (maxX,maxY))
  where
    minX = fst $ minimumBy (compare `on` fst) img
    maxX = fst $ maximumBy (compare `on` fst) img
    minY = snd $ minimumBy (compare `on` snd) img
    maxY = snd $ maximumBy (compare `on` snd) img

inBounds :: (Point, Point) -> Point -> Bool
inBounds points (x, y) = minX <= x && x <= maxX && minY <= y && y <= maxY
  where
    ((minX, minY), (maxX, maxY)) = points

pixVal :: Image -> Point -> Bool
pixVal img@(pointSet, bounds, outside) p =
  if inBounds bounds p
  then p ∈ pointSet
  else outside

newPointOn :: Algorithm -> Image -> Point -> Bool
newPointOn alg img (x,y) = alg !! algIndex
  where
    neighbourhood = [(a,b) | b <- [y-1..y+1], a <- [x-1..x+1]]
    bitString = [if pixVal img p then 1 else 0 | p <- neighbourhood]
    algIndex = toDec bitString

newImage :: Algorithm -> Image -> Image
newImage alg img@(pointSet, bounds, outside) = (newPointSet, newBounds, newOutside)
  where
    newOutside = if head alg then not outside else outside
    ((minX, minY), (maxX, maxY)) = bounds
    newPointSet = Set.fromList [(x,y) | x <- [minX-1..maxX+1], y <- [minY-1..maxY+1], newPointOn alg img (x,y)]
    newBounds = imageBounds newPointSet

pixelsLit :: Image -> Int
pixelsLit (pointSet, _, outside) = if outside then -1 else length pointSet

showImg :: Image -> String
showImg img@(_, bounds, _) = intercalate "\n" [[if pixVal img (x,y) then '#' else '.' | x <- [minX..maxX]] | y <- [minY..maxY]]
  where
    ((minX, minY), (maxX, maxY)) = bounds

main :: IO ()
main = do
  inputStr <- readFile "./inputs/day20"
  let
    (alg, img) = readInput inputStr
    imgs = iterate (newImage alg) img
  print $ pixelsLit <$> take 51 imgs
  pure ()
