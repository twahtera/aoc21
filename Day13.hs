module Day13 where
import Data.Set (Set, fromList, size)
import qualified Data.Set as Set
import Data.List.Extra (splitOn, maximumOn)
import Data.List (intercalate)

type Point = (Int, Int)
data Fold = X Int | Y Int

readInput :: String -> (Set Point, [Fold])
readInput str = (coords, folds)
  where
    parts = splitOn [""] $ lines str
    coordStrs = splitOn "," <$> head parts
    coords = fromList $ (\l -> ( read $ head l, read $ l !! 1)) <$> coordStrs
    folds = readFold <$> parts !! 1

readFold :: String -> Fold
readFold str = case last $ head parts of
  'x' -> X $ read $ parts !! 1
  'y' -> Y $ read $ parts !! 1
  _ -> X (-100)
  where
    parts = splitOn "=" str


foldPoint :: Fold -> Point -> Point
foldPoint (X i) = foldLeft i
foldPoint (Y i) = foldUp i

foldSheet :: Fold -> Set Point -> Set Point
foldSheet f = Set.map (foldPoint f)

foldUp :: Int -> Point -> Point
foldUp i (x,y) =
  if y > i
  then (x, i - (y-i))
  else (x,y)

foldLeft :: Int -> Point -> Point
foldLeft i (x,y) =
  if x > i
  then (i - (x-i), y)
  else (x,y)

-- Star 2
showSheet :: Set Point -> String
showSheet s = "\n" ++ intercalate "\n" [concat [showPoint (i, j) | i <- [0..w]] | j <- [0..h]]
  where
    (w, _) = setMaximumOn fst s
    (_, h) = setMaximumOn snd s
    showPoint p = if p `elem` s then "#" else "."

setMaximumOn :: Ord b => (a -> b) -> Set a -> a
setMaximumOn f s = maximumOn f (Set.toList s)

main :: IO ()
main = do
  inputStr <- readFile "./inputs/day13"
  let
    (sheet, folds) = readInput inputStr
    fstFold = foldSheet (head folds) sheet

    foldedSheets = scanl (flip foldSheet) sheet folds

  print $ size fstFold
  putStrLn $ showSheet $ last foldedSheets
  --sequence_ $ putStrLn . showSheet <$> foldedSheets
