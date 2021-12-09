module Day8 where
import Data.Bifunctor (bimap)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Tuple (swap)
import Data.List (sort, permutations)


getInput :: FilePath -> IO [([String], [String])]
getInput path = do
  fileStr <- readFile path
  let
    ls = lines fileStr
  pure $ parseLine <$> ls

parseLine :: String -> ([String], [String])
parseLine str = (head splitStr, splitStr !! 1)
  where
    splitStr = splitOn ["|"] $ words str

-- Star 1
getSpecialDigits :: [String] -> Int
getSpecialDigits strs = length $ filter isSpecialDigit strs
  where
    isSpecialDigit str = length str `elem` specialLengths
    specialLengths = [2, 4, 3, 7]

-- Star 2
segmentsInNums :: [(Int, String)]
segmentsInNums =
  [ (0, "abcefg")
  , (1, "cf")
  , (2, "acdeg")
  , (3, "acdfg")
  , (4, "bcdf")
  , (5, "abdfg")
  , (6, "abdefg")
  , (7, "acf")
  , (8, "abcdefg")
  , (9, "abcdfg")
  ]

-- (Input, Output)
type Conf = [(Char, Char)]

idConf = [('a', 'a'), ('b', 'b'), ('c', 'c'), ('d', 'd'), ('e', 'e'), ('f', 'f'), ('g', 'g')]

allConfs :: [Conf]
allConfs = flip zip "abcdefg" <$> permutations "abcdefg"

isValid :: [String] -> Conf -> Bool
isValid input conf = all isJust $ readDigit conf <$> input

readDigit :: Conf -> String -> Maybe Int
readDigit conf input = lookup (sort outputs) $ swap <$> segmentsInNums
  where
    outputs = catMaybes $ flip lookup conf <$> input

validConfs :: [String] -> [Conf]
validConfs input = filter (isValid input) allConfs

listToDec :: [Int] -> Int
listToDec is = sum $ (\(a,b) -> 10^a * b) <$> zip [0..] (reverse is)

solveInput :: ([String], [String]) -> Int
solveInput (input, output) = listToDec $ catMaybes $ readDigit conf <$> output
  where
    conf = head $ validConfs input

main :: IO ()
main = do
  input <- getInput "./inputs/day8"

  let
    outputs = map snd input
    allSpecialDigits = sum $ getSpecialDigits <$> outputs
  print allSpecialDigits

  --Star 2
  let
    sols = solveInput <$> input
  print $ sum sols
