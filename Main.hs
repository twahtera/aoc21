module Main where


powerset [] = [[]]
powerset (x:xs) = map (x:) (powerset xs) ++ powerset xs

checkList :: [Integer] -> [Integer] -> Bool
checkList all sublist = even allsum && (sublistsum == allsum `div` 2)
  where
    sublistsum = sum sublist
    allsum = sum all

validSets :: Integer -> [[Integer]]
validSets n =
  if solutionsExist
  then filter (checkList [1..n]) $ powerset [1..n]
  else []
  where
    solutionsExist = n `mod` 4 == 3 || n `mod` 4 == 0

lineString :: Integer -> [Integer] -> String
lineString len sel = [if i `elem` sel then '#' else '.' | i <- [1..len] ]

firstSet = safeHead . validSets

printFirstSet n =
  case firstSet n of
    Nothing -> putStrLn "-"
    Just xs -> putStrLn $ lineString n xs

slowCheck n = not $ null (validSets n)

main :: IO ()
main = sequence_ $ printFirstSet <$> [1..60]

safeHead [] = Nothing
safeHead (x:xs) = Just x
