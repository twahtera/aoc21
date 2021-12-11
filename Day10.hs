{-# LANGUAGE MultiWayIf #-}
module Day10 where
import Data.List ( find, foldl', sort )
import Data.List.Extra (elemIndex)
import Data.Maybe (catMaybes)

getInput :: FilePath -> IO [String]
getInput path = lines <$> readFile path

data ParseResult = Ok | Incomplete String | IllegalChar Char
  deriving Show

opening = "([{<"
closing = ")]}>"

pair :: Char -> Char
pair c = case elemIndex c opening of
  Just idx -> closing !! idx
  _ -> c

parseParens :: String -> String -> ParseResult
parseParens [] [] = Ok
parseParens [] stack = Incomplete stack
parseParens (c:cs) stack = if
  | c `elem` opening -> parseParens cs (pair c:stack)
  | c `elem` closing ->
    case stack of
      (s:ss) ->
        if c == s
        then parseParens cs ss
        else IllegalChar c
      _ -> IllegalChar c
  | otherwise -> IllegalChar c

getError :: ParseResult -> Maybe Char
getError (IllegalChar c) = Just c
getError _ = Nothing

scoreChar :: Char -> Int
scoreChar c = case c of
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  '>' -> 25137
  _   -> 0

scoreString :: String -> Maybe Int
scoreString s = scoreChar <$> getError (parseParens s "")

-- Star 2
-- Star 2
scoreStackChar :: Char -> Int
scoreStackChar c = case c of
  ')' -> 1
  ']' -> 2
  '}' -> 3
  '>' -> 4
  _   -> 0

scoreCompletionString :: Int -> String -> Int
scoreCompletionString = foldl' (\acc c -> acc * 5 + scoreStackChar c)

getCompletionString :: String -> Maybe String
getCompletionString str = case parseParens str "" of
  Incomplete s -> Just s
  _ -> Nothing

main :: IO ()
main = do
  input <- getInput "./inputs/day10"

  print $ sum $ catMaybes (scoreString <$> input)

  let
    completionStrings = getCompletionString <$> input
    stringScores = scoreCompletionString 0 <$> catMaybes completionStrings
    winner = sort stringScores !! (length stringScores `div` 2)

  print winner
