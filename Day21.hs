{-# LANGUAGE MultiWayIf #-}

module Day21 where
import Data.List.Split (splitOn)
import Data.List (find, findIndex)
import Data.Maybe (fromJust, fromMaybe)
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Bifunctor (first)

type PlayerState = (Int, Int)
type GameState = (PlayerState, PlayerState, Bool, Int)

readInput :: String -> (PlayerState, PlayerState)
readInput str = ((head vals -1, 0), (vals !! 1 -1, 0))
  where
    ls = lines str
    vals = read <$> ( last . splitOn " " <$> ls)

playRound :: GameState -> GameState
playRound ((p1square, p1score), (p2square, p2score), turn, die) =
  ((p1square', p1score'), (p2square', p2score'), not turn, (die + 3) `mod` 100)
  where
    p1square' =
      if turn
      then (p1square + (die+1)*3 + 3) `mod` 10
      else p1square
    p1score' =
      if turn
      then p1score + p1square' + 1
      else p1score

    p2square' =
      if not turn
      then (p2square + (die+1)*3 + 3) `mod` 10
      else p2square
    p2score' =
      if not turn
      then p2score + p2square' + 1
      else p2score

gameOver :: GameState -> Bool
gameOver ((_, p1s), (_, p2s), _, _) = p1s >= 1000 || p2s >= 1000

findWin :: [GameState] -> Int
findWin = fromJust . findIndex gameOver

s1 :: GameState -> Int
s1 g = winRound * 3 * min s1 s2
  where
    is = iterate playRound g
    winRound = fromIntegral $ findWin is
    ((_, s1), (_,s2), _, _) = is !! fromIntegral winRound

-- Star 2
win :: GameParams -> Maybe (Int, Int)
win (p1score, p2score, _, _, _) =
  if | p1score >= 21 -> Just (1, 0)
     | p2score >= 21 -> Just (0, 1)
     | otherwise -> Nothing

-- Player 1 score, Player 2 score, Player 1 location, Player 2 location, Turn
type GameParams = (Int, Int, Int, Int, Bool)

wins :: Map GameParams (Int, Int) -> GameParams -> Maybe (Int, Int)
wins m p@(p1, p2, p1l, p2l, turn) =
  case win p of
    Just w -> Just w
    Nothing -> Nothing

      where
        dice :: [(Int, Int)]
        dice = [(3,1), (4,3), (5,6), (6,7), (7,6), (8,3), (9,1)]
        nextParams =
          if turn
          then [((p1 + 1 + (p1l + d) `mod` 10, p2, (p1l + d) `mod` 10, p2l, not turn), w) | (d,w) <- dice]
          else [((p1, p2 + 1 + (p2l + d) `mod` 10, p1l, (p2l + d) `mod` 10, not turn), w) | (d,w) <- dice]

        nextVals = first (m !?) <$> nextParams
        weightedVals nV = (\((p1w, p2w), w) -> (p1w * w, p2w * w)) <$> nV
        summedVals wV = (sum $ fst <$> wV, sum $ snd <$> wV)

buildMap :: Map GameParams (Int, Int)
buildMap = Map.empty
  where
    params = [(p1, p2, p1l, p2l, turn) | p1 <- [29, 28..0], p2 <- [29,28..0], p1l <- [0..9], p2l <- [0..9], turn <- [True, False]]

-- main :: IO ()
-- main = do
--   inputStr <- readFile "./inputs/day21example"
--   let
--     (p1, p2) = readInput inputStr
--     s0 = (p1, p2, True, 0)
--   print $ s1 s0
