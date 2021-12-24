module Day18 where

import Day17 (splitAtElem)
import Debug.Trace
import Data.Maybe (fromMaybe)
import Day11 (iterateM)
import GHC.Base (Alternative((<|>)))

data Tree = Node Tree Tree
          | Val Int
          deriving (Eq)

instance Show Tree where
  show (Val i) = show i
  show (Node t1 t2) = "[" ++ show t1 ++ "," ++ show t2 ++ "]"

instance Read Tree where
  readsPrec _  = pure . readTree'

readTree' :: String -> (Tree, String)
readTree' ('[':s) = (Node t1 t2, tail s'')
  where
    (t1, s') = readTree' s
    (t2, s'') = readTree' $ tail s'
readTree' s = (Val val, rest)
  where
    (valStr, rest) = break (\c -> c == ',' || c == ']') s
    val = read valStr

lft :: Tree -> Tree
lft (Node t _) = t
lft v = v

rgt :: Tree -> Tree
rgt (Node _ t) = t
rgt v = v

add :: Tree -> Tree -> Tree
add t1 t2 = reduce $ Node t1 t2

reduce :: Tree -> Tree
reduce t = last $ t : iterateM reduce' t

reduce' :: Tree -> Maybe Tree
reduce' t = explode t <|> split' t


explode :: Tree -> Maybe Tree
explode t = fst <$> explode' t 0

explode' :: Tree -> Int -> Maybe (Tree, (Int, Int))
explode' (Node (Node (Val a) (Val b)) t2) 3 = Just (Node (Val 0) (addToLft t2 b), (a, 0))
explode' (Node t1 (Node (Val a) (Val b))) 3 = Just (Node (addToRgt t1 a) (Val 0), (0, b))

explode' (Node t1 t2) depth =
  case (explodeLeft, explodeRight) of
    (Just (newT1, (addLeft, addRight)), _      ) -> Just (Node newT1 (addToLft t2 addRight), (addLeft, 0))
    (Nothing, Just (newT2, (addLeft, addRight))) -> Just (Node (addToRgt t1 addLeft) newT2, (0, addRight))
    (Nothing, Nothing) -> Nothing
  where
    explodeLeft = explode' t1 $ depth + 1
    explodeRight = explode' t2 $ depth + 1
explode' (Val n) _ = Nothing

addToRgt :: Tree -> Int -> Tree
addToRgt t 0 = t
addToRgt (Val a) n = Val $ a + n
addToRgt (Node t1 t2) n = Node t1 (addToRgt t2 n)

addToLft :: Tree -> Int -> Tree
addToLft t 0 = t
addToLft (Val a) n = Val $ a + n
addToLft (Node t1 t2) n = Node (addToLft t1 n) t2

divUp a b = case a `divMod` b of
  (d, 0) -> d
  (d, _) -> d+1

split' :: Tree -> Maybe Tree
split' (Val n) = if n >= 10
                then Just $ Node (Val $ n `div` 2) (Val $ n `divUp` 2)
                else Nothing
split' (Node t1 t2) =
  case (split' t1, split' t2) of
  (Just t1', _) -> Just $ Node t1' t2
  (Nothing, Just t2') -> Just $ Node t1 t2'
  _ -> Nothing

split :: Tree -> Tree
split t = fromMaybe t (split' t)

magnitude :: Tree -> Int
magnitude (Val n) = n
magnitude (Node t1 t2) = 3 * magnitude t1 + 2 * magnitude t2

star2 :: [Tree] -> Int
star2 trees = maximum sums
  where
    tuples = [(a,b) | a <- trees, b <- trees]
    sums = magnitude . uncurry add <$> tuples

main :: IO ()
main = do
  inputStr <- readFile "./inputs/day18"
  let
    input :: [Tree]
    input = read <$> lines inputStr
    calculation = foldl1 add input
  print $ magnitude calculation
  print $ star2 input
