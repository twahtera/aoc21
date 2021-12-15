module Day14 where
import Data.List.Extra (splitOn, maximumOn, minimumOn )
import Data.Maybe (catMaybes)
import Data.MultiSet (MultiSet, union)
import qualified Data.MultiSet as MultiSet
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.List (foldl')
import Debug.Trace (trace)

readInput :: String -> (String, [(Char, Char, Char)])
readInput str = (template, readRule <$> ruleLines)
  where
    ls = lines str
    template = head ls
    ruleLines = drop 2 ls
    readRule s = (\ss -> (head (head ss), head ss !! 1, head (ss !! 1))) $ splitOn " -> " s

-- Not used any more. too slow crap
insertChar :: ((Char, Char), Char) -> Char -> Char -> Maybe Char
insertChar (s,new) a b =
  if s == (a,b)
  then Just new
  else Nothing


modifyString :: [Char -> Char -> Maybe Char] -> String -> String
modifyString insertChars (a:b:rest) = case charsToInsert of
                                        [] -> a : modifyString insertChars (b:rest)
                                        (n:_) -> a : n : modifyString insertChars (b:rest)
  where
    charsToInsert = catMaybes [ic a b | ic <- insertChars]

modifyString _ str = str

-- Star 2
-- Makes a multiset of all the inserted characters
iteratePolymers :: [(Char, Char, Char)] -> MultiSet (Char, Char) -> MultiSet (Char, Char)
iteratePolymers fs ms = deleted `union` inserted
  where
    insDels = f ms <$> fs
    dels = catMaybes $ snd <$> insDels
    ins = concat $ fst <$> insDels

    deleted = foldr MultiSet.deleteAll ms dels
    inserted = MultiSet.fromOccurList ins

f :: MultiSet (Char, Char) -> (Char, Char, Char) -> ([((Char, Char), Int)], Maybe (Char, Char))
f ms (a,b,c) = (ins, del)
  where
    occur = MultiSet.occur (a,b) ms
    del = if occur > 0 then Just (a,b) else Nothing
    ins = if occur > 0 then [((a,c), occur), ((c,b), occur)] else []


iterateN :: [(Char,Char,Char)] -> String -> Int -> MultiSet (Char, Char)
iterateN fs template iters = ms --mco - lco
  where
    tuples = zip template (tail template)
    initialMs = MultiSet.fromList tuples
    rounds = iterate (iteratePolymers fs) initialMs
    ms = rounds !! iters

solve :: [(Char,Char,Char)] -> String -> Int -> Int
solve fs template iters = mco - lco
  where
    ms = iterateN fs template iters
    charOccurs = MultiSet.insert (last template) $ MultiSet.map fst ms

    lco = snd $ minimumOn snd $ MultiSet.toOccurList charOccurs
    mco = snd $ maximumOn snd $ MultiSet.toOccurList charOccurs

main :: IO ()
main = do
  inputStr <- readFile "./inputs/day14"
  let
    (template, rules) = readInput inputStr
    star1 = solve rules template 40
  print star1
