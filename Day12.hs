module Day12 where
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List.Extra (splitOn)
import Data.Foldable (foldl')
import Data.Char (isUpper)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

type Vertex = String
type Edge = (Vertex, Vertex)
type Graph = Map Vertex [Vertex]
type Path = [Vertex]

addEdge :: Graph -> Edge -> Graph
addEdge g (v1, v2) = Map.alter (addVertex v1) v2 $ Map.alter (addVertex v2)  v1 g
  where
    addVertex v mVl = case mVl of
      Just vs -> Just (v:vs)
      Nothing -> Just [v]

isLarge :: Vertex -> Bool
isLarge = all isUpper

readInput :: String -> Graph
readInput str = foldl' addEdge Map.empty edges
  where
    edges = (\l -> (l !! 0, l !! 1)) . splitOn "-" <$> lines str

paths :: Vertex -> Vertex -> Path -> Graph-> [Path]
paths goal cur path g =
  if end
  then [path]
  else concat [paths goal next (next:path) g | next <- newNodes]
  where
    end = cur == goal
    newNodes = filter goodNeighbour neighs
    neighs = fromMaybe [] $ Map.lookup cur g
    goodNeighbour v = isLarge v || notElem v path

-- Star 2
copyVertex :: Vertex -> Graph -> Graph
copyVertex v g = foldl' addEdge g edges
  where
    newVertex = "copy"++v
    neighs = fromMaybe [] $ Map.lookup v g
    edges = [(newVertex, n) | n <- neighs]

star2Graphs :: Graph -> [Graph]
star2Graphs g = map (`copyVertex` g) copyCaves
  where
    copyCaves = filter copyCave $ Map.keys g
    copyCave v = not (isLarge v) && v /= "start" && v /= "end"

removeCopy :: String -> String
removeCopy ('c':'o':'p':'y':s) = s
removeCopy s = s

nub = Set.toList . Set.fromList

main :: IO ()
main = do
  inputStr <- readFile "./inputs/day12example"
  let
    g = readInput inputStr
    ps = paths "end" "start" ["start"] g
  print $ length ps

  let
    gs = star2Graphs g
    p2s = paths "end" "start" ["start"] <$> gs
    uniquePaths = nub $ concat p2s
    noCopies = map (map removeCopy) uniquePaths
    realPaths = nub noCopies
  --print realPaths
  print $ length $ concat p2s
  print $ length realPaths
