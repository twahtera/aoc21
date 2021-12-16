module Day16 where
import Day3 (toDec)
import Data.List (findIndex)
import Data.Maybe (fromMaybe, catMaybes)

readInput :: String -> [Int]
readInput s = concat $ hexToBin <$> s

hexToBin :: Char -> [Int]
hexToBin '0' = [0, 0, 0, 0]
hexToBin '1' = [0, 0, 0, 1]
hexToBin '2' = [0, 0, 1, 0]
hexToBin '3' = [0, 0, 1, 1]
hexToBin '4' = [0, 1, 0, 0]
hexToBin '5' = [0, 1, 0, 1]
hexToBin '6' = [0, 1, 1, 0]
hexToBin '7' = [0, 1, 1, 1]
hexToBin '8' = [1, 0, 0, 0]
hexToBin '9' = [1, 0, 0, 1]
hexToBin 'A' = [1, 0, 1, 0]
hexToBin 'B' = [1, 0, 1, 1]
hexToBin 'C' = [1, 1, 0, 0]
hexToBin 'D' = [1, 1, 0, 1]
hexToBin 'E' = [1, 1, 1, 0]
hexToBin 'F' = [1, 1, 1, 1]
hexToBin _ = []

data Packet
  = Literal Header Int
  | Operator Header [Packet]
  deriving (Show)

type Header = (Int, Int)

parsePacket :: [Int] -> (Maybe Packet, [Int])
parsePacket = parseFirst [parseLiteral, parseOperator]

parseHeader :: [Int] -> (Header, [Int])
parseHeader stream = ((pVersion, pType), rest)
  where
    (bits, rest) = splitAt 6 stream
    pVersion = toDec $ take 3 bits
    pType = toDec $ drop 3 bits

parseLiteral :: [Int] -> (Maybe Packet, [Int])
parseLiteral stream =
  case h of
    (_, 4) ->
      (Just $ Literal h litVal, rest)
    _ -> (Nothing, stream)
  where
    (h, s') = parseHeader stream
    (litVal, rest) = parseLiteralValue s'

parseLiteralValue :: [Int] -> (Int, [Int])
parseLiteralValue s = (toDec bits, rest)
  where
    parses = iterParse parseLiteralChunk s
    lastIdx = fromMaybe 0 $ findIndex (not . fst . fst) parses
    chunks = take (lastIdx + 1) parses
    bits = concat $ snd . fst <$> chunks
    rest = snd $ last chunks

iterParse :: ([Int] -> (Maybe a, [Int])) -> [Int] -> [(a, [Int])]
iterParse _ [] = []
iterParse f s =
  case parsed of
    Just p -> (p, rest) : iterParse f rest
    Nothing -> []

  where
    (parsed, rest) = f s

parseFirst :: [[Int] -> (Maybe a, [Int])] -> [Int] -> (Maybe a, [Int])
parseFirst [] s = (Nothing, s)
parseFirst (f:fs) s =
  case f s of
    (Just p, rest) -> (Just p, rest)
    (Nothing, _) -> parseFirst fs s

parseLiteralChunk :: [Int] -> (Maybe (Bool, [Int]), [Int])
parseLiteralChunk s = (Just (head bits == 1, drop 1 bits), rest)
  where
    (bits, rest) = splitAt 5 s

parseOperator :: [Int] -> (Maybe Packet, [Int])
parseOperator s = (Just $ Operator header packets, rest)
  where
    (header, s') = parseHeader s
    (lType, s'') = splitAt 1 s'
    (packets, rest) = if lType == [0]
                      then parseType0Op s''
                      else parseType1Op s''

parseType0Op :: [Int] -> ([Packet], [Int])
parseType0Op s = (packets, rest)
  where
    (lBits, s') = splitAt 15 s
    (content, rest) = splitAt (toDec lBits) s'
    packetParses = iterParse parsePacket content
    packets = fst <$> packetParses

parseType1Op :: [Int] -> ([Packet], [Int])
parseType1Op s = (packets, rest)
  where
    (nBits, s') = splitAt 11 s
    packetParses = take (toDec nBits) $ iterParse parsePacket s'
    packets = fst <$> packetParses
    rest = snd $ last packetParses

getHeaders :: Packet -> [Header]
getHeaders (Literal h _) = [h]
getHeaders (Operator h ps) = h : concat (getHeaders <$> ps)

star1 :: [Int] -> Int
star1 s = sum $ fst <$> headers
  where
    (p, _) = parsePacket s
    headers = maybe [] getHeaders p

-- Star2
evalPacket :: Packet -> Int
evalPacket (Literal h v) = v
evalPacket (Operator (_, 0) ps) = sum $ evalPacket <$> ps
evalPacket (Operator (_, 1) ps) = product $ evalPacket <$> ps
evalPacket (Operator (_, 2) ps) = minimum $ evalPacket <$> ps
evalPacket (Operator (_, 3) ps) = maximum $ evalPacket <$> ps
evalPacket (Operator (_, 5) (p:p':_)) = if evalPacket p > evalPacket p' then 1 else 0
evalPacket (Operator (_, 6) (p:p':_)) = if evalPacket p < evalPacket p' then 1 else 0
evalPacket (Operator (_, 7) (p:p':_)) = if evalPacket p == evalPacket p' then 1 else 0
evalPacket _ = -1

star2 :: String -> Maybe Int
star2 s = evalPacket <$> fst (parsePacket $ readInput s)

main :: IO ()
main = do
  inputStr <- readFile "./inputs/day16"

  let
    input = readInput inputStr
  print $ star1 input

  let
    (packet, _) = parsePacket input

  print $ evalPacket <$> packet
