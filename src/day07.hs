import Data.List
import Data.Char
import Data.Function

type Hand = String

value :: Char -> Int
value 'A' = 14
value 'K' = 13
value 'Q' = 12
value 'J' = 11
value 'T' = 10
value n = digitToInt n

handType :: Hand -> Int
handType cs = case counts of
    [5] -> 7
    [1, 4] -> 6
    [2, 3] -> 5
    [1, 1, 3] -> 4
    [1, 2, 2] -> 3
    [1, 1, 1, 2] -> 2
    [1, 1, 1, 1, 1] -> 1
    _ -> error ("Bad hand: " ++ show cs ++ " -> " ++ show counts)
  where counts = (sort . map length . group . sort) cs

handView cs = (handType cs, map value cs)

parse :: String -> [(Hand, Int)]
parse = map ((\[hand, bet] -> (hand, read bet)) . words) . lines

part1 :: [(Hand, Int)] -> Int
part1 = sum . map (\(rank, (hand, bet)) -> rank * bet) . zip [1..] .
        sortOn (handView . fst)

options :: Hand -> [Hand]
options [] = return []
options (c : cs) = do
  c <- case c of
    'J' -> "23456789TQKA"
    _ -> [c]
  cs <- options cs
  return (c : cs)

bestType :: Hand -> Int
bestType = maximum . map handType . options

value2 :: Char -> Int
value2 'A' = 14
value2 'K' = 13
value2 'Q' = 12
value2 'J' = 1
value2 'T' = 10
value2 n = digitToInt n

handView2 cs = (bestType cs, map value2 cs)

part2 :: [(Hand, Int)] -> Int
part2 = sum . map (\(rank, (hand, bet)) -> rank * bet) . zip [1..] .
        sortOn (handView2 . fst)

main = do
  input <- parse <$> getContents
  putStrLn $ show $ part1 input
  putStrLn $ show $ part2 input
