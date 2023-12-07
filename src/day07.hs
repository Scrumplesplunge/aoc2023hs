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
handType cs = case sort $ map length $ group $ sort cs of
    [5] -> 7
    [1, 4] -> 6
    [2, 3] -> 5
    [1, 1, 3] -> 4
    [1, 2, 2] -> 3
    [1, 1, 1, 2] -> 2
    [1, 1, 1, 1, 1] -> 1

handView cs = (handType cs, map value cs)

parse :: String -> [(Hand, Int)]
parse = map ((\[hand, bet] -> (hand, read bet)) . words) . lines

part1 :: [(Hand, Int)] -> Int
part1 = sum . map (\(rank, (hand, bet)) -> rank * bet) . zip [1..] .
        sortOn (handView . fst)

upgrade :: Int -> Int -> Int
upgrade 5 x = 7  -- Five jokers -> Five of a kind.
upgrade 4 x = 7  -- Four jokers -> Five of a kind (match the fifth).
upgrade 3 5 = 7  -- Three jokers in a full house -> Five of a kind.
upgrade 3 x = 6  -- Three jokers but no full house -> Four of a kind.
upgrade 2 5 = 7  -- Two jokers in a full house -> Five of a kind.
upgrade 2 3 = 6  -- Two jokers and another pair -> Four of a kind.
upgrade 2 2 = 4  -- Two jokers and three different cards -> Three of a kind.
upgrade 1 6 = 7  -- One joker: Four of a kind -> Five of a kind.
upgrade 1 4 = 6  -- One joker: Three of a kind -> Four of a kind.
upgrade 1 3 = 5  -- One joker: Two pairs -> Full house.
upgrade 1 2 = 4  -- One joker: One pair -> Three of a kind.
upgrade 1 1 = 2  -- One joker: High card -> One pair.
upgrade 0 x = x  -- No jokers, no change.

value2 :: Char -> Int
value2 'J' = 1
value2 x = value x

handView2 cs = (upgrade (length [1 | c <- cs, c == 'J']) (handType cs), map value2 cs)

part2 :: [(Hand, Int)] -> Int
part2 = sum . map (\(rank, (hand, bet)) -> rank * bet) . zip [1..] .
        sortOn (handView2 . fst)

main = do
  input <- parse <$> getContents
  putStrLn $ show $ part1 input
  putStrLn $ show $ part2 input
