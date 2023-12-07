import Data.List
import Data.Function

data Card = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine
          | Ten | Jack | Queen | King | Ace deriving (Eq, Ord, Show)
data Hand = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse
          | FourOfAKind | FiveOfAKind deriving (Eq, Ord, Show)

handType :: [Card] -> Hand
handType cs = case sort $ map length $ group $ sort cs of
    [5] -> FiveOfAKind
    [1, 4] -> FourOfAKind
    [2, 3] -> FullHouse
    [1, 1, 3] -> ThreeOfAKind
    [1, 2, 2] -> TwoPair
    [1, 1, 1, 2] -> OnePair
    [1, 1, 1, 1, 1] -> HighCard

parse :: String -> [(Hand, [Card], Int)]
parse = map (parseHand . words) . lines
  where parseHand [h, b] = let h' = map card h in (handType h', h', read b)
        card '2' = Two
        card '3' = Three
        card '4' = Four
        card '5' = Five
        card '6' = Six
        card '7' = Seven
        card '8' = Eight
        card '9' = Nine
        card 'T' = Ten
        card 'J' = Jack
        card 'Q' = Queen
        card 'K' = King
        card 'A' = Ace

winnings :: [(Hand, [Card], Int)] -> Int
winnings = sum . map (uncurry (*)) . zip [1..] . map (\(h, c, b) -> b) .
           sortBy (compare `on` (\(h, cs, b) -> (h, cs)))

upgrade :: Int -> Hand -> Hand
upgrade 5 x            = FiveOfAKind
upgrade 4 x            = FiveOfAKind
upgrade 3 FullHouse    = FiveOfAKind
upgrade 3 x            = FourOfAKind
upgrade 2 FullHouse    = FiveOfAKind
upgrade 2 TwoPair      = FourOfAKind
upgrade 2 OnePair      = ThreeOfAKind
upgrade 1 FourOfAKind  = FiveOfAKind
upgrade 1 ThreeOfAKind = FourOfAKind
upgrade 1 TwoPair      = FullHouse
upgrade 1 OnePair      = ThreeOfAKind
upgrade 1 HighCard     = OnePair
upgrade 0 x            = x

jokerify (t, cs, b) = (upgrade numJokers t, cs', b)
  where cs' = [if c == Jack then Joker else c | c <- cs]
        numJokers = length [1 | c <- cs', c == Joker]

main = do
  input <- parse <$> getContents
  putStrLn $ show $ winnings input
  putStrLn $ show $ winnings $ map jokerify input
