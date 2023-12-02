import Text.Parsec
import Text.Parsec.String

data Color = Red | Green | Blue deriving Eq
type Round = (Int, Int, Int)  -- (red, green, blue)
type Game = (Int, [Round])    -- (id, rounds)

parseColor :: Parser Color
parseColor = choice [string "red" >> return Red,
                     string "green" >> return Green,
                     string "blue" >> return Blue]

parseCount :: Parser (Color, Int)
parseCount = do
  n <- read <$> many1 digit
  string " "
  c <- parseColor
  return (c, n)

lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll k [] = []
lookupAll k ((k', v) : xs) | k == k' = v : lookupAll k xs
lookupAll k (_ : xs) = lookupAll k xs

parseRound :: Parser Round
parseRound = do
  ncs <- parseCount `sepBy` string ", "
  let red   = sum (lookupAll Red ncs)
      green = sum (lookupAll Green ncs)
      blue  = sum (lookupAll Blue ncs)
  return (red, green, blue)

parseGame :: Parser Game
parseGame = do
  string "Game "
  n <- read <$> many1 digit
  string ": "
  rs <- parseRound `sepBy` string "; "
  char '\n'
  return (n, rs)

parseInput :: Parser [Game]
parseInput = many parseGame

power :: Game -> Int
power (_, rs) = r * g * b
  where (r, g, b) = foldr rmax (0, 0, 0) rs
        rmax (r, g, b) (r', g', b') = (max r r', max g g', max b b')

part1, part2 :: [Game] -> Int
part1 = sum . map fst . filter (all possible . snd)
  where possible (r, g, b) = r <= 12 && g <= 13 && b <= 14
part2 = sum . map power

main = do
  text <- getContents
  case runParser parseInput () "input" text of
    Left e -> error (show e)
    Right g -> do
      putStrLn $ show $ part1 g
      putStrLn $ show $ part2 g
