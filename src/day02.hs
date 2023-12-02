import Data.Char
import Data.List
import Text.Parsec
import Text.Parsec.String

data Color = Red | Green | Blue deriving Eq
type Round = (Int, Int, Int)  -- (red, green, blue)
type Game = (Int, [Round])    -- (id, rounds)

parseColor :: Parser Color
parseColor = choice [string "red"   >> return Red,
                     string "green" >> return Green,
                     string "blue"  >> return Blue]

nat :: Parser Int
nat = foldl' (\a i -> 10 * a + digitToInt i) 0 <$> many1 digit

parseCount :: Parser (Color, Int)
parseCount = do
  n <- nat
  string " "
  c <- parseColor
  return (c, n)

parseRound :: Parser Round
parseRound = do
  ncs <- parseCount `sepBy` string ", "
  let totals x [] = x
      totals (r, g, b) ((Red, x)   : xs) = totals (r + x, g, b) xs
      totals (r, g, b) ((Green, x) : xs) = totals (r, g + x, b) xs
      totals (r, g, b) ((Blue, x)  : xs) = totals (r, g, b + x) xs
  return (totals (0, 0, 0) ncs)

parseGame :: Parser Game
parseGame = do
  string "Game "
  n <- nat
  string ": "
  rs <- parseRound `sepBy` string "; "
  char '\n'
  return (n, rs)

parseInput :: Parser [Game]
parseInput = many parseGame

power :: Game -> Int
power (_, rs) = r * g * b
  where (r, g, b) = foldl' rmax (0, 0, 0) rs
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
