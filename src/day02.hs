import Data.Char
import Data.Either
import Data.List
import Text.Parsec
import Text.Parsec.String

data Round = Round !Int !Int !Int  -- (red, green, blue)
type Game = (Int, [Round])         -- (id, rounds)

nat :: Parser Int
nat = foldl' (\a i -> 10 * a + digitToInt i) 0 <$> many1 digit

parseCount :: Parser (Round -> Round)
parseCount = do
  x <- nat
  string " "
  choice [string "red"   >> return (\(Round r g b) -> (Round (r + x) g  b)),
          string "green" >> return (\(Round r g b) -> (Round r (g + x) b)),
          string "blue"  >> return (\(Round r g b) -> (Round r g (b + x)))]

parseRound :: Parser Round
parseRound = do
  ncs <- parseCount `sepBy` string ", "
  return (foldl' (\x f -> f x) (Round 0 0 0) ncs)

parseGame :: Parser Game
parseGame = do
  string "Game "
  n <- nat
  string ": "
  rs <- parseRound `sepBy` string "; "
  return (n, rs)

part1, part2 :: Game -> Int
part1 (id, rs) = if all possible rs then id else 0
  where possible (Round r g b) = r <= 12 && g <= 13 && b <= 14
part2 (_, rs) = r * g * b
  where Round r g b = foldl' rmax (Round 0 0 0) rs
        rmax (Round r g b) (Round r' g' b') =
          Round (max r r') (max g g') (max b b')

data Sums = Sums !Int !Int
solve = go (Sums 0 0)
  where go s [] = s
        go (Sums p1 p2) (g : gs) = go (Sums (p1 + part1 g) (p2 + part2 g)) gs

main = do
  ls <- lines <$> getContents
  let input = map (fromRight (error "parse error") . parse parseGame "input") ls
      (Sums p1 p2) = solve input
  putStrLn $ show $ p1
  putStrLn $ show $ p2
