import Text.Parsec
import Text.Parsec.String
import Data.Char
import Data.List

data Cond = Lt Char Int | Gt Char Int | Always deriving Show
data Action = Accept | Reject | Jump String deriving Show
type Workflow = (String, [(Cond, Action)])
data Part a = Part a a a a deriving Show

-- Map one part to another by modifying the value of one category.
pmap :: Char -> (a -> a) -> Part a -> Part a
pmap 'x' f (Part x m a s) = Part (f x) m a s
pmap 'm' f (Part x m a s) = Part x (f m) a s
pmap 'a' f (Part x m a s) = Part x m (f a) s
pmap 's' f (Part x m a s) = Part x m a (f s)

int :: Parser Int
int = foldl' (\n x -> 10 * n + digitToInt x) 0 <$> many1 digit

cond = do
  v <- oneOf "xmas"
  c <- oneOf "<>"
  n <- int
  case c of
    '<' -> return (Lt v n)
    '>' -> return (Gt v n)

action = choice [char 'A' >> return Accept,
                 char 'R' >> return Reject,
                 many1 lower >>= (return . Jump)]

branch = do
  c <- cond
  char ':'
  a <- action
  return (c, a)

workflow :: Parser Workflow
workflow = do
  name <- many1 lower
  char '{'
  cs <- try branch `sepEndBy` char ','
  a <- action
  char '}'
  return (name, cs ++ [(Always, a)])

part :: Parser (Part Int)
part = do
  string "{x="
  x <- int
  string ",m="
  m <- int
  string ",a="
  a <- int
  string ",s="
  s <- int
  string "}"
  return (Part x m a s)

parseInput :: String -> ([Workflow], [Part Int])
parseInput = finish . break (null . snd) . zip [1..] . lines
  where finish :: ([(Int, String)], [(Int, String)]) -> ([Workflow], [Part Int])
        finish (ws, (_, "") : ps) = (map (f workflow) ws, map (f part) ps)
        f :: Parser a -> (Int, String) -> a
        f p (n, x) = case parse p ("input:" ++ show n) x of
          Left e -> error (show e)
          Right x -> x

part1 :: ([Workflow], [Part Int]) -> Int
part1 (ws, ps) = sum $ map score $ filter (check (get "in")) ps
  where score (Part x m a s) = x + m + a + s
        get w = case lookup w ws of
          Nothing -> error "Bad workflow reference"
          Just x -> x
        cat 'x' (Part x m a s) = x
        cat 'm' (Part x m a s) = m
        cat 'a' (Part x m a s) = a
        cat 's' (Part x m a s) = s
        check :: [(Cond, Action)] -> Part Int -> Bool
        check ((Always, a) : _) p = act a p
        check ((Lt c n, a) : xs) p = if cat c p < n then act a p else check xs p
        check ((Gt c n, a) : xs) p = if cat c p > n then act a p else check xs p
        act :: Action -> Part Int -> Bool
        act Accept p = True
        act Reject p = False
        act (Jump w) p = check (get w) p

type Value = (Int, Int)  -- Inclusive range.

possible :: Part Value -> Bool
possible (Part x m a s) = not (any (\(l, u) -> u < l) [x, m, a, s])

-- Splits a value into options that evaluate to false and options that evaluate
-- to true under the given condition.
fork :: Cond -> Part Value -> (Part Value, Part Value)
fork Always p = (Part (2, 1) (2, 1) (2, 1) (2, 1), p)
fork (Lt c n) p = (pmap c (\(l, u) -> (max l n, u)) p,        -- c >= n
                   pmap c (\(l, u) -> (l, min u (n - 1))) p)  -- c < n
fork (Gt c n) p = (pmap c (\(l, u) -> (l, min u n)) p,        -- c <= n
                   pmap c (\(l, u) -> (max l (n + 1), u)) p)  -- c > n

-- part2 :: ([Workflow], [Part Int]) -> Int
part2 (ws, ps) = sum $ map count $ check (get "in") (Part val val val val)
  where val = (1, 4000)
        get w = case lookup w ws of
          Nothing -> error "Bad workflow reference"
          Just x -> x
        check :: [(Cond, Action)] -> Part Value -> [Part Value]
        check ((c, a) : xs) p | possible p = let (f, t) = fork c p
                                             in check xs f ++ act a t
                              | otherwise  = []
        check [] p | possible p = error "Fell off the workflow!"
                   | otherwise  = []
        act :: Action -> Part Value -> [Part Value]
        act Accept p = [p]
        act Reject p = []
        act (Jump w) p = check (get w) p
        count p@(Part x m a s) = size x * size m * size a * size s
        size (l, u) = max 0 (u - l + 1)

main = do
  input <- parseInput <$> getContents
  putStrLn $ show $ part1 input
  putStrLn $ show $ part2 input
