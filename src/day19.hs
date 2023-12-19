import Text.Parsec
import Text.Parsec.String
import Data.Char
import Data.List

data Cond = Lt Char Int | Gt Char Int | Always deriving Show
data Action = Accept | Reject | Jump String deriving Show
-- Name of the workflow, plus a list of conditions and actions where the last
-- condition must be `Always`.
type Workflow = (String, [(Cond, Action)])
-- Abstract representation of a part with an inclusive range of values for each
-- of the categories.
data Part = Part (Int, Int) (Int, Int) (Int, Int) (Int, Int) deriving Show

-- Map one part to another by modifying the value of one category.
pmap :: Char -> ((Int, Int) -> (Int, Int)) -> Part -> Part
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

part :: Parser Part
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
  return (Part (x, x) (m, m) (a, a) (s, s))

parseInput :: String -> ([Workflow], [Part])
parseInput = finish . break (null . snd) . zip [1..] . lines
  where finish :: ([(Int, String)], [(Int, String)]) -> ([Workflow], [Part])
        finish (ws, (_, "") : ps) = (map (f workflow) ws, map (f part) ps)
        f :: Parser a -> (Int, String) -> a
        f p (n, x) = case parse p ("input:" ++ show n) x of
          Left e -> error (show e)
          Right x -> x

possible :: Part -> Bool
possible (Part x m a s) = not (any (\(l, u) -> u < l) [x, m, a, s])

-- Splits a value into options that evaluate to false and options that evaluate
-- to true under the given condition.
fork :: Cond -> Part -> (Part, Part)
fork Always p = (Part (2, 1) (2, 1) (2, 1) (2, 1), p)
fork (Lt c n) p = (pmap c (\(l, u) -> (max l n, u)) p,        -- c >= n
                   pmap c (\(l, u) -> (l, min u (n - 1))) p)  -- c < n
fork (Gt c n) p = (pmap c (\(l, u) -> (l, min u n)) p,        -- c <= n
                   pmap c (\(l, u) -> (max l (n + 1), u)) p)  -- c > n

-- Given an initial abstract part, returns a list of disjoint abstract parts
-- which are each a subset of the input and all satisfy the workflow.
eval :: [Workflow] -> Part -> [Part]
eval ws = check (get "in")
  where get w = case lookup w ws of
          Nothing -> error "Bad workflow reference"
          Just x -> x
        check :: [(Cond, Action)] -> Part -> [Part]
        check ((c, a) : xs) p | possible p = let (f, t) = fork c p
                                             in check xs f ++ act a t
                              | otherwise  = []
        check [] p | possible p = error "Fell off the workflow!"
                   | otherwise  = []
        act :: Action -> Part -> [Part]
        act Accept p | possible p = [p]
                     | otherwise = []
        act Reject p = []
        act (Jump w) p = check (get w) p

part1 :: ([Workflow], [Part]) -> Int
part1 (ws, ps) = sum $ map score $ filter (not . null . eval ws) ps
  where score (Part (x, _) (m, _) (a, _) (s, _)) = x + m + a + s

part2 :: ([Workflow], [Part]) -> Int
part2 (ws, ps) = sum $ map count $ eval ws (Part any any any any)
  where any = (1, 4000)
        count p@(Part x m a s) = size x * size m * size a * size s
        size (l, u) = max 0 (u - l + 1)

main = do
  input <- parseInput <$> getContents
  putStrLn $ show $ part1 input
  putStrLn $ show $ part2 input
