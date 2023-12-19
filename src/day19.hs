import Text.Parsec
import Text.Parsec.String
import Data.Char
import Data.List

data Cond = Lt Char Int | Gt Char Int | Always deriving Show
data Action = Accept | Reject | Jump String deriving Show
type Workflow = (String, [(Cond, Action)])
data Part a = Part a a a a deriving Show

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

data Value = None | Below Int | AtLeast Int | Between Int Int | Any
  deriving Show
ne = Between 1 4001

lessThan :: Int -> Value -> Value
lessThan _  None          = None
lessThan u' (Below u)     = Below (min u u')
lessThan u  (AtLeast l)   = if l < u then Between l u else None
lessThan u' (Between l u) = if l < u' then Between l (min u u') else None
lessThan u  Any           = Below u

greaterThan :: Int -> Value -> Value
greaterThan _  None          = None
greaterThan l  (Below u)     = if l + 1 < u then Between (l + 1) u else None
greaterThan l' (AtLeast l)   = AtLeast (max l (l' + 1))
greaterThan l' (Between l u) = if l' + 1 < u then Between (max l (l' + 1)) u else None
greaterThan l  Any           = AtLeast (l + 1)

pmap :: Char -> (a -> a) -> Part a -> Part a
pmap 'x' f (Part x m a s) = Part (f x) m a s
pmap 'm' f (Part x m a s) = Part x (f m) a s
pmap 'a' f (Part x m a s) = Part x m (f a) s
pmap 's' f (Part x m a s) = Part x m a (f s)

possible :: Part Value -> Bool
possible (Part None _ _ _) = False
possible (Part _ None _ _) = False
possible (Part _ _ None _) = False
possible (Part _ _ _ None) = False
possible _ = True

fork :: Cond -> Part Value -> (Part Value, Part Value)
fork Always p = (Part None None None None, p)
fork (Lt c n) p = (pmap c (greaterThan (n - 1)) p,
                   pmap c (lessThan n) p)
fork (Gt c n) p = (pmap c (lessThan (n + 1)) p,
                   pmap c (greaterThan n) p)

-- part2 :: ([Workflow], [Part Int]) -> Int
part2 (ws, ps) = sum $ map count $ check (get "in") (Part ne ne ne ne)
  where get w = case lookup w ws of
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
        count p@(Part x m a s) =
          if finite x && finite m && finite a && finite s then
            size x * size m * size a * size s
          else
            error ("Infinite part: " ++ show p)
        finite (Between l u) = True
        finite _ = False
        size (Between l u) = u - l
        size _ = error "Infinite range"

main = do
  input <- parseInput <$> getContents
  putStrLn $ show $ part1 input
  putStrLn $ show $ part2 input
