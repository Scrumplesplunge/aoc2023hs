import Control.Monad
import Data.Array
import Data.Tuple

type Coord = (Int, Int)
type Grid = Array Coord Char
parse :: String -> (Coord, Grid)
parse input = (start, array ((1, 1), (w, h)) cells)
  where ls = map row $ zip [1..] (lines input)
        row (y, l) = zip [(x, y) | x <- [1..]] l
        w = length (head ls)
        h = length ls
        cells = concat ls
        start = case lookup 'S' (map swap cells) of
                  Nothing -> error "No start location"
                  Just c -> c

data Direction = U | D | L | R

-- Given a pipe and the direction we entered it from, emit the exit direction.
follow :: Char -> Direction -> Maybe Direction
follow '|' U = Just U
follow '|' D = Just D
follow '-' L = Just L
follow '-' R = Just R
follow 'L' D = Just R
follow 'L' L = Just U
follow 'J' D = Just L
follow 'J' R = Just U
follow '7' R = Just D
follow '7' U = Just L
follow 'F' U = Just R
follow 'F' L = Just D
follow _   _ = Nothing

step :: Coord -> Direction -> Coord
step (x, y) U = (x, y - 1)
step (x, y) D = (x, y + 1)
step (x, y) L = (x - 1, y)
step (x, y) R = (x + 1, y)

check :: Grid -> Coord -> Maybe Coord
check grid coord = if inRange (bounds grid) coord then Just coord else Nothing

-- Follow a pipe until we get back to the start cell (or Nothing otherwise).
explore :: Grid -> Direction -> Coord -> Maybe [(Coord, Char)]
explore grid startDir = go [] startDir
  where go :: [(Coord, Char)] -> Direction -> Coord -> Maybe [(Coord, Char)]
        go cs dir pos = do
          let c = grid ! pos
          case c of
            'S' -> return ((pos, inferPipe dir startDir) : cs)
            _ -> do
              dir' <- follow c dir
              pos' <- check grid $ step pos dir'
              go ((pos, c) : cs) dir' pos'

inferPipe :: Direction -> Direction -> Char
inferPipe U U = '|'
inferPipe U L = '7'
inferPipe U R = 'F'
inferPipe D D = '|'
inferPipe D L = 'J'
inferPipe D R = 'L'
inferPipe L U = 'L'
inferPipe L L = '-'
inferPipe L D = 'F'
inferPipe R U = 'J'
inferPipe R R = '-'
inferPipe R D = '7'

-- Count the number of cells in a row which are inside the loop.
count :: String -> Int
count = sum . odds . go 0
  where go n [] = [n]
        go n ('|' : xs) = n : go 0 xs
        go n ('L' : xs) = top n xs
        go n ('F' : xs) = bottom n xs
        go n (_   : xs) = go (n + 1) xs
        top n ('-' : xs) = top n xs
        top n ('7' : xs) = n : go 0 xs
        top n ('J' : xs) = go n xs
        bottom n ('-' : xs) = bottom n xs
        bottom n ('7' : xs) = go n xs
        bottom n ('J' : xs) = n : go 0 xs
        odds (x : y : xs) = y : odds xs
        odds _ = []

main = do
  (start, grid) <- parse <$> getContents
  -- Find the loop.
  let Just cs = msum $ map (explore grid <*> step start) [U, D, L, R]
  putStrLn $ show $ (length cs + 1) `div` 2
  -- Build a clean grid which only contains the loop.
  let b@(_, (w, h)) = bounds grid
      clean = array b (zip (indices grid) (repeat '.')) // cs
      rows = [[clean ! (x, y) | x <- [1..w]] | y <- [1..h]]
  putStrLn $ show $ sum $ map count $ rows
