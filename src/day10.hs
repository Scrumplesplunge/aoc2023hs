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

step :: Direction -> Coord -> Coord
step U (x, y) = (x, y - 1)
step D (x, y) = (x, y + 1)
step L (x, y) = (x - 1, y)
step R (x, y) = (x + 1, y)

check :: Grid -> Coord -> Maybe Coord
check grid coord = if inRange (bounds grid) coord then Just coord else Nothing

-- Follow a pipe until we get back to the start cell (or Nothing otherwise).
explore :: Grid -> Coord -> Direction -> Maybe [Coord]
explore = go []
  where go cs grid pos dir = do
          let c = grid ! pos
          case c of
            'S' -> return cs
            _ -> do
              dir' <- follow c dir
              pos' <- check grid $ step dir' pos
              go (pos : cs) grid pos' dir'

-- Given the direction of the entrance to the loop and the list of cells that
-- compose the loop itself (not including the start position), infer the shape
-- of the piece of pipe at the start position.
findStartPipe :: Direction -> [Coord] -> Char
findStartPipe d cs = result
  where (ax, ay) = last cs
        (bx, by) = head cs
        delta = (bx - ax, by - ay)
        result = case (d, bx - ax, by - ay) of
          (U,  0,  2) -> '|'
          (U, -1,  1) -> 'J'
          (U,  1,  1) -> 'L'
          (D,  0, -2) -> '|'
          (D, -1, -1) -> '7'
          (D,  1, -1) -> 'F'
          (L,  2,  0) -> '-'
          (L,  1, -1) -> 'J'
          (L,  1,  1) -> '7'
          (R, -2,  0) -> '-'
          (R, -1, -1) -> 'L'
          (R, -1,  1) -> 'F'

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
  let consider d = explore grid (step d start) d >>= (\cs -> return (d, cs))
      Just (d, cs) = msum $ map consider [U, D, L, R]
  putStrLn $ show $ (length cs + 1) `div` 2
  -- Build a clean grid which only contains the loop.
  let startPipe = findStartPipe d cs
      b@(_, (w, h)) = bounds grid
      empty = array b (zip (indices grid) (repeat '.'))
      clean = empty // ((start, startPipe) : map (\c -> (c, grid ! c)) cs)
      rows = [[clean ! (x, y) | x <- [1..w]] | y <- [1..h]]
      inside = sum $ map count $ rows
  putStrLn $ show $ inside
