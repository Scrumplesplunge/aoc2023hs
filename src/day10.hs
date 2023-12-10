import Control.Monad
import Control.Monad.State
import Data.Array
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
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

-- Returns true if it is possible to move in the given direction starting at the
-- top left corner of the given cell.
canSqueeze :: Grid -> Coord -> Direction -> Bool
canSqueeze grid (x, y) U = not (a `elem` "LF-" && b `elem` "J7-")
  where a = grid ! (x - 1, y - 1)
        b = grid ! (x, y - 1)
canSqueeze grid (x, y) D = not (a `elem` "LF-" && b `elem` "J7-")
  where a = grid ! (x - 1, y)
        b = grid ! (x, y)
canSqueeze grid (x, y) L = not (a `elem` "|7F" && b `elem` "|JL")
  where a = grid ! (x - 1, y - 1)
        b = grid ! (x - 1, y)
canSqueeze grid (x, y) R = not (a `elem` "|7F" && b `elem` "|JL")
  where a = grid ! (x, y - 1)
        b = grid ! (x, y)

squeeze grid coord dir =
  if canSqueeze grid coord dir then Just (step dir coord) else Nothing

main = do
  (start, grid) <- parse <$> getContents
  -- Find the loop.
  let consider d = explore grid (step d start) d >>= (\cs -> return (d, cs))
      Just (d, cs) = msum $ map consider [U, D, L, R]
  putStrLn $ show $ (length cs + 1) `div` 2
  -- Build a clean grid which only contains the loop.
  let startPipe = findStartPipe d cs
      empty = array (bounds grid) (zip (indices grid) (repeat '.'))
      clean = empty // (map (\c -> (c, grid ! c)) cs ++ [(start, startPipe)])
  -- Fill the contained area.
  let ((1, 1), (w, h)) = bounds grid
      nearEdge (x, y) = x == 1 || y == 1 || x == w + 1 || y == h + 1
      fill :: Coord -> StateT (Set Coord) Maybe ()
      fill pos = do
        seen <- Set.member pos <$> get
        if seen then
          return ()
        else if nearEdge pos then
          fail "At edge"
        else do
          modify (Set.insert pos)
          let ns = catMaybes (map (squeeze clean pos) [U, D, L, R])
          mapM fill ns
          return ()
      runFill :: Coord -> Maybe (Set Coord)
      runFill c = snd <$> runStateT (fill c) Set.empty
      starts = concat [[p, step D p] | p <- [start, step R start]]
      -- Find the set of reachable vertices.
      Just vs = msum $ map runFill starts
      -- Restrict the set to the open spaces.
      es = filter (\v -> clean ! v == '.') (Set.elems vs)
  putStrLn $ show $ length es
