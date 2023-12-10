import Debug.Trace
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

part1 (start, grid) = (length x + 1) `div` 2
  where Just x = msum $ map (\d -> explore grid (step d start) d) [U, D, L, R]

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

-- TODO: Share work between parts 1 and 2.
part2 (start, grid) = length rs'  -- debug $ clean // (map (\c -> (c, 'O')) rs')
  where Just (d, cs) = msum $ map consider [U, D, L, R]
        consider d = explore grid (step d start) d >>= (\cs -> return (d, cs))
        startPipe = findStartPipe d cs
        -- Build a new grid which only contains pipes that are part of the wall.
        empty = array (bounds grid) (zip (indices grid) (repeat '.'))
        clean = empty // (map (\c -> (c, grid ! c)) cs ++ [(start, startPipe)])
        -- For debugging, print out a grid
        ((1, 1), (w, h)) = bounds grid
        nearEdge (x, y) = x == 1 || y == 1 || x == w + 1 || y == h + 1
        debug g = unlines [[g ! (x, y) | x <- [1..w]] | y <- [1..h]]
        fill :: Coord -> StateT (Set Coord) Maybe ()
        fill pos = do
          trace ("fill " ++ show pos) return ()
          seen <- Set.member pos <$> get
          if seen then
            return ()
          else if nearEdge pos then
            fail "At edge"
          else do
            modify (Set.insert pos)
            let ns = catMaybes (map (squeeze clean pos) [U, D, L, R])
            trace ("neighbours " ++ show ns) return ()
            mapM fill ns
            return ()
        runFill :: Coord -> Maybe (Set Coord)
        runFill c = snd <$> runStateT (fill c) Set.empty
        -- Find the set of reachable vertices.
        Just rs = msum $ map runFill [start, step D start, step R start, step R $ step D start]
        -- Restrict the set to the open spaces.
        rs' = filter (\c -> clean ! c == '.') (Set.elems rs)

-- part 2: 7464 too high
main = do
  input <- parse <$> getContents
  putStrLn $ show $ part1 input
  putStrLn $ show $ part2 input
