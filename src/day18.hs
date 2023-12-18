import Data.Char
import Data.Array
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Function

data Direction = U | D | L | R deriving (Eq, Ord, Read, Show)
type Vec = (Int, Int)

move :: (Direction, Int) -> Vec -> Vec
move (U, n) (x, y) = (x, y - n)
move (D, n) (x, y) = (x, y + n)
move (L, n) (x, y) = (x - n, y)
move (R, n) (x, y) = (x + n, y)

follow :: [(Direction, Int)] -> Vec -> [Vec]
follow [] p = [p]
follow (x : xs) p = p : follow xs (move x p)

-- Condense a set of vertices down to a zoomed out grid (xs, ys, vs) such that
-- each new cell (x, y) represents a rectangle of size (xs !! x) * (ys !! y)
-- and the rectangle is either entirely inside the shape or entirely outside the
-- shape.
condense :: [Vec] -> ([Int], [Int], [Vec])
condense vs = (weights xs, weights ys, map rewrite vs)
  where xs = squash 0 $ sort $ nub $ map fst vs
        ys = squash 0 $ sort $ nub $ map snd vs
        squash :: Int -> [Int] -> Map Int (Int, Int)
        squash x' [] = Map.empty
        squash x' [x] = Map.singleton x (x', 1)
        squash x' (x1 : xs@(x2 : _)) =
          if x2 - x1 > 1 then
            Map.union (Map.fromList [(x1,     (x', 1)),
                                     (x1 + 1, (x' + 1, x2 - x1 - 1))])
                      (squash (x' + 2) xs)
          else Map.insert x1 (x', 1) $ squash (x' + 1) xs
        rewrite (x, y) = (fst (xs Map.! x), fst (ys Map.! y))
        weights = map snd . Map.elems

-- Given a set of vertices with grid-aligned edges, extend it with all the
-- points along the edges.
steps :: [Vec] -> [Vec]
steps ((x1, y1) : vs@((x2, y2) : _))
  | x1 == x2 = [(x1, y) | y <- [min y1 y2 .. max y1 y2]] ++ steps vs
  | y1 == y2 = [(x, y1) | x <- [min x1 x2 .. max x1 x2]] ++ steps vs
  | otherwise = error "Edge is not grid-aligned."
steps vs = vs

-- Calculate the total area of the shape formed by the given set of
-- instructions.
area input = sum xs * sum ys - total
  where (xs, ys, vs) = condense $ follow input (0, 0)
        path = steps vs
        minX = minimum (map fst path) - 1
        maxX = maximum (map fst path) + 1
        minY = minimum (map snd path) - 1
        maxY = maximum (map snd path) + 1
        range = ((minX, minY), (maxX, maxY))
        empty = array range
                      [((x, y), '.') | x <- [minX..maxX], y <- [minY..maxY]]
        edges = empty // map (\p -> (p, '#')) path
        explore :: Set (Int, Int) -> (Int, Int) -> Set (Int, Int)
        explore seen p =
          if p `Set.member` seen || not (inRange range p) ||
             edges ! p == '#' then
            seen
          else
            let ns = map (\d -> move (d, 1) p) [U, D, L, R]
                seen' = Set.insert p seen
            in foldl' explore seen' ns
        outside = edges // map (\p -> (p, 'O'))
                               (Set.elems $ explore Set.empty (minX, minY))
        total = sum [wx * wy | (x, wx) <- zip [minX + 1 .. maxX - 1] xs,
                               (y, wy) <- zip [minY + 1 .. maxY - 1] ys,
                               outside ! (x, y) == 'O']

-- Parse the directions for a single line.
part1, part2 :: String -> (Direction, Int)
part1 = parseRow . words
  where parseRow [d, n, _] = (read d, read n)
part2 = parseRow . words
  where parseRow [_, _, h] = (direction $ take 1 $ drop 7 h,
                              distance $ take 5 $ drop 2 h)
        distance = foldl' (\n d -> 16 * n + digitToInt d) 0
        direction "0" = R
        direction "1" = D
        direction "2" = L
        direction "3" = U
        direction x = error $ "wtf is direction " ++ x

main = do
  input <- lines <$> getContents
  putStrLn $ show $ area $ map part1 input
  putStrLn $ show $ area $ map part2 input
