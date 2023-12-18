import Debug.Trace
import Data.Array
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Function

data Direction = U | D | L | R deriving (Eq, Ord, Read, Show)

parse :: String -> [(Direction, Int)]
parse = map (parseRow . words) . lines
  where parseRow [d, n, _] = (read d, read n)

move :: Direction -> (Int, Int) -> (Int, Int)
move U (x, y) = (x, y - 1)
move D (x, y) = (x, y + 1)
move L (x, y) = (x - 1, y)
move R (x, y) = (x + 1, y)

follow :: [(Direction, Int)] -> (Int, Int) -> [(Int, Int)]
follow [] p = [p]
follow ((d, 0) : xs) p = p : follow xs p
follow ((d, n) : xs) p = p : follow ((d, n - 1) : xs) (move d p)

-- part1 :: [(Direction, Int)] -> Int
-- 99811 too high
part1 input = trace debug $ (maxX - minX + 1) * (maxY - minY + 1) - length out
  where path = follow input (0, 0)
        minX = minimum (map fst path) - 1
        maxX = maximum (map fst path) + 1
        minY = minimum (map snd path) - 1
        maxY = maximum (map snd path) + 1
        range = ((minX, minY), (maxX, maxY))
        rows = map (sort . map fst) $ groupBy ((==) `on` snd) $ sortOn snd $ path
        debugR row = [if x `elem` row then '#' else '.' | x <- [minX .. maxX]]
        empty = array range
                      [((x, y), '.') | x <- [minX..maxX], y <- [minY..maxY]]
        edges = empty // map (\p -> (p, '#')) path
        explore :: Set (Int, Int) -> (Int, Int) -> Set (Int, Int)
        explore seen p =
          if p `Set.member` seen || not (inRange range p) ||
             edges ! p == '#' then
            seen
          else
            let ns = map (flip move p) [U, D, L, R]
                seen' = Set.insert p seen
            in foldl' explore seen' ns
        out = explore Set.empty (minX, minY)
        outside = edges // map (\p -> (p, 'O')) (Set.elems $ explore Set.empty (minX, minY))
        showGrid a = [[a ! (x, y) | x <- [minX..maxX]] | y <- [minY..maxY]]
        debug = (unlines $ map show input) ++ (unlines $ zipWith (\l r -> l ++ "  " ++ r) (showGrid edges) (showGrid outside))
        fillOut as [] = []
        fillOut ('O' : as) ('.' : bs) = 'O' : fillOut as bs
        fillRow [] = 0
        fillRow xs@(x : _) =
          let (x', xs') = adjacent $ snd $ adjacent xs
          in (x' - x) + fillRow xs'
        adjacent [x] = (x + 1, [])
        adjacent (x1 : xs@(x2 : _))
          | x1 + 1 == x2 = adjacent xs
          | otherwise    = (x1 + 1, xs)
        adjacent xs = error ("welp, " ++ show xs)

main = do
  input <- parse <$> getContents
  putStrLn $ show $ part1 input
