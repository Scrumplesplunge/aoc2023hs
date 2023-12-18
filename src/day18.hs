import Data.Char
import Debug.Trace
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

-- condense :: [Vec] -> ([Int], [Int], [Vec])
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
        debug = "xs = " ++ show xs ++ "\nys = " ++ show ys ++ "\nvs = " ++ show vs
        rewrite (x, y)
          | not (x `Map.member` xs) = error ("x=" ++ show x ++ " missing\n" ++ debug)
          | not (y `Map.member` ys) = error ("y=" ++ show y ++ " missing\n" ++ debug)
          | otherwise               = (fst (xs Map.! x), fst (ys Map.! y))
        weights = map snd . Map.elems

steps :: [Vec] -> [Vec]
steps ((x1, y1) : vs@((x2, y2) : _))
  | x1 == x2 = [(x1, y) | y <- [min y1 y2 .. max y1 y2]] ++ steps vs
  | y1 == y2 = [(x, y1) | x <- [min x1 x2 .. max x1 x2]] ++ steps vs
  | otherwise = error "Edge is not grid-aligned."
steps vs = vs

-- #####
-- #...#    ###
-- #...# -> #.#
-- #####    ###

-- part1 :: [(Direction, Int)] -> Int
-- 99811 too high
area input = trace debug $ sum xs * sum ys - total
  where (xs, ys, vs) = condense $ follow input (0, 0)
        path = steps vs
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
            let ns = map (\d -> move (d, 1) p) [U, D, L, R]
                seen' = Set.insert p seen
            in foldl' explore seen' ns
        out = explore Set.empty (minX, minY)
        outside = edges // map (\p -> (p, 'O')) (Set.elems $ explore Set.empty (minX, minY))
        total = sum [wx * wy | (x, wx) <- zip [minX + 1 .. maxX - 1] xs,
                               (y, wy) <- zip [minY + 1 .. maxY - 1] ys,
                               outside ! (x, y) == 'O']
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
