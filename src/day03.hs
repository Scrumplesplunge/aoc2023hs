import Control.Monad.State
import Data.Char
import Data.List

-- expandX and expand are convolution filters which make each cell the maximum
-- of itself and its immediate neighbours.
expandX :: (Ord a, Bounded a) => [a] -> [a]
expandX = go . (minBound :)
  where go [_] = []
        go xs@(_ : xs') = maximum (take 3 xs) : go xs'
expand :: (Ord a, Bounded a) => [[a]] -> [[a]]
expand = transpose . map expandX . transpose . map expandX

int :: String -> Int
int = foldl' (\a x -> 10 * a + digitToInt x) 0

part1 input = sum . map (sum . rowIds) $ grid
  where isSym c = c /= '.' && not (isDigit c)
        grid = zipWith zip input $ expand $ map (map isSym) input
        rowIds [] = []
        rowIds xs =
          let (ns, xs') = span (isDigit . fst) xs
              isPartNumber = any snd ns
              n = int (map fst ns)
              xs'' = dropWhile (not . isDigit . fst) xs'
          in if isPartNumber then n : rowIds xs'' else rowIds xs''

part2 input = sumGroups . filter ((/= 0) . fst) . concat . map rowIds $ grid
  where -- Build a grid where each '*' is annotated with a unique ID starting at
        -- 1, and each character is annotated with the (assumed unique) adjacent
        -- '*'.
        getId :: Char -> State Int Int
        getId '*' = do { id <- get; modify (+1); return id }
        getId _ = return 0
        grid :: [[(Char, Int)]]
        grid = zipWith zip input $ expand $ fst $
               runState (mapM (mapM getId) input) 1
        -- Parse the part numbers in a line, tagged with gear IDs.
        rowIds :: [(Char, Int)] -> [(Int, Int)]
        rowIds [] = []
        rowIds xs = let (ns, xs') = span (isDigit . fst) xs
                        id = maximum (map snd ns)
                        n = int (map fst ns)
                        xs'' = dropWhile (not . isDigit . fst) xs'
                    in if null ns then rowIds xs'' else (id, n) : rowIds xs''
        -- Group the part numbers by the corresponding gear.
        groupIds = groupBy (\a b -> fst a == fst b) . sortOn fst
        -- Calculate the product for each gear with exactly two part numbers and
        -- sum the results.
        sumGroups =
          sum . map (product . map snd) . filter ((>1) . length) . groupIds

main = do
  input <- lines <$> getContents
  putStrLn $ show $ part1 input
  putStrLn $ show $ part2 input
