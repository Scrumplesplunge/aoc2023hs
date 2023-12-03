import Control.Monad.State
import Data.Char
import Data.List

-- expandX maps a list bools to another list of bools where bool i is true if
-- position i is within 1 space of a position that was true in the input, e.g.
--      #...#..
--   -> ##.###.
expandX :: (Ord a, Bounded a) => [a] -> [a]
expandX = go . (minBound :)
  where go [_] = []
        go xs = maximum (take 3 xs) : go (tail xs)

expand :: (Ord a, Bounded a) => [[a]] -> [[a]]
expand = transpose . map expandX . transpose . map expandX

part1 input = sum . map (sum . row) $ grid
  where isSym c = c /= '.' && not (isDigit c)
        grid = zipWith zip input $ expand $ map (map isSym) input
        row [] = []
        row xs = let (ns, xs') = span (isDigit . fst) xs
                     isPartNumber = any snd ns
                     n = foldl' (\a x -> 10 * a + digitToInt x) 0 (map fst ns)
                     xs'' = dropWhile (not . isDigit . fst) xs'
                 in if isPartNumber then n : row xs'' else row xs''

part2 input = sumGroups . filter ((/= 0) . fst) . concat . map row $ grid'
  where nextId :: State Int Int
        nextId = do
          id <- get
          modify (+1)
          return id
        makeRow :: String -> State Int [Int]
        makeRow [] = return []
        makeRow ('*' : xs) = do
          id <- nextId
          rest <- makeRow xs
          return (id : rest)
        makeRow (x : xs) = makeRow xs >>= (return . (0 :))
        makeGrid :: [String] -> State Int [[Int]]
        makeGrid input = mapM makeRow input
        grid :: [[Int]]
        grid = fst $ runState (makeGrid input) 1
        grid' = zipWith zip input $ expand grid
        row :: [(Char, Int)] -> [(Int, Int)]
        row [] = []
        row xs = let (ns, xs') = span (isDigit . fst) xs
                     id = maximum (map snd ns)
                     n = foldl' (\a x -> 10 * a + digitToInt x) 0 (map fst ns)
                     xs'' = dropWhile (not . isDigit . fst) xs'
                 in if null ns then row xs'' else (id, n) : row xs''
        groupIds = groupBy (\a b -> fst a == fst b) . sortOn fst
        sumGroups =
          sum . map (product . map snd) . filter ((>1) . length) . groupIds

main = do
  input <- lines <$> getContents
  putStrLn $ show $ part1 input
  putStrLn $ show $ part2 input
