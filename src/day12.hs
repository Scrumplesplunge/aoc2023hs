import Data.Char
import Data.List
import Data.Array

parse :: String -> [(String, [Int])]
parse = map (entry . words) . lines
  where entry [line, counts] = (line, nums 0 counts)
        nums n (',' : xs) = n : nums 0 xs
        nums n (d   : xs) = nums (10 * n + digitToInt d) xs
        nums n [] = [n]

arrange :: String -> [Int] -> Int
arrange l cs = a ! (0, 0)
  where (n, m) = (length l, length cs)
        line = array (0, n - 1) $ zip [0..] l
        counts = array (0, m - 1) $ zip [0..] cs
        bad i = line ! i == '#'
        -- `a ! (i, j)` is the arrangement count for `drop i l` and `drop j cs`.
        a = array ((0, 0), (n, m)) [((i, j), f i j) | j <- [0..m], i <- [0..n]]
              -- Empty line: possible only if the list of counts is empty.
        f i j | i == n          = if j == m then 1 else 0
              -- Empty list of counts: impossible if we have any '#'.
              | j == m          = if bad i then 0 else a ! (i + 1, j)
              -- Empty space in the line: skip to the next.
              | line ! i == '.' = a ! (i + 1, j)
              -- Otherwise, consider whether the first count is matched by
              -- a sequence starting at i or not.
              | otherwise       = here + after
          where c = counts ! j
                i' = i + c
                -- True if a sequence of `c` bad entries can fit before the next
                -- empty space.
                canFit = i' <= n && not (any (\i -> line ! i == '.') [i..i+c-1])
                -- True if a sequence starting at `i` would stop at `i + c`.
                canStop = i' == n || line ! i' /= '#'
                poss = canFit && canStop
                -- The number of arrangements where the jth sequence starts at
                -- position i.
                here = if poss then a ! (min (i' + 1) n, j + 1) else 0
                -- The number of arrangements where the jth sequence starts
                -- strictly after position i.
                after = if line ! i == '?' then a ! (i + 1, j) else 0

unfold :: (String, [Int]) -> (String, [Int])
unfold (l, c) = (intercalate "?" (replicate 5 l), concat (replicate 5 c))

main = do
  input <- parse <$> getContents
  putStrLn . show . sum . map (uncurry arrange) $ input
  putStrLn . show . sum . map (uncurry arrange . unfold) $ input
