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
        a = array ((0, 0), (n, m)) [((i, j), f i j) | i <- [0..n], j <- [0..m]]
        f i j | j == m        = if any (== '#') (drop i l) then 0 else 1
              | i == n        = 0
              | l !! i == '.' = a ! (i + 1, j)
              | otherwise     = here + after
          where c = cs !! j
                (p, s) = splitAt c (drop i l)
                canFit = length p == c && not (any (== '.') p)
                canStop = null s || head s /= '#'
                poss = canFit && canStop
                here = if poss then a ! (n - length (drop 1 s), j + 1) else 0
                after = if l !! i == '?' then a ! (i + 1, j) else 0

part1 :: [(String, [Int])] -> Int
part1 = sum . map (uncurry arrange)

part2 :: [(String, [Int])] -> Int
part2 = sum . map (uncurry arrange) . map unfold
  where unfold (l, c) =
          (intercalate "?" $ replicate 5 l, concat $ replicate 5 c)

main = do
  input <- parse <$> getContents
  putStrLn $ show $ part1 input
  putStrLn $ show $ part2 input
