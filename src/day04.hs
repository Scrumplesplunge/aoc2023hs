import Data.Bits
import Data.List

parse :: String -> [Int]
parse = map card . lines
  where card = wins . break ((== '|') . head) . words . drop 10
        wins (ws, ("|" : ns)) = length (ws `intersect` ns)

part1 :: Int -> Int
part1 n = if n == 0 then 0 else 1 `shiftL` (n - 1)

part2 :: [Int] -> Int
part2 = go 0 . zip [1 | _ <- [0..]]
  where go n [] = n
        go n x@((c, s) : cs) = go (n + c) (acc c s cs)
        acc _ _ [] = []
        acc _ 0 cs = cs
        acc c s ((c', s') : cs) = (c' + c, s') : acc c (s - 1) cs

main = do
  cs <- parse <$> getContents
  putStrLn $ show $ sum $ map part1 cs
  putStrLn $ show $ part2 cs
