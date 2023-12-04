import Data.List

-- Parse the number of winning numbers from each line.
parse :: String -> [Int]
parse = map (wins . break ((== '|') . head) . words . drop 10) . lines
  where wins (ws, ("|" : ns)) = length (ws `intersect` ns)

-- Sum the scores for each card.
part1 :: [Int] -> Int
part1 = let score n = if n == 0 then 0 else 2 ^ (n - 1) in sum . map score

-- Iterate backwards to build a list where the ith element contains the number
-- of cards won transitively by a single copy of the ith card.
part2 :: [Int] -> Int
part2 = sum . foldr (\w cs -> 1 + sum (take w cs) : cs) []

main = do
  cs <- parse <$> getContents
  putStrLn $ show $ part1 cs
  putStrLn $ show $ part2 cs
