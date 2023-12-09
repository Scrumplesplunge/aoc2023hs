parse :: String -> [[Int]]
parse = map (map read . words) . lines

extrapolate :: [Int] -> Int
extrapolate ys
  | all (== 0) ys = 0
  | otherwise     = last ys + extrapolate (zipWith (-) (tail ys) ys)

main = do
  input <- parse <$> getContents
  putStrLn $ show $ sum $ map extrapolate input
  putStrLn $ show $ sum $ map (extrapolate . reverse) input
