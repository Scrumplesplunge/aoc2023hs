parse :: String -> ([String], [String])
parse = (\[ts, ds] -> (ts, ds)) . map (tail . words) . lines

-- Integer square root.
isqrt :: Integer -> Integer
isqrt x = go 1 x
  where go a b = let m = (a + b) `div` 2 in
                 if a == m then a
                 else if m * m <= x then go m b
                 else go a m

-- For a race of length t ms where we held the button for s ms, the distance
-- we travel is x = (t - s) * s = ts - s^2. So, the range where we beat the
-- record is the part of y = x - d = ts - s^2 - d that is greater than 0. We can
-- solve for the roots of y using the quadratic formula and make sure to round
-- in the right direction.
ways :: (Int, Int) -> Int
ways (t, d) = high - low + 1
  where rdet = fromInteger $ isqrt $ (toInteger t)^2 - 4 * toInteger d
        -- Add 1 for the low bound so that the division rounds up.
        low = (t - rdet + 1) `div` 2
        high = (t + rdet) `div` 2

part1 = product . map ways . (\(ts, ds) -> zip (map read ts) (map read ds))
part2 = ways . (\(ts, ds) -> (read (concat ts), read (concat ds)))

main = do
  rs <- parse <$> getContents
  putStrLn $ show $ part1 rs
  putStrLn $ show $ part2 rs
