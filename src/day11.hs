import Data.List

dists :: Int -> [String] -> Int
dists e input = distY input + distY (transpose input)
  where -- Calculate the sum of vertical distances for all pairs.
        distY = go 0 0 0 . map (\r -> length [c | c <- r, c == '#'])
        -- n is the number of stars strictly above the current row
        -- s is the sum of dy for each star strictly above the current row
        -- total is the running total
        go n s total [] = total
        go n s total (0 : rs) = go n (s + n * e) total rs
        go n s total (r : rs) = go (n + r) (s + n + r) (total + s * r) rs

main = do
  input <- lines <$> getContents
  putStrLn . show . dists 2 $ input
  putStrLn . show . dists 1000000 $ input
