import Data.List

labels :: Int -> [String] -> [Int]
labels expansion = go 0
  where go n [] = []
        go n (r : rs) =
          n : go (n + if all (== '.') r then expansion + 1 else 1) rs

stars :: Int -> [String] -> [(Int, Int)]
stars e rs = [(x, y) | (y, r) <- zip ys rs, (x, c) <- zip xs r, c == '#']
  where ys = labels e rs
        xs = labels e (transpose rs)

dist (ax, ay) (bx, by) = abs (ax - bx) + abs (ay - by)

dists :: Int -> [String] -> Int
dists n input = sum [dist a b | (a, r) <- zip s (tail (tails s)), b <- r]
  where s = stars n $ input

main = do
  input <- lines <$> getContents
  putStrLn . show . dists 1 $ input
  putStrLn . show . dists 999999 $ input
