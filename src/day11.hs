import Data.List

expandY :: [String] -> [String]
expandY (r : rs) = if all (== '.') r then r : r : expandY rs else r : expandY rs
expandY [] = []
expand = expandY . transpose . expandY . transpose

locate :: [String] -> [((Int, Int), Char)]
locate = concat . zipWith (\y r -> zipWith (\x c -> ((x, y), c)) [1..] r) [1..]

stars :: [((Int, Int), Char)] -> [(Int, Int)]
stars = map fst . filter ((== '#') . snd)

dist (ax, ay) (bx, by) = abs (ax - bx) + abs (ay - by)

part1 input = sum [dist a b | (a, r) <- zip s (tail (tails s)), b <- r]
  where s = stars . locate . expand $ input

main = do
  input <- lines <$> getContents
  putStrLn . show . part1 $ input
