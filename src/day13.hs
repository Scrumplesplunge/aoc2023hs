import Data.List

parse :: String -> [[String]]
parse = grids [] . lines
  where grids :: [String] -> [String] -> [[String]]
        grids g [] = [reverse g]
        grids g ("" : ls) = reverse g : grids [] ls
        grids g (l  : ls) = grids (l : g) ls

rowMirrors :: String -> [Int]
rowMirrors "" = []
rowMirrors (x : xs) = go 1 [x] xs
  where reflected l r = and (zipWith (==) l r)
        go i ls rs@(r:rs') | reflected ls rs = i : go (i + 1) (r : ls) rs'
                           | otherwise       =     go (i + 1) (r : ls) rs'
        go i ls [] = []

mirrors :: [String] -> [Int]
mirrors = foldl1 intersect . map rowMirrors

-- Returns a list of [(smudgePos, [mirrorOffset])]
smudgedRowMirrors :: String -> [(Int, [Int])]
smudgedRowMirrors = zip [0..] . map rowMirrors . options
  where toggle '.' = '#'
        toggle '#' = '.'
        options "" = []
        options (x : xs) = (toggle x : xs) : map (x:) (options xs)

-- Returns a list of [(smudgePos, mirrorOffset)]
smudgedMirrors :: [String] -> [Int]
smudgedMirrors grid = [i | i <- nub $ map snd outs, not (i `elem` is)]
  where -- Possible mirror columns for each row.
        clean :: [[Int]]
        clean = map rowMirrors grid
        -- Possible (smudgePos, [mirrorOffset]) options for each row, assuming
        -- that row is the one with the smudge.
        smudged :: [[(Int, [Int])]]
        smudged = map smudgedRowMirrors grid
        -- We need to calculate the intersections of the lists for each row for
        -- all combinations containing exactly one smudged row. To do that
        -- efficiently, we first compute `gaps`, a list of intersected lists
        -- where entry i is the intersection of all clean rows except for i.
        ls :: [[Int]]
        ls = scanl1 intersect clean
        rs :: [[Int]]
        rs = scanr1 intersect clean
        is = head rs
        gaps :: [[Int]]
        gaps = rs !! 1 : zipWith intersect ls (drop 2 rs) ++ [reverse ls !! 1]
        -- Given the smudge options for row i and the intersection of all clean
        -- rows except row i, computes a list of possible mirror offsets.
        checkRow sm cl = [(p, m) | (p, as) <- sm, m <- intersect as cl]
        outs :: [((Int, Int), Int)]
        outs = concat $
               zipWith (\y r -> [((x, y), m) | (x, m) <- r]) [1..] $
               zipWith checkRow smudged gaps

mirrorId grid = i
  where xs = mirrors grid
        ys = mirrors (transpose grid)
        [i] = xs ++ map (*100) ys

smudgedMirrorId :: [String] -> Int
smudgedMirrorId grid = i
  where xs = smudgedMirrors grid
        ys = smudgedMirrors (transpose grid)
        [i] = xs ++ map (*100) ys

part1 :: [[String]] -> Int
part1 = sum . map mirrorId

part2 :: [[String]] -> Int
part2 = sum . map smudgedMirrorId

main = do
  input <- parse <$> getContents
  putStrLn $ show $ part1 input
  putStrLn $ show $ part2 input
