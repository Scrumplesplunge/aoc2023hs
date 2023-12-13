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
  where clean :: [[Int]]
        clean = map rowMirrors grid
        -- smudged !! y is the options for row y.
        smudged :: [[(Int, [Int])]]
        smudged = map smudgedRowMirrors grid
        ls :: [[Int]]
        ls = scanl1 intersect clean
        rs :: [[Int]]
        rs = scanr1 intersect clean
        is = head rs
        gaps :: [[Int]]
        gaps = rs !! 1 : zipWith intersect ls (drop 2 rs) ++ [reverse ls !! 1]
        checkRow sm cl = [(p, m) | (p, as) <- sm, m <- intersect as cl]
        outs :: [((Int, Int), Int)]
        outs = concat $
               zipWith (\y r -> [((x, y), m) | (x, m) <- r]) [1..] $
               zipWith checkRow smudged gaps

-- part1 :: [String] -> Int
mirrorId grid = i
  where xs = mirrors grid
        ys = mirrors (transpose grid)
        [i] = xs ++ map (*100) ys

smudgedMirrorId :: [String] -> Int
smudgedMirrorId grid = only is
  where xs = smudgedMirrors grid
        ys = smudgedMirrors (transpose grid)
        is = xs ++ map (*100) ys
        only [x] = x
        only xs = error ("For grid:\n" ++ unlines grid ++ "Not unique: " ++ show xs)

part1 :: [[String]] -> Int
part1 = sum . map mirrorId

part2 :: [[String]] -> Int
part2 = sum . map smudgedMirrorId

main = do
  input <- parse <$> getContents
  putStrLn $ show $ part1 input
  putStrLn $ show $ part2 input
