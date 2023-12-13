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

getId :: ([String] -> [Int]) -> [String] -> Int
getId f grid = i
  where [i] = f grid ++ map (*100) (f (transpose grid))

smudgedRowMirrors :: String -> [Int]
smudgedRowMirrors = nub . concat . map rowMirrors . options
  where toggle '.' = '#'
        toggle '#' = '.'
        options "" = []
        options (x : xs) = (toggle x : xs) : map (x:) (options xs)

smudgedMirrors :: [String] -> [Int]
smudgedMirrors grid = outs \\ allClean
  where -- Possible mirror columns for each row.
        clean = map rowMirrors grid
        -- Possible mirrorOffset options for each row, assuming that row is the
        -- one with the smudge.
        smudged = map smudgedRowMirrors grid
        -- We need to calculate the intersections of the lists for each row for
        -- all combinations containing exactly one smudged row. To do that
        -- efficiently, we first compute `gaps`, a list of intersected lists
        -- where entry i is the intersection of all clean rows except for i.
        ls = scanl1 intersect clean
        (allClean : allButFirst : rs) = scanr1 intersect clean
        gaps = allButFirst : zipWith intersect ls rs ++ [reverse ls !! 1]
        -- Given the smudge options for row i and the intersection of all clean
        -- rows except row i, computes a list of possible mirror offsets.
        outs = nub $ concat $ zipWith intersect smudged gaps

main = do
  input <- parse <$> getContents
  putStrLn $ show $ sum $ map (getId mirrors) $ input
  putStrLn $ show $ sum $ map (getId smudgedMirrors) $ input
