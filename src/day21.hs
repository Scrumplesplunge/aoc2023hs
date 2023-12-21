import Data.Array

parse :: String -> Array (Int, Int) Char
parse input = array ((1, 1), (w, h)) [((x, y), c) | (y, l) <- zip [1..] ls,
                                                    (x, c) <- zip [1..] l]
  where ls = lines input
        (w, h) = (length (head ls), length ls)

next :: Array (Int, Int) Char -> Array (Int, Int) Bool -> Array (Int, Int) Bool
next grid cells = empty // map (\i -> (i, True)) neighbours
  where empty = fmap (const False) grid
        neighbours = do
          ((x, y), True) <- assocs cells
          n <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
          if inRange (bounds grid) n && grid ! n /= '#' then
            return n
          else
            []

countTrue :: Array (Int, Int) Bool -> Int
countTrue cells = length [1 | True <- elems cells]

part1 :: Array (Int, Int) Char -> Int
part1 grid = countTrue $ iterate (next grid) (fmap (== 'S') grid) !! 64

part2 :: Array (Int, Int) Char -> Int
part2 grid = fulls + points + slopes
  where empty = fmap (const False) grid
        grids start = iterate (next grid) $ empty // [(start, True)]
        (_, (size, _)) = bounds grid
        mid = size `div` 2 + 1
        n = 26501365
        -- There are two types of full grid which alternate. First full grid is
        -- at iteration 131. New ones arrive every 131 iterations, forming
        -- a full ring around the old ones.
        -- i  0  1  2  3  4  5
        -- A  1  1  9  9 25 25  (i `div` 2 * 2 + 1)^2
        -- B  0  4  4 16 16 36  ((i + 1) `div` 2 * 2)^2
        i = (n - size) `div` size
        numFullA = (i `div` 2 * 2 + 1)^2
        numFullB = ((i + 1) `div` 2 * 2)^2
        full        = drop size $ grids (mid, mid)
        fullA = full !! ((n - size) `mod` 2)
        fullB = full !! ((n - size + 1) `mod` 2)
        fulls = numFullA * countTrue fullA + numFullB * countTrue fullB
        -- There is a "pointy" grid for each of the four directions.
        point = (n - mid) `mod` size
        left        = grids (1, mid)    !! point
        right       = grids (size, mid) !! point
        top         = grids (mid, 1)    !! point
        bottom      = grids (mid, size) !! point
        points = sum $ map countTrue [left, right, top, bottom]
        -- There are two different types of grid for each diagonal which
        -- alternate along the edge. It takes exactly 262 more iterations to
        -- cross the whole grid. We get the first one in iteration 132, then we
        -- get two more every 262 steps.
        slopeA = (n - size - 1) `mod` (2 * size)
        slopeB = (n - 2 * size - 1) `mod` (2 * size)
        numSlopeA = 1 + (n - size - 1) `div` (2 * size) * 2
        numSlopeB = (n - 1) `div` (2 * size) * 2
        topLeft     = grids (1, 1)
        topRight    = grids (size, 1)
        bottomLeft  = grids (1, size)
        bottomRight = grids (size, size)
        slopeAs = map (!! slopeA) [topLeft, topRight, bottomLeft, bottomRight]
        slopeBs = map (!! slopeB) [topLeft, topRight, bottomLeft, bottomRight]
        slopes = numSlopeA * sum (map countTrue slopeAs) +
                 numSlopeB * sum (map countTrue slopeBs)

main = do
  input <- parse <$> getContents
  putStrLn $ show $ part1 input
  putStrLn $ show $ part2 input
