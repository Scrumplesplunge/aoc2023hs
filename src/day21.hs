import Data.Array
import Data.Set (Set)
import qualified Data.Set as Set

parse :: String -> Array (Int, Int) Char
parse input = array ((1, 1), (w, h)) [((x, y), c) | (y, l) <- zip [1..] ls,
                                                    (x, c) <- zip [1..] l]
  where ls = lines input
        (w, h) = (length (head ls), length ls)

part1 :: Array (Int, Int) Char -> Int
part1 grid = countTrue $ head $ drop 12 $ iterate next (fmap (== 'S') grid)
  where empty = fmap (const False) grid
        countTrue cells = length [1 | True <- elems cells]
        next :: Array (Int, Int) Bool -> Array (Int, Int) Bool
        next cells = empty // map (\i -> (i, True)) neighbours
          where neighbours = do
                  ((x, y), True) <- assocs cells
                  n <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
                  if inRange (bounds grid) n && grid ! n /= '#' then
                    return n
                  else
                    []

-- part2 :: Array (Int, Int) Char -> Int
--
-- ..O.. O.S.O O.... ..... ....O S.O.O O.O.S ....O O....
-- .O.O. .O.O. .O... ..... ...O. .O.O. .O.O. ...O. .O...
-- O.S.O ..O.. S.O.. ..O.. ..O.S O.O.. ..O.O ..O.O O.O..
-- .O.O. ..... .O... .O.O. ...O. .O... ...O. .O.O. .O.O.
-- ..O.. ..... O.... O.S.O ....O O.... ....O O.O.S S.O.O
--
-- Step 65 forms a perfect diamond touching the middle of all four sides.
-- Step 66 enters four adjacent grids in the center of the corresponding edge.
-- Step 130 touches all four corners of the origin tile for the first time.
-- Step 131 enters the four diagonally adjacent grids at their corners.
--
--       ..... ..O.. .....
--       ..... .O.O. .....
--       ..... O...O .....
--       ....O ..... O....
--       ...O. ..... .O...
--
--       ..O.. .O.O. ..O..
--       .O... O.O.O ...O.
--       O.... .O.O. ....O
--       ..... O.O.O .....
--       ..... .O.O. .....
--
-- ..O..       O.O.O       ..O..
-- .O...       .O.O.       ...O.
-- O....       O.O.O       ....O
-- .O...       .O.O.       ...O.
-- ..O..       O.O.O       ..O..
--
-- ...O. .....       ..... .O...
-- ....O .....       ..... O....
-- ..... O....       ....O .....
-- ..... .O...       ...O. .....
-- ..... ..O..       ..O.. .....
--
--             .....
--             .....
--             O...O
--             .O.O.
--             ..O..
--
-- The final state will consist of:
--
--   * Two types of full squares (alternating parity)
--   * Four points
--   * Eight types of diagonal.

-- 628203198597421 too low
-- 628206310652633 too low
-- 628206330073385 right answer
part2 grid = total
  where grids start = iterate next $ empty // [(start, True)]
        (_, (size, _)) = bounds grid
        mid = size `div` 2 + 1
        full        = drop size $ grids (mid, mid)
        left        = grids (1, mid)
        right       = grids (size, mid)
        top         = grids (mid, 1)
        bottom      = grids (mid, size)
        topLeft     = grids (1, 1)
        topRight    = grids (size, 1)
        bottomLeft  = grids (1, size)
        bottomRight = grids (size, size)
        n = 26501365
        -- left/right/top/bottom are entered from the origin by step 66.
        -- It takes exactly 131 more iterations to cross the whole grid.
        point = (n - mid) `mod` size
        -- topLeft/topRight/bottomLeft/bottomRight are entered from
        -- left/right/top/bottom by step 132.
        -- It takes exactly 262 more iterations to cross the whole grid.
        slopeA = (n - size - 1) `mod` (2 * size)
        -- The second kind of slope is out of phase from the first kind by 131:
        -- it is entered in iteration 263 from the first kind of diagonal.
        slopeB = (n - 2 * size - 1) `mod` (2 * size)
        -- We get the first one in iteration 132, then we get two more every 262
        -- steps.
        numA = 1 + (n - size - 1) `div` (2 * size) * 2
        numB = (n - 1) `div` (2 * size) * 2
        -- . . A . .
        -- . A B A .
        -- A B A B A
        -- . A B A .
        -- . . A . .
        -- First full grid is at iteration 131. New ones arrive every 131
        -- iterations, forming a full ring around the old ones.
        -- i  0  1  2  3  4  5
        -- x  1  1  3  3  5  5  i `div` 2 * 2 + 1
        -- A  1  1  9  9 25 25  x^2
        -- y  0  2  2  4  4  6  (i + 1) `div` 2 * 2
        -- B  0  4  4 16 16 36  y^2
        i = (n - size) `div` size
        fullA = full !! ((n - size) `mod` 2)
        fullB = full !! ((n - size + 1) `mod` 2)
        numFullA = (i `div` 2 * 2 + 1)^2
        numFullB = ((i + 1) `div` 2 * 2)^2
        -- Assemble the lot.
        edges = [
          ("fullA",     fullA),
          ("topRightA", topRight !! slopeA),
          ("fullB",     fullB),
          ("topRightB", topRight !! slopeB),
          ("top",       top      !! point)]
        slopes = [topLeft, topRight, bottomLeft, bottomRight]
        points = [left, right, top, bottom]
        total = numFullA * countTrue fullA +
                numFullB * countTrue fullB +
                numA * sum [countTrue (g !! slopeA) | g <- slopes] +
                numB * sum [countTrue (g !! slopeB) | g <- slopes] +
                sum [countTrue (g !! point) | g <- points]
        range = bounds grid
        empty = fmap (const False) grid
        s = head [(x, y) | ((x, y), 'S') <- assocs grid]
        countTrue cells = length [1 | True <- elems cells]
        next :: Array (Int, Int) Bool -> Array (Int, Int) Bool
        next cells = empty // map (\i -> (i, True)) neighbours
          where neighbours = do
                  ((x, y), True) <- assocs cells
                  n <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
                  if inRange range n && grid ! n /= '#' then
                    return n
                  else
                    []
        assemble c = array (bounds grid) $
                       map (\(i, x) -> if x then (i, 'O') else (i, grid ! i))
                       (assocs c)
        debug (i, c) =
          let g = assemble c
          in show i ++ " steps: " ++ show (countTrue c) ++ " cells\n" ++
             unlines [[g ! (x, y) | x <- [1..w]] | y <- [1..h]] ++ "\n"
        (_, (w, h)) = bounds grid

main = do
  input <- parse <$> getContents
  putStrLn $ show $ part1 input
  putStrLn $ show $ part2 input
