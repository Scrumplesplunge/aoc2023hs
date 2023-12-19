import Data.Char
import Data.List

data Direction = U | D | L | R deriving (Eq, Ord, Read)

-- ...######    ...#####!  Compute the area using the Shoelace Formula, then add
-- ...#....#    ...#####!  on the missing cells required for the 1-cell thick
-- ####.####    #####!!!!  border on the bottom and right sides.
-- #....#...    #####!...
-- ##...##.. -> ######!..  `go 0 ds` finds the area marked with `#` using
-- .#....#..    .#####!..  the Shoelace Formula on all the horizontal edges.
-- .#..###..    .###!!!..
-- .#..#....    .###!....  The remaining cells, marked with `!`, constitute one
-- .####....    .!!!!....  more than half of the cells on the perimeter.
area :: [(Direction, Int)] -> Int
area ds = abs (go 0 ds) + sum (map snd ds) `div` 2 + 1
  where go y [] = 0
        go y ((U, n) : ds) = go (y - n) ds
        go y ((D, n) : ds) = go (y + n) ds
        go y ((L, n) : ds) = go y ds - n * y
        go y ((R, n) : ds) = go y ds + n * y

-- Part 1 uses the human-readable directions.
part1 :: String -> (Direction, Int)
part1 = parseRow . words
  where parseRow [d, n, _] = (read d, read n)

-- Part 2 uses the obfuscated directions encoded in the hex code.
part2 = parseRow . words
  where parseRow [_, _, h] = (direction $ take 1 $ drop 7 h,
                              distance $ take 5 $ drop 2 h)
        distance = foldl' (\n d -> 16 * n + digitToInt d) 0
        direction "0" = R
        direction "1" = D
        direction "2" = L
        direction "3" = U

main = do
  input <- lines <$> getContents
  putStrLn $ show $ area $ map part1 input
  putStrLn $ show $ area $ map part2 input
