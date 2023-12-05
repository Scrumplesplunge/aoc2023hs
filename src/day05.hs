import Data.Char
import Data.List

type Seed = Int
type Map = [(Int, Int, Int)]
type Input = ([Seed], [Map])

nat :: String -> Int
nat = foldl' (\a x -> 10 * a + digitToInt x) 0

parse :: String -> Input
parse = sections . lines
  where sections :: [String] -> Input
        sections (s : "" : ms) = (seeds s, makeMaps [] ms)
        seeds :: String -> [Seed]
        seeds = map nat . tail . words
        makeMaps :: [String] -> [String] -> [Map]
        makeMaps m [] = [makeMap (reverse m)]
        makeMaps m ("" : ms) = makeMap (reverse m) : makeMaps [] ms
        makeMaps m (m' : ms) = makeMaps (m' : m) ms
        makeMap :: [String] -> Map
        makeMap = map makeEntry . tail
        makeEntry :: String -> (Int, Int, Int)
        makeEntry e = let [a, b, c] = map nat $ words e in (a, b, c)

lookupOne :: Map -> Int -> Int
lookupOne ((d, s, n) : ms) x | s <= x && x < s + n = d - s + x
lookupOne (_ : ms) x = lookupOne ms x
lookupOne [] x = x

part1 :: Input -> Int
part1 (ss, ms) = minimum $ foldl (\ss m -> map (lookupOne m) ss) ss ms

lookupRange :: Map -> (Int, Int) -> [(Int, Int)]
-- Case where the search range is empty.
lookupRange _ (ro, 0) = []
-- Case where the start of the input range is inside a map range.
lookupRange ((d, s, n) : ms) (ro, rn) | s <= ro && ro < s + n =
    let start = ro
        end = min (s + n) (ro + rn)
    in (start - s + d, end - start) : lookupRange ms (end, ro + rn - end)
-- Case where the end of the input range is inside a map range.
lookupRange ((d, s, n) : ms) (ro, rn) | s < ro + rn && ro + rn <= s + n =
    let start = max s ro
        end = ro + rn
    in (start - s + d, end - start) : lookupRange ms (ro, start - ro)
-- Case where the range does not overlap with this map entry.
lookupRange (_ : ms) (ro, rn) = lookupRange ms (ro, rn)
lookupRange [] (ro, rn) = [(ro, rn)]

part2 :: Input -> Int
part2 (ss, ms) = minimum $ map fst $ foldl mapRanges (makeSeedRange ss) ms
  where mapRanges sr m = concat $ map (lookupRange m) sr
        makeSeedRange (o : n : ss) = (o, n) : makeSeedRange ss
        makeSeedRange [] = []

main = do
  input <- parse <$> getContents
  putStrLn $ show $ part1 input
  putStrLn $ show $ part2 input
