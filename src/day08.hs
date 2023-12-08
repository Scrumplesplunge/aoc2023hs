import Data.Map (Map, (!))
import Data.List
import qualified Data.Map as Map

type Input = (Int, String, Map String (String, String))

parse :: String -> Input
parse input = (length ds, concat $ repeat ds, Map.fromList $ map entry es)
  where (ds : "" : es) = lines input
        entry e = (take 3 e, (take 3 $ drop 7 e, take 3 $ drop 12 e))

part1 :: Input -> Int
part1 (_, ds, es) = go 0 ds "AAA"
  where go n ds "ZZZ" = n
        go n ('L' : ds) k = go (n + 1) ds (fst $ es ! k)
        go n ('R' : ds) k = go (n + 1) ds (snd $ es ! k)

-- Assumption: The number of steps to reach an end is always a multiple of the
-- length of the list of instructions.
walkThisWay :: Input -> String -> (String, Int)
walkThisWay (l, ds, es) start = go 0 ds start
  where go n (d : ds) k =
          if end k then
            (k, n)
          else
            go (n + 1) ds (pick d $ es ! k)
        end k = k !! 2 == 'Z'
        pick :: Char -> (a, a) -> a
        pick 'L' (l, r) = l
        pick 'R' (l, r) = r

walk :: Input -> String -> ([String], [String])
walk (l, ds, es) start = go 0 ds start [] Map.empty
  where go :: Int -> String -> String -> [String] -> Map (Int, String) [String]
           -> ([String], [String])
        go n (d : ds) k hs seen =
          let nk = (n `mod` l, k)
              next = go (n + 1) ds (pick d $ es ! k) (k : hs)
          in case Map.lookup nk seen of
            Nothing -> next (Map.insert nk hs seen)
            Just hs' -> (reverse hs', drop (length hs') (reverse hs))
        pick :: Char -> (a, a) -> a
        pick 'L' (l, r) = l
        pick 'R' (l, r) = r

splat :: ([String], [String]) -> [String]
splat (p, r) = p ++ concat (repeat r)

debug (p, r) = (length p, length r, filter end p, filter end r)
  where end x = x !! 2 == 'Z'

-- part2 :: Input -> Int
part2 input@(_, _, es) = foldl' lcm 1 $ map (snd . walkThisWay input) starts
  where starts = [k | k <- map fst $ Map.toList es, k !! 2 == 'A']
        end x = x !! 2 == 'Z'

main = do
  input <- parse <$> getContents
  putStrLn $ show $ part1 input
  putStrLn $ show $ part2 input
