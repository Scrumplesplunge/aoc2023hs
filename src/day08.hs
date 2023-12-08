import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map

type Input = (String, Map String (String, String))

parse :: String -> Input
parse input = (concat $ repeat ds, Map.fromList $ map entry es)
  where (ds : "" : es) = lines input
        entry e = (take 3 e, (take 3 $ drop 7 e, take 3 $ drop 12 e))

walk :: Input -> String -> Int
walk (ds, es) start = go 0 ds start
  where go n _ k | end k = n
        go n (d : ds) k = go (n + 1) ds (pick d $ es ! k)
        end k = k !! 2 == 'Z'
        pick 'L' (l, r) = l
        pick 'R' (l, r) = r

main = do
  input <- parse <$> getContents
  -- Assumptions:
  --   * The number of steps to reach an end is always a multiple of the length
  --     of the list of instructions.
  --   * The number of steps to cycle back around to and end is always exactly
  --     the same as the number of steps to initially reach that end node.
  let starts = [k | k <- map fst $ Map.toList (snd input), k !! 2 == 'A']
      ends = map (walk input) starts
  putStrLn $ show $ head ends
  putStrLn $ show $ foldl' lcm 1 ends
