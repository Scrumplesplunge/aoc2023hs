import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map

-- Roll stones towards the start of the string.
roll :: String -> String
roll = go 0
  where go e ('.' : xs) = go (e + 1) xs
        go e ('O' : xs) = 'O' : go e xs
        go e ('#' : xs) = replicate e '.' ++ ('#' : go 0 xs)
        go e [] = replicate e '.'

load :: [String] -> Int
load = sum . zipWith (\y xs -> sum [y | 'O' <- xs]) [1..] . reverse

-- Roll everything North and then rotate clockwise by 90 degrees.
tumble :: [String] -> [String]
tumble = map (reverse . roll) . transpose

spin :: [String] -> [[String]]
spin = iterate (tumble . tumble . tumble . tumble)

part2 :: [String] -> Int
part2 = run Map.empty Map.empty . zip [0..] . spin
  where run :: Map Int [String] -> Map [String] Int -> [(Int, [String])] -> Int
        run i2x x2i ((i, x) : xs) =
          case Map.lookup x x2i of
            Nothing -> run (Map.insert i x i2x) (Map.insert x i x2i) xs
            Just j ->
              -- Cycle i is a repeat of cycle j, so we have a loop. The
              -- billionth cycle will match the cycle j + r, where
              let r = (1000000000 - j) `mod` (i - j) in load (i2x ! (j + r))

main = do
  input <- lines <$> getContents
  putStrLn $ show $ load $ transpose $ map roll $ transpose $ input
  putStrLn $ show $ part2 $ input
