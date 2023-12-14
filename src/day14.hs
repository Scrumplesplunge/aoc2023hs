import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map

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
              -- Want the result after 1000000000 cycles
              -- 1000000000 = j + (i - j) * q + r for some (q, r)
              -- 1000000000 - j = (i - j) * q + r
              -- We can find (q, r) with (1000000000 - j) `divMod` (i - j)
              -- And then the final state will be the same as the `j + r`th one.
              let (q, r) = (1000000000 - j) `divMod` (i - j)
              in load (i2x ! (j + r))

main = do
  input <- lines <$> getContents
  putStrLn $ show $ load $ reverse $ transpose $ tumble $ input
  putStrLn $ show $ part2 $ input
