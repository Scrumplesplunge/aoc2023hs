import Data.Array
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List

parse :: String -> Array (Int, Int) Char
parse input = array ((1, 1), (w, h))
                    [((x, y), c) | (y, r) <- zip [1..] ls,
                                   (x, c) <- zip [1..] r]
  where ls = lines input
        (w, h) = (length (head ls), length ls)

data Direction = U | D | L | R deriving (Eq, Ord, Show)

bounce :: Char -> Direction -> [Direction]
bounce '|' R = [U, D]
bounce '|' L = [U, D]
bounce '-' U = [L, R]
bounce '-' D = [L, R]
bounce '/' U = [R]
bounce '/' D = [L]
bounce '/' L = [D]
bounce '/' R = [U]
bounce '\\' U = [L]
bounce '\\' D = [R]
bounce '\\' L = [U]
bounce '\\' R = [D]
bounce _ x = [x]

move :: (Int, Int) -> Direction -> (Int, Int)
move (x, y) U = (x, y - 1)
move (x, y) D = (x, y + 1)
move (x, y) L = (x - 1, y)
move (x, y) R = (x + 1, y)

type Node = ((Int, Int), Direction)

energised :: Node -> Array (Int, Int) Char -> Int
energised x grid = length $ group $ map fst $ Set.elems $ trace x Set.empty
  where trace :: Node -> Set Node -> Set Node
        trace x@(pos, d) seen =
          foldl' (go pos) (Set.insert x seen) $ bounce (grid ! pos) d
        go :: (Int, Int) -> Set Node -> Direction -> Set Node
        go pos seen d =
          let pos' = move pos d
              x' = (pos', d)
          in if inRange (bounds grid) pos' && not (x' `elem` seen) then
               trace x' seen
             else
               seen

part1 = energised ((1, 1), R)

debug grid seen = [[grid' ! (x, y) | x <- [1..w]] | y <- [1..h]]
  where grid' = grid // zip seen (repeat '#')
        (_, (w, h)) = bounds grid'

part2 grid = maximum $ map (\x -> energised x grid) $ options
  where (_, (w, h)) = bounds grid
        options = [((x, 1), D) | x <- [1..w]] ++
                  [((x, h), U) | x <- [1..w]] ++
                  [((1, y), R) | y <- [1..h]] ++
                  [((w, y), L) | y <- [1..h]]

main = do
  input <- parse <$> getContents
  putStrLn $ show $ part1 input
  putStrLn $ show $ part2 input
