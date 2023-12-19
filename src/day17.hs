import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.Set (Set)
import qualified Data.Set as Set

type Grid = Array (Int, Int) Int
parse :: String -> Grid
parse input = array ((1, 1), (w, h)) cs
  where ls = lines input
        (w, h) = (length (head ls), length ls)
        num c = read [c]
        cs = [((x, y), num c) | (y, l) <- zip [1..] ls, (x, c) <- zip [1..] l]

data Direction = U | D | L | R deriving (Eq, Ix, Ord, Show)

move :: Direction -> (Int, Int) -> (Int, Int)
move U (x, y) = (x, y - 1)
move D (x, y) = (x, y + 1)
move L (x, y) = (x - 1, y)
move R (x, y) = (x + 1, y)

directions :: Direction -> [Direction]
directions U = [L, R]
directions D = [L, R]
directions L = [U, D]
directions R = [U, D]

measureLoss grid n [] = []
measureLoss grid n (x@pos : xs) =
  let n' = n + grid ! pos in (n', x) : measureLoss grid n' xs

solve :: (Int, Int) -> Grid -> Int
solve (minSteps, maxSteps) grid = runST explore
  where (_, end) = bounds grid
        options :: Int -> ((Int, Int), Direction)
                -> [(Int, ((Int, Int), Direction))]
        options n (pos, d) = do
          d <- directions d
          let steps = take maxSteps $ tail $ iterate (move d) pos
              reachable = filter (inRange (bounds grid)) steps
          (n', pos) <- drop (minSteps - 1) $ measureLoss grid n reachable
          return (n', (pos, d))
        explore :: ST s Int
        explore = do
          seen <- newArray (((1, 1), U), (end, R)) False
          run seen (Set.fromList [(0, ((1, 1), R)), (0, ((1, 1), D))])
        run :: STArray s ((Int, Int), Direction) Bool
            -> Set (Int, ((Int, Int), Direction))
            -> ST s Int
        run seen frontier = do
          let ((n, x), frontier') = Set.deleteFindMin frontier
              ns = Set.fromList (options n x)
          alreadySeen <- readArray seen x
          if fst x == end then
            return n
          else if alreadySeen then
            run seen frontier'
          else do
            writeArray seen x True
            run seen (Set.union frontier' ns)

main = do
  input <- parse <$> getContents
  putStrLn $ show $ solve (1, 3) input
  putStrLn $ show $ solve (4, 10) input
