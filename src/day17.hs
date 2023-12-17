import Debug.Trace
import Data.List
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

options :: (Int, Int) -> Grid -> Int -> ((Int, Int), Direction)
        -> [(Int, ((Int, Int), Direction))]
options (minSteps, maxSteps) grid n (pos, d) = do
  d <- directions d
  let steps = take maxSteps $ tail $ iterate (move d) pos
      reachable = filter (inRange (bounds grid)) steps
  (n', pos) <- drop (minSteps - 1) $ measureLoss grid n reachable
  return (n', (pos, d))

run :: (Int, Int) -> Grid -> STArray s ((Int, Int), Direction) Int
    -> Set (Int, ((Int, Int), Direction)) -> ST s ()
run steps grid a frontier | Set.null frontier = return ()
run steps grid a frontier = do
  let ((n, x), frontier') = Set.deleteFindMin frontier
  n' <- readArray a x
  if n' >= 0 then
    run steps grid a frontier'
  else do
    writeArray a x n
    let ns = options steps grid n x
    -- let debug k = show (n, x) ++ " -> " ++ show k
    --trace (unlines $ map debug ns)
    run steps grid a (frontier' `Set.union` Set.fromList ns)

explore :: (Int, Int) -> Grid -> ST s (STArray s ((Int, Int), Direction) Int)
explore steps grid = do
  let (_, (w, h)) = bounds grid
  a <- newArray (((1, 1), U), ((w, h), R)) (-1)
  run steps grid a (Set.fromList [(0, ((1, 1), R)), (0, ((1, 1), D))])
  return a

debug :: Array ((Int, Int), Direction) Int -> String
debug a = ints [[best $ map (f x y) [U, D, L, R] | x <- [1..w]] |
                y <- [1..h]]
  where f :: Int -> Int -> Direction -> Int
        f x y d = a ! ((x, y), d)
        (_, ((w, h), _)) = bounds a
        ints ns = unlines $ map (concat . map int) ns
        int n = let x = show n in replicate (4 - length x) ' ' ++ x
        best os = let os' = filter (>=0) os in if null os' then (-1) else minimum os'

-- part1 :: Grid -> Int
part1 grid = minimum $ filter (>=0) [a ! ((w, h), d) | d <- [U, D, L, R]]
  where (_, (w, h)) = bounds grid
        a = runSTArray (explore (1, 3) grid)

-- part2 :: Grid -> Int
-- 307 too low
part2 grid = minimum $ filter (>=0) [a ! ((w, h), d) | d <- [U, D, L, R]]
  where (_, (w, h)) = bounds grid
        a = runSTArray (explore (4, 10) grid)

main = do
  input <- parse <$> getContents
  putStrLn $ show $ part1 input
  putStrLn $ show $ part2 input
