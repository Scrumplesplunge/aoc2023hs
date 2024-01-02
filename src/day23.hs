import Control.Monad.State
import Data.Array
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

type Grid a = Array (Int, Int) a
type Node = (Int, Int)

parse :: String -> Grid Char
parse input = array ((1, 1), (w, h)) [((x, y), c) | (y, l) <- zip [1..] ls,
                                                    (x, c) <- zip [1..] l]
  where ls = lines input
        (w, h) = (length (head ls), length ls)

-- Convert the grid into a graph representing path lengths between nodes with
-- more than two connected edges.
type S a = State (Set Node, [(Node, Node, Int)]) a
graphify :: (Grid Char -> (Int, Int) -> [(Int, Int)])
         -> Grid Char -> (Node, Node, [(Node, Node, Int)])
graphify next grid = (start, end, edges)
  where (_, (w, h)) = bounds grid
        start = (2, 1)
        end = (w - 1, h)
        neighbours p = [p | p <- next grid p,
                            inRange (bounds grid) p,
                            grid ! p /= '#']
        (nodes, edges) = snd $ runState (exploreNode start) (Set.empty, [])
        exploreNode :: Node -> S ()
        exploreNode node = do
          (seen, _) <- get
          if node `Set.member` seen then
            return ()
          else do
            modify (\(s, es) -> (Set.insert node s, es))
            sequence [exploreEdge node node pos 1 | pos <- neighbours node]
            return ()
        exploreEdge :: Node -> (Int, Int) -> (Int, Int) -> Int -> S ()
        exploreEdge from prev pos n | pos == end =
          modify (\(s, es) -> (s, (from, pos, n) : es))
        exploreEdge from prev pos n = do
          let ns = [p | p <- neighbours pos, p /= prev]
          case ns of
            [] -> return ()
            [pos'] -> exploreEdge from pos pos' (n + 1)
            _ -> do
              modify (\(s, es) -> (s, (from, pos, n) : es))
              exploreNode pos

solve :: (Node, Node, [(Node, Node, Int)]) -> Int
solve (start, end, edges) = case longest start end edges of
    Nothing -> error "IMPOSSIBLE"
    Just x -> x
  where longest :: Node -> Node -> [(Node, Node, Int)] -> Maybe Int
        longest a b es = case (es `from` a, es `to` b) of
          ([e@(_, x, d)], _) | x == b -> Just d
                             | otherwise -> (+d) <$> longest x b (es `without` a)
          (_, [e@(x, _, d)]) -> (+d) <$> longest a x (es `without` b)
          (as, bs) -> best a as b (es `without` a)
        to, from :: [(Node, Node, Int)] -> Node -> [(Node, Node, Int)]
        to es x = [(a, x, d) | (a, b, d) <- es, b == x]
        from es x = [(x, b, d) | (a, b, d) <- es, a == x]
        without es x = [(a, b, d) | (a, b, d) <- es, a /= x && b /= x]
        best :: Node -> [(Node, Node, Int)] -> Node -> [(Node, Node, Int)]
             -> Maybe Int
        best a as b es = maximum' $ catMaybes $ do
          (_, x, d) <- as
          if x == b then
            return (Just d)
          else
            return ((+d) <$> longest x b es)
        maximum' [] = Nothing
        maximum' xs = Just $ maximum xs

part1 :: Grid Char -> (Int, Int) -> [(Int, Int)]
part1 grid pos@(x, y) = case grid ! pos of
    '^' -> [up]
    'v' -> [down]
    '<' -> [left]
    '>' -> [right]
    _ -> [up, down, left, right]
  where (up, down, left, right) = ((x, y - 1), (x, y + 1),
                                   (x - 1, y), (x + 1, y))

part2 :: Grid Char -> (Int, Int) -> [(Int, Int)]
part2 grid pos@(x, y) = [up, down, left, right]
  where (up, down, left, right) = ((x, y - 1), (x, y + 1),
                                   (x - 1, y), (x + 1, y))

main = do
  input <- parse <$> getContents
  putStrLn $ show $ solve $ graphify part1 input
  putStrLn $ show $ solve $ graphify part2 input
