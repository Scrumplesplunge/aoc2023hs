import Control.Monad.State
import Data.Maybe
import Debug.Trace
import Control.Monad.ST
import Data.Array.ST
import Data.Array.MArray
import Data.Array
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

-- ignore :: Grid Char -> Grid Char
ignore grid = grid // [(p, '#') | p <- options, cuts p]
  where b@(_, (w, h)) = bounds grid
        reachable :: Grid Char -> Grid Char
        reachable g = runST $ do
          a <- newArray b '.'
          explore g a (2, 1)
          explore g a (w - 1, h)
          freeze a
        init = reachable grid
        explore :: Grid Char -> STArray s (Int, Int) Char -> (Int, Int)
                -> ST s ()
        explore g a pos@(x, y) = do
          c <- readArray a pos
          if c == 'O' then
            return ()
          else do
            writeArray a pos 'O'
            let (u, d, l, r) = ((x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y))
                ns = [n | n <- [u, d, l, r], inRange b n, g ! n /= '#']
            sequence [explore g a n | n <- ns]
            return ()
        options = [(x, y) | x <- [1..w], y <- [1..h], grid ! (x, y) /= '#']
        cuts pos = reachable (grid // [(pos, '#')]) // [(pos, 'O')] /= init

--     grid // [(p, '#') | p <- Set.elems dead]
--   where go :: Set (Int, Int) -> (Int, Int) -> (Bool, Set (Int, Int))
--         go seen pos@(x, y) = if pos == end then
--                                (True, Set.empty)
--                              else if anyLive then
--                                (True, dead)
--                              else
--                                (False, Set.insert pos dead)
--           where (_, (w, h)) = bounds grid
--                 start = (2, 1)
--                 end = (w - 1, h)
--                 seen' = Set.insert pos seen
--                 lds = [go seen' n | n <- next grid pos,
--                                     not (n `Set.member` seen'),
--                                     inRange (bounds grid) n,
--                                     grid ! n /= '#']
--                 (ls, ds) = (map fst lds, map snd lds)
--                 dead = foldl' Set.union Set.empty ds
--                 anyLive = or ls
--         (_, dead) = go Set.empty (2, 1)

-- required :: Grid Char -> Grid Char
required grid = grid // [(p, '!') | p <- options, cuts p]
  where b@(_, (w, h)) = bounds grid
        end = (w - 1, h)
        possible :: Grid Char -> Bool
        possible g = explore g Set.empty (2, 1)
        dirs (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
        explore :: Grid Char -> Set (Int, Int) -> (Int, Int) -> Bool
        explore g seen pos | pos `Set.member` seen = False
        explore g seen pos | pos == end = True
        explore g seen pos = or [explore g seen' pos' | pos' <- ns]
          where ns = [n | n <- dirs pos, inRange b n, g ! n /= '#']
                seen' = Set.insert pos seen
        options = [(x, y) | x <- [1..w], y <- [1..h], grid ! (x, y) /= '#']
        cuts pos = not (possible (grid // [(pos, '#')]))

type Grid a = Array (Int, Int) a

parse :: String -> Grid Char
parse input = array ((1, 1), (w, h)) [((x, y), c) | (y, l) <- zip [1..] ls,
                                                    (x, c) <- zip [1..] l]
  where ls = lines input
        (w, h) = (length (head ls), length ls)

-- next :: Grid Char -> (Int, Int) -> [(Int, Int)]
-- next grid pos@(x, y) = [up, down, left, right]
--   where (up, down, left, right) = ((x, y - 1), (x, y + 1),
--                                    (x - 1, y), (x + 1, y))

type Node = (Int, Int)
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

solve :: (Grid Char -> (Int, Int) -> [(Int, Int)]) -> Grid Char -> Int
solve next grid = go Set.empty start
  where (_, (w, h)) = bounds grid
        start = (2, 1)
        end = (w - 1, h)
        go :: Set (Int, Int) -> (Int, Int) -> Int
        go seen pos@(x, y) | pos == end = length seen
                           | otherwise = maximum (0 : allowed)
          where seen' = Set.insert pos seen
                allowed = [go seen' pos' | pos' <- next grid pos,
                                           inRange (bounds grid) pos',
                                           grid ! pos' /= '#',
                                           not (pos' `Set.member` seen')]

solveGraph1 :: Node -> Node -> [(Node, Node, Int)] -> Int
solveGraph1 start end edges = go Set.empty start 0
  where go :: Set Node -> Node -> Int -> Int
        go seen pos dist | pos == end = dist
        go seen pos dist = maximum (0 : neighbours pos)
          where neighbours pos = [go seen' to (dist + dist') |
                                  (from, to, dist') <- edges,
                                  not (to `Set.member` seen')]
                seen' = Set.insert pos seen

solveGraph2 :: Node -> Node -> [(Node, Node, Int)] -> Int
solveGraph2 start end edges = case longest start end edges of
    Nothing -> error "IMPOSSIBLE"
    Just x -> x
  where longest :: Node -> Node -> [(Node, Node, Int)] -> Maybe Int
        longest a b es =
          --trace ("longest " ++ show a ++ " " ++ show b ++ " " ++ show es) $
          case (es `from` a, es `to` b) of
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
        best a as b es =
            -- trace ("best " ++ show a ++ " " ++ show as ++ " " ++ show b ++ " " ++ show es) $
            maximum' $ catMaybes $ do
          (_, x, d) <- as
          if x == b then
            return (Just d)
          else
            return ((+d) <$> longest x b es)
        maximum' [] = Nothing
        maximum' xs = Just $ maximum xs

solve' :: (Grid Char -> (Int, Int) -> [(Int, Int)]) -> Grid Char -> Int
solve' next input = solveGraph2 start end edges
  where (start, end, edges) = graphify next input

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

debug :: Grid Char -> String
debug g = unlines [[g ! (x, y) | x <- [1..w]] | y <- [1..h]]
  where (_, (w, h)) = bounds g

choices :: Grid Char -> [(Int, Int)]
choices g = [(x, y) | x <- [1..w], y <- [1..h], choice x y]
  where b@(_, (w, h)) = bounds g
        options x y = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
        choice x y = length [o | o <- options x y,
                                 inRange b o,
                                 g ! o /= '#'] > 2

-- part 2: 2006 too low
main = do
  input <- parse <$> getContents
  putStrLn $ show $ graphify part1 input
  putStrLn $ show $ graphify part2 input
  putStrLn $ show $ solve' part1 input
  putStrLn $ show $ solve' part2 input
