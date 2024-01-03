import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple

parse :: String -> [(String, String)]
parse = concatMap edges . lines
  where parts [] = []
        parts xs = let (' ' : a, xs') = splitAt 4 xs in a : parts xs'
        edges xs = do
          let (a, ':' : xs') = splitAt 3 xs
          b <- parts xs'
          return (a, b)

type Node = Int
type Edge = (Node, Node, Int)
type Graph = ([(Node, Int)], [Edge])
rejig :: [(String, String)] -> Graph
rejig edges = (vs, es)
  where nodes [] = []
        nodes ((a, b) : es) = (a, ()) : (b, ()) : nodes es
        ns = Map.fromList (nodes edges)
        vs = [(i, 1) | i <- [1..length ns]]
        es = [(Map.findIndex a ns, Map.findIndex b ns, 1) | (a, b) <- edges]

-- Merge two nodes in an edge list, discarding self-connections.
mergeEdges :: Node -> Node -> [Edge] -> [Edge]
mergeEdges a b = go Map.empty
  where go ms [] = [(a, c, k) | (c, k) <- Map.assocs ms]
        go ms (e@(p, q, k) : es) = case (rewrite p, rewrite q) of
          (p', q') | p' == a && q' == a -> go ms es
                   | p' == a            -> go (count q' k ms) es
                   | q' == a            -> go (count p' k ms) es
                   | otherwise          -> e : go ms es
        rewrite x | x == b = a
        rewrite x = x
        count k v m = Map.alter (inc v) k m
        inc k Nothing = Just k
        inc k (Just k') = Just (k + k')

-- Merge nodes in a vertex list.
mergeVertices :: Node -> Node -> [(Node, Int)] -> [(Node, Int)]
mergeVertices a b = go 0
  where go n [] = [(a, n)]
        go n (v@(x, k) : vs) | x == a || x == b = go (n + k) vs
                             | otherwise        = v : go n vs

-- Merge two nodes in an edge list, discarding self-connections. O(E).
mergeNodes :: Node -> Node -> Graph -> Graph
mergeNodes a b (vs, es) = (mergeVertices a b vs, mergeEdges a b es)

-- Identify a minimum s-t cut. Returns (s, t, n) where n is the cut.
minCutPhase :: [Edge] -> (Node, Node, Int)
minCutPhase es = go 0 es
  where go :: Node -> [(Node, Node, Int)] -> (Node, Node, Int)
        go s [] = error ":("
        go s [(0, t, n)] = (s, t, n)
        go s [(t, 0, n)] = (s, t, n)
        go _ es = go t (mergeEdges 0 t es)
          where cs = [e | e@(a, b, _) <- es, a == 0 || b == 0]
                (a, b, _) = maximumBy (compare `on` weight) cs
                weight (_, _, w) = w
                t = if min a b == 0 then max a b else error ":("

-- Identify the minimum cut. Returns (n, k) where n is the cut and k is the size
-- of one side.
minCut :: Graph -> (Int, Int)
minCut g = go (10000, 0) g
  where go :: (Int, Int) -> Graph -> (Int, Int)
        go best (_, []) = best
        go (n, k) g@(_, es) =
          let (s, t, n') = minCutPhase es
              v = case lookup t (fst g) of
                Nothing -> error ":("
                Just v -> v
              best' = if n' < n then (n', v) else (n, k)
              g' = mergeNodes s t g
          in go best' g'

part1 :: Graph -> Int
part1 g = let (3, k) = minCut g in k * (n - k)
  where n = length (fst g)

main = do
  input <- rejig <$> parse <$> getContents
  putStrLn $ show $ part1 input
