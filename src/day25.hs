import Data.Function
import Data.List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
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
type Edges = Map Node (Map Node Int)
type Graph = ([(Node, Int)], Edges)
rejig :: [(String, String)] -> Graph
rejig edges = (vs, es)
  where nodes [] = []
        nodes ((a, b) : es) = (a, ()) : (b, ()) : nodes es
        ns = Map.fromList (nodes edges)
        vs = [(i, 1) | i <- [1..length ns]]
        es' = [(Map.findIndex a ns, Map.findIndex b ns) | (a, b) <- edges]
        es'' = groupBy ((==) `on` fst) $ sort (es' ++ map swap es')
        entry ((k, v) : kvs) =
          (k, Map.fromList [(x, 1) | x <- (v : map snd kvs)])
        es = Map.fromList (map entry es'')

-- Merge two nodes in an edge list.
mergeEdges :: Node -> Node -> Map Node (Map Node Int) -> Map Node (Map Node Int)
mergeEdges a b es = edits `Map.union` Map.delete b es
  where as = es ! a
        bs = es ! b
        a' = Map.delete a $ Map.delete b $ Map.unionWith (+) as bs
        fix n _ = Map.insert a (a' ! n) $ Map.delete b $ es ! n
        edits = Map.insert a a' $ Map.mapWithKey fix a'

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
minCutPhase :: Edges -> (Node, Node, Int)
minCutPhase es = go 0 es
  where go :: Node -> Edges -> (Node, Node, Int)
        go s es | Map.null es = error ":("
        go s es | length es == 2 =
          case Map.assocs (es ! 0) of
            [(t, n)] -> (s, t, n)
        go _ es = go t (mergeEdges 0 t es)
          where (t, _) = maximumBy (compare `on` snd) $ Map.assocs (es ! 0)

-- Identify the minimum cut. Returns (n, k) where n is the cut and k is the size
-- of one side.
minCut :: Graph -> (Int, Int)
minCut g = go (10000, 0) g
  where go :: (Int, Int) -> Graph -> (Int, Int)
        go best (_, es) | Map.size es == 1 = best
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
