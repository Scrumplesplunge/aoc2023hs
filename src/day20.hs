import Debug.Trace
import Control.Monad.State
import Data.List
import Data.Map.Strict (Map, (!), (!?))
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as Seq
import Text.Parsec
import Text.Parsec.String

data ModuleType = B | F | C deriving (Eq, Show)
type ModuleName = String
data Module = Broadcaster | FlipFlop Bool | Conjunction (Map ModuleName Bool)
            | Rx Bool
  deriving (Eq, Show)

line :: Parser (ModuleName, (ModuleType, [ModuleName]))
line = do
  (n, t) <- choice [string "broadcaster" >> return ("broadcaster", B),
                    char '%' >> many1 lower >>= (\x -> return (x, F)),
                    char '&' >> many1 lower >>= (\x -> return (x, C))]
  string " -> "
  ns <- many1 lower `sepBy` string ", "
  return (n, (t, ns))

parseInput :: String -> Map ModuleName (Module, [ModuleName])
parseInput = Map.fromList . finish . map parseLine . zip [1..] . lines
  where parseLine (n, l) = case parse line ("input:" ++ show n) l of
          Left e -> error (show e)
          Right x -> x
        finish mods = map (buildState mods) mods
        buildState mods (name, (t, os)) = (name, (state t name mods, os))
        state B _ _ = Broadcaster
        state F _ _ = FlipFlop False
        state C n mods = Conjunction (Map.fromList inputs)
          where inputs = [(m, False) | (m, (t, os)) <- mods, n `elem` os]

pulse :: (ModuleName, Bool) -> Module -> (Module, [Bool])
pulse (from, value) Broadcaster = (Broadcaster, [value])
pulse (from, True) f@(FlipFlop _) = (f, [])
pulse (from, False) f@(FlipFlop x) = let x' = not x in (FlipFlop x', [x'])
pulse (from, value) c@(Conjunction mem) =
  let mem' = Map.insert from value mem
  in (Conjunction mem', [not $ and (Map.elems mem')])
pulse (from, False) (Rx _) = (Rx True, [])
pulse (from, True) r@(Rx _) = (r, [])

mustGet :: (Ord k, Show k, Show v) => Map k v -> k -> v
mustGet m k = case m !? k of
  Just v -> v
  Nothing -> error ("Couldn't find " ++ show k ++ " in:\n" ++ show m)

part1 :: Map ModuleName (Module, [ModuleName]) -> Int
part1 mods = score (iterate button (0, 0, initial) !! 1000)
  where initial = Map.map fst mods
        run :: Seq (ModuleName, ModuleName, Bool)
            -> (Int, Int, Map ModuleName Module)
            -> (Int, Int, Map ModuleName Module)
        run Seq.Empty out = out
        run ((from, here, value) :<| ps) (l, h, state)
          | not (here `Map.member` mods) = if value then
                                             run ps (l, h + 1, state)
                                           else
                                             run ps (l + 1, h, state)
        run ((from, here, value) :<| ps) (l, h, state) =
          let (l', h') = if value then (l, h + 1) else (l + 1, h)
              (mod, newPulses) = pulse (from, value) (state `mustGet` here)
              state' = Map.insert here mod state
              sent = [(here, to, value) | value <- newPulses,
                                          to <- snd (mods `mustGet` here) ]
          in run (ps >< Seq.fromList sent) (l', h', state')
        button :: (Int, Int, Map ModuleName Module)
               -> (Int, Int, Map ModuleName Module)
        button = run (Seq.fromList [("button", "broadcaster", False)])
        score (l, h, state) = l * h

-- There are four large cyclic clusters in my graph of modules which each feed
-- into a Conjunction with one input (i.e. a NOT gate) and then into a second
-- Conjunction which merged the outputs. Each of the large clusters has a cycle
-- with a different period n and will emit a low pulse for every nth button
-- press. RX will receive a pulse when all four emit this low pulse at the same
-- time, which can be found with the least common multiple.
part2 :: Map ModuleName (Module, [ModuleName]) -> Int
part2 mods = foldl1' lcm (map count forks)
  where inputs m = [n | (n, (_, os)) <- Map.assocs mods, m `elem` os]
        -- The list of nodes receiving messages from the broadcaster.
        forks = snd (mods ! "broadcaster")
        -- The list of nodes feeding into the conjunction right before rx.
        joins = inputs end
        [end] = inputs "rx"
        stubs = Map.fromList [(j, Rx False) | j <- joins]
        -- The cycle counts required for each fork to produce a False signal.
        count start = length $ takeWhile (not . done) $ iterate button initial
          where initial :: Map ModuleName Module
                initial = stubs `Map.union` Map.map fst mods
                done :: Map ModuleName Module -> Bool
                done s = or [s ! j == Rx True | j <- joins]
                button = run (Seq.fromList [("broadcaster", start, False)])
        run :: Seq (ModuleName, ModuleName, Bool)
            -> Map ModuleName Module
            -> Map ModuleName Module
        run Seq.Empty out = out
        run ((from, here, value) :<| ps) state =
          let (mod, newPulses) = pulse (from, value) (state `mustGet` here)
              state' = Map.insert here mod state
              sent = [(here, to, value) | value <- newPulses,
                                          to <- snd (mods `mustGet` here) ]
          in run (ps >< Seq.fromList sent) state'

main = do
  input <- parseInput <$> getContents
  putStrLn $ show $ part1 input
  putStrLn $ show $ part2 input
