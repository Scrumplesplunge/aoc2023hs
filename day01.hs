import Data.Char
import Data.Maybe
import Data.List
import Control.Monad

numbers = [("one",   '1'), ("two",   '2'), ("three", '3'),
           ("four",  '4'), ("five",  '5'), ("six",   '6'),
           ("seven", '7'), ("eight", '8'), ("nine",  '9')]

tryDigit (x : xs) | isDigit x = Just x
tryDigit _ = Nothing
tryNum xs (n, d) = if n `isPrefixOf` xs then Just d else Nothing
checkPos xs = msum (tryDigit xs : map (tryNum xs) numbers)
firstLast xs = [head xs, last xs]

part1 = filter isDigit
part2 = catMaybes . map checkPos . tails
run f = putStrLn . show . sum . map ((read :: String -> Int) . firstLast . f)

main = do
  input <- fmap lines getContents
  run part1 input
  run part2 input
