{-# LANGUAGE RankNTypes #-}

import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.Char
import Data.List

steps :: String -> [String]
steps [] = []
steps input = let (x, xs) = break (==',') input in x : steps (drop 1 xs)

hash :: String -> Int
hash = foldl' (\x c -> (17 * (x + c)) `mod` 256) 0 . map ord

type Bucket = [(String, Int)]

add :: (String, Int) -> Bucket -> Bucket
add x [] = [x]
add x@(l, f) (x'@(l', _) : bs) | l == l'   = x : bs
                               | otherwise = x' : add x bs

remove :: String -> Bucket -> Bucket
remove l xs = [x | x@(l', _) <- xs, l /= l']

step :: STArray s Int Bucket -> String -> ST s ()
step m step = case o of
                '-' -> readArray m h >>= (writeArray m h . remove l)
                '=' -> readArray m h >>= (writeArray m h . add (l, read f))
  where (l, o : f) = break (\x -> x `elem` "-=") step
        h = hash l

part2' :: [String] -> (forall s . ST s (STArray s Int Bucket))
part2' input = do
  a <- newArray (0, 255) []
  mapM (step a) input
  return a

part2 :: [String] -> Int
part2 input = sum [(1 + i) * j * f | (i, bs) <- a, (j, (x, f)) <- zip [1..] bs]
  where a = assocs $ runSTArray (part2' input)

main = do
  input <- steps <$> (!!0) <$> lines <$> getContents
  putStrLn $ show $ sum $ map hash $ input
  putStrLn $ show $ part2 input
