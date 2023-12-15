{-# LANGUAGE RankNTypes #-}

import Control.Monad.ST
import Data.Array
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.ST
import Data.Char
import Data.List

steps :: String -> [String]
steps [] = []
steps input = let (x, xs) = break (==',') input in x : steps (drop 1 xs)

hash :: String -> Int
hash = foldl' (\x c -> (17 * (x + c)) `mod` 256) 0 . map ord

part1 :: [String] -> Int
part1 = sum . map hash

type Step = (String, Char, Int)
parseStep :: String -> Step
parseStep step = (label, op, read focalLength)
  where (label, op : focalLength) = break (\x -> x `elem` "-=") step

modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray a k f = readArray a k >>= (writeArray a k . f)

add :: (String, Int) -> Bucket -> Bucket
add x [] = [x]
add x@(l, f) (x'@(l', _) : bs) | l == l'   = x : bs
                               | otherwise = x' : add x bs

remove :: String -> Bucket -> Bucket
remove x [] = []
remove l (x'@(l', _) : bs) | l == l'   = bs
                           | otherwise = x' : remove l bs

type Bucket = [(String, Int)]
type HashMap s = STArray s Int Bucket
step :: HashMap s -> Step -> ST s ()
step m (l, '=', f) = modifyArray m (hash l) (add (l, f))
step m (l, '-', f) = modifyArray m (hash l) (remove l)

part2' :: [String] -> (forall s . ST s (HashMap s))
part2' input = do
  a <- newArray (0, 255) []
  mapM (step a . parseStep) input
  return a

part2 :: [String] -> Int
part2 input = sum [(1 + i) * j * f | (i, bs) <- a, (j, (x, f)) <- zip [1..] bs]
  where a = assocs $ runSTArray (part2' input)

main = do
  input <- steps <$> (!!0) <$> lines <$> getContents
  putStrLn $ show $ part1 input
  putStrLn $ show $ part2 input
