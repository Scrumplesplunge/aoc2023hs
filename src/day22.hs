import Control.Monad.ST
import Data.Array.ST
import Data.Array.MArray
import Data.Char
import Data.List
import Text.Parsec
import Text.Parsec.String

type Vec2 = (Int, Int)
type Vec3 = (Int, Int, Int)

parseInput :: String -> [(Vec3, Vec3)]
parseInput = sortOn (\((_, _, z), _) -> z) . map row . zip [1..] . lines
  where row (i, l) = case parse row' ("input:" ++ show i) l of
          Left e -> error (show e)
          Right x -> x
        int :: Parser Int
        int = foldl' (\n x -> 10 * n + digitToInt x) 0 <$> many1 digit
        vec3 = do
          x <- int
          char ','
          y <- int
          char ','
          z <- int
          return (x, y, z)
        row' = do
          l <- vec3
          char '~'
          r <- vec3
          return (l, r)

xy :: (Vec3, Vec3) -> [Vec2]
xy ((x1, y1, _), (x2, y2, _)) = [(x, y) | x <- [x1..x2], y <- [y1..y2]]

height :: (Vec3, Vec3) -> Int
height ((_, _, z1), (_, _, z2)) = (z2 - z1 + 1)

type Brick = Int
dump :: STArray s Vec2 (Int, Brick) -> Brick -> (Vec3, Vec3)
     -> ST s [(Brick, Brick)]
dump a id b = do
  footprint <- sequence [readArray a pos | pos <- xy b]
  let maxHeight = maximum (map fst footprint)
      blocks = nub [(b, id) | (h, b) <- footprint, h == maxHeight]
      h = height b
  sequence [writeArray a pos (maxHeight + h, id) | pos <- xy b]
  return blocks

part1 :: [(Vec3, Vec3)] -> Int
part1 bricks = length [b | b <- [1..length bricks], removable b]
  where contacts = sort $ runST $ do
          a <- newArray ((0, 0), (9, 9)) (0, 0)
          concat <$> sequence [dump a id b | (id, b) <- zip [1..] bricks]
        -- Returns the number of bricks that brick b is supported by.
        supports b = length [b1 | (b1, b2) <- contacts, b2 == b, b1 > 0]
        -- Returns true if brick b can be removed. This is the case if all
        -- bricks resting on b have at least one other support.
        removable b = and [supports b2 > 1 | (b1, b2) <- contacts, b1 == b]

part2 :: [(Vec3, Vec3)] -> Int
part2 bricks = sum [cascade [b] (0, contacts) - 1 | b <- [1..length bricks]]
  where fall :: [(Brick, Brick)] -> Brick -> [Brick]
        fall cs b = [b2 | (b1, b2) <- cs, b1 == b, supports cs b2 == 1]
        cascade :: [Brick] -> (Int, [(Brick, Brick)]) -> Int
        cascade [] (n, cs) = n
        cascade (b : bs) (n, cs) =
          let fs = fall cs b
              cs' = [x | x@(b1, b2) <- cs, b1 /= b && b2 /= b]
          in cascade (fs ++ bs) (n + 1, cs')
        contacts :: [(Brick, Brick)]
        contacts = sort $ runST $ do
          a <- newArray ((0, 0), (9, 9)) (0, 0)
          concat <$> sequence [dump a id b | (id, b) <- zip [1..] bricks]
        -- Returns the number of bricks that brick b is supported by.
        supports cs b = length [b1 | (b1, b2) <- cs, b2 == b, b1 > 0]

main = do
  input <- parseInput <$> getContents
  putStrLn $ show $ part1 input
  putStrLn $ show $ part2 input
