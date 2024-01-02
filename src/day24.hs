import Debug.Trace
import Data.Char
import Data.List
import Text.Parsec
import Text.Parsec.String

type Vec3 = (Integer, Integer, Integer)
type Mat3 = (Vec3, Vec3, Vec3)  -- Row-major

parseInput :: String -> [(Vec3, Vec3)]
parseInput = map row . zip [1..] . lines
  where row (i, l) = case parse row' ("input:" ++ show i) l of
          Left e -> error (show e)
          Right x -> x
        nat :: Parser Integer
        nat = foldl' (\n x -> 10 * n + toInteger(digitToInt x)) 0 <$> many1 digit
        int :: Parser Integer
        int = choice [nat, char '-' >> fmap (0-) nat]
        vec3 = do
          x <- int
          string ", "
          y <- int
          string ", "
          z <- int
          return (x, y, z)
        row' = do
          l <- vec3
          string " @ "
          r <- vec3
          return (l, r)

vadd (ax, ay, az) (bx, by, bz) = (ax + bx, ay + by, az + bz)
vsub (ax, ay, az) (bx, by, bz) = (ax - bx, ay - by, az - bz)
vmul (x, y, z) k = (k * x, k * y, k * z)
cross (ax, ay, az) (bx, by, bz) = (ay * bz - az * by,
                                   az * bx - ax * bz,
                                   ax * by - ay * bx)
dot (ax, ay, az) (bx, by, bz) = ax * bx + ay * by + az * bz

z = (0, 0, 0)

vdiv a (0, 0, 0) = Nothing
vdiv (0, 0, 0) b = Nothing
vdiv a b = if cross a b == z && r == 0 then Just q else Nothing
  where (q, r) = dot a b `divMod` dot b b

-- Given two hailstones A and B, the path of A crosses the path of B if, for
-- some t0 and t1:
--
--   p0 + v0 * t0 = p1 + v1 * t1
--   p1 - p0 = v0 * t0 - v1 * t1
--
-- Which forms a set of simultaneous equations:
--
--   |v0x -v1x| . |t0| = |p1x-p0x|
--   |v0y -v1y|   |t1|   |p1y-p0y|
--
-- We can solve for t0 and t1, then use either t0 or t1 to derive the point of
-- intersection between the two lines and check that it is within the bounds.
-- Since the intersection point may not have integer coordinates, we can
-- multiply everything by the determinant of the matrix and work with integers
-- instead.
crossings :: [(Vec3, Vec3)] -> Int
crossings input = length [(a, b) | (a : bs) <- tails input, b <- bs, crosses a b]
  where crosses a@(pa, va@(vax, vay, _)) b@(pb, vb@(vbx, vby, _)) =
            (det /= 0 && t1det * det > 0 && t2det * det > 0 && inRange p)
          where det = vax * vby - vay * vbx
                (dpx, dpy, _) = pb `vsub` pa
                t1det = vby * dpx - vbx * dpy
                t2det = vay * dpx - vax * dpy
                a = 200000000000000 * det
                b = 400000000000000 * det
                (low, high) = if det < 0 then (b, a) else (a, b)
                p :: Vec3
                p@(x, y, _) = (pa `vmul` det) `vadd` (va `vmul` t1det)
                p' = (fromIntegral x, fromIntegral y, 0)
                inRange :: Vec3 -> Bool
                inRange (x, y, _) = low <= x && x <= high &&
                                    low <= y && y <= high

part1 :: [(Vec3, Vec3)] -> Int
part1 = crossings . map dropZ
  where dropZ ((x, y, _), (vx, vy, _)) = ((x, y, 0), (vx, vy, 0))

-- Given a matrix M and a vector b, solve for the vector a such that Ma = b.
solve :: Mat3 -> Vec3 -> Vec3
solve m@((a0, a1, a2),
         (b0, b1, b2),
         (c0, c1, c2))
      (x, y, z) = (p, q, r)
  where p' = (b1 * c2 - b2 * c1) * x +
             (a2 * c1 - a1 * c2) * y +
             (a1 * b2 - a2 * b1) * z
        q' = (b2 * c0 - b0 * c2) * x +
             (a0 * c2 - a2 * c0) * y +
             (a2 * b0 - a0 * b2) * z
        r' = (b0 * c1 - b1 * c0) * x +
             (a1 * c0 - a0 * c1) * y +
             (a0 * b1 - a1 * b0) * z
        d = a0 * (b1 * c2 - b2 * c1) +
            a1 * (b2 * c0 - b0 * c2) +
            a2 * (b0 * c1 - b1 * c0)
        f x = if x `mod` d /= 0 then
                error ("non-integer: " ++ show (x, d))
              else x `div` d
        (p, q, r) = if d == 0 then error "det is 0" else (f p', f q', f r')

-- Objects collide if their relative positions and their relative velocities are
-- parallel.
--
-- We want the start position of a rock which hits every single other rock,
-- which means that the rock's velocity relative to every hailstone is parallel
-- with the rock's offset from that hailstone.
--
-- Find p, v, ti for all i such that:
--
--   pi + vi * ti = p + v * ti for all i
--
-- Suppose we hit x0 at t0, t01 nanoseconds before we hit x1. Then:
--
-- We hit x0 at p0 + v0 * t0
-- We hit x1 at p1 + v1 * (t0 + t)
-- With a delta of p1 + v1 * (t0 + t01) - p0 - v0 * t0
--               = p1 + v1 * t0 + v1 * t01 - p0 - v0 * t0
--               = p1 - p0 + (v1 - v0) * t0 + v1 * t01
-- And a velocity of (p1 - p0 + (v1 - v0) * t0 + v1 * t01) / t01
--                 = (p1 - p0) / t01 + (v1 - v0) * t0 / t01 + v1
--
-- Defining a = p1 - p0
--          i = 1 / t01
--          b = v1 - v0
--          j = t0 / t01
--
-- The set of suitable velocities form a plane a * i + b * j + v1 ranging over
-- i and j. That can be re-expressed as n.x = d where:
--
--   n = a x b = (p1 - p0) x (v1 - v0)
--   d = n . v1
--
-- By intersecting this plane with two others, we can find the velocity.
velocity :: [(Vec3, Vec3)] -> Vec3
velocity (x0 : x1 : x2 : xs) = solve (n0, n1, n2) (d0, d1, d2)
  where plane (p0, v0) (p1, v1) = (n, d)
          where n = (p1 `vsub` p0) `cross` (v1 `vsub` v0)
                d = v1 `dot` n
        (n0, d0) = plane x0 x1
        (n1, d1) = plane x0 x2
        (n2, d2) = plane x1 x2

-- Given the stone's velocity v, we can find the starting position p from two
-- hailstones by finding their collision times.
--
--   Find t0, t1 such that:
--
--     p0 + v0 * t0 + v * (t1 - t0) = p1 + v1 * t1
--     p1 - p0 = v0 * t0 - v1 * t1 + v * t1 - v * t0
--     p1 - p0 = (v0 - v) * t0 + (v - v1) * t1
--
-- This is a set of simultaneous equations (for each coordinate). Defining:
--
--   x = p1 - p0
--   a = v0 - v
--   b = v - v1
--
-- We have:
--
--   |ax bx 0| . |t0| = |x0|
--   |ay by 0|   |t1|   |x1|
--   | 0  0 1|   | 1|   | 1|
--
-- By solving this for t0 and t1, we can use p0, v0, v and t0 to derive p.
--
--   p + v * t0 = p0 + v0 * t0
--   p = p0 + v0 * t0 - v * t0
position :: [(Vec3, Vec3)] -> Vec3 -> Vec3
position ((p0, v0) : (p1, v1) : xs) v = c0 `vsub` (v `vmul` t0)
  where (ax, ay, _) = v0 `vsub` v
        (bx, by, _) = v `vsub` v1
        x = p1 `vsub` p0
        (t0, t1, _) = solve ((ax, bx, 0), (ay, by, 0), (0, 0, 1)) x
        c0 = p0 `vadd` (v0 `vmul` t0)

part2 :: [(Vec3, Vec3)] -> Integer
part2 = manhattan . (position <*> velocity)
  where manhattan (x, y, z) = x + y + z

main = do
  input <- parseInput <$> getContents
  putStrLn $ show $ part1 input
  putStrLn $ show $ part2 input
