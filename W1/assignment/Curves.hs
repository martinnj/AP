module Curves where

-- PART 1 LIBRARY
newtype Point = Point (Double, Double)
  deriving (Show)

-- Creates a 2D Point with coordinates (x,y)
point :: (Double, Double) -> Point
point (x, y) = Point(x, y)

-- Points are considered equal (sharing a location) if the difference between
-- their coordinates are less that 0.01.
instance Eq Point where
  Point(ax, ay) == Point(bx, by) = (abs(ax-bx) < 0.01) &&
                                   (abs(ay-by) < 0.01)

-- Instancing Point under Num to use the + and - operators.
-- */abs/signum is nonsensical but included to shut up the compiler ^_^
instance Num (Point) where
  Point(ax, ay) + Point(bx, by) = Point(ax+bx, ay+by)
  Point(ax, ay) * Point(bx, by) = Point(ax*bx, ay*by) -- Does not make sense,
                                                      -- but needed for
                                                      -- overload.
  Point(ax, ay) - Point(bx, by) = Point(ax-bx, ay-by)
  abs(Point(x, y)) = Point(abs(x), abs(y))
  signum (Point (x,y)) = Point (signum x, signum y)
  fromInteger i = Point (fromInteger i, fromInteger i)

-- A curve is a list of points.
newtype Curve = Curve [Point]
  deriving (Show, Eq)

-- Create a curve from a starting Point and a list of subsequent points.
curve :: Point -> [Point] -> Curve
curve p ps = Curve (p : ps)

-- Connect two curves by appending their point lists.
connect :: Curve -> Curve -> Curve
connect (Curve(xs)) (Curve(ys)) = Curve(xs ++ ys)

-- Rotate a curve around origin (0,0) by d degrees.
rotate :: Curve -> Double -> Curve
rotate (Curve(cs)) d = Curve (map (rotate' d) cs)
  where rotate' :: Double -> Point -> Point
        rotate' d (Point(x,y)) = Point(x * cos r - y * sin r,
                                       x * sin r + y * cos r)
          where r = (d * pi/180)

-- Translate a Curve around the plane.
translate :: Curve -> Point -> Curve
translate (Curve(cs)) p = Curve (map (+ p) cs)

-- Axis enumerations.
data Axis = Vertical | Horizontal

-- Reflect a curve around an axis, the axis can be offset by offset "o".
reflect :: Curve -> Axis -> Double -> Curve
reflect (Curve(ps)) a o = Curve(map (flip' a o) ps)
  where
    flip' :: Axis -> Double -> Point -> Point
    flip' Horizontal o (Point(x,y)) = Point(x           , y*(-1)+(2*o))
    flip' Vertical   o (Point(x,y)) = Point(x*(-1)+(2*o), y)

--bbox :: Curve -> (Point, Point)
-- TODO: Actual function, should be easy, linear search through all points,
-- record lowest and highest x and y values.

--width :: Curve -> Double
-- TODO: Use bbox and take the distance between the x coords.

--height :: Curve -> Double
-- TODO: What do you think? ;)

-- Returns the list of points contained in a Curve.
toList :: Curve -> [Point]
toList (Curve ps) = ps


-- PART 2 GENERATE SVG
-- Converts a curve to it's SVG-XML representation.
--toSVG :: Curve -> String
-- TODO: Implement it ^_^

-- Save a Curve object as an SVG file.
--toFile :: Curve -> FilePath -> IO ()
-- TODO: save result from toSVG(Curve) to a file.

-- PART 3 TEST WITH HILBERT CURVE

-- Creates the Hilbert curve.
-- hilbert :: Curve -> Curve
-- hilbert c = c0 `connect` c1 `connect` c2 `connect`c3
--   where w = width c
--         h = height c
--         p = 6
--         ch = reflect c Horizontal 0
--         c0 = ch `rotate` (-90) `translate` (Point(w+p+w, h+p+h))
--         c1 = c `translate` (Point(w+p+w, h))
--         c2 = c
--         c3 = ch `rotate` 90 `translate` (Point(0, h+p))

-- Test to see if the created picture is indeed the Hilbert Curve.
--h :: Curve
--h = hilbert $ hilbert $ hilbert $ hilbert $ curve (Point(0,0)) []
--toFile h "hilbert.svg"


-- PART 4 PEANO AND OTHER CURVES (OPTIONAL)

-- PART 5 EXTENSIONS (OPTIONAL)
--scale :: Curve -> Double -> Curve

rev :: Curve -> Curve
rev (Curve(ps)) = Curve(reverse ps)
