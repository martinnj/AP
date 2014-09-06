module Curves where

newtype Point = Point (Double, Double)
  deriving (Show)


point :: (Double, Double) -> Point
point (x, y) = Point(x, y)

instance Eq Point where
  Point(ax, ay) == Point(bx, by) = (abs(ax-bx) < 0.01) &&
                                   (abs(ay-by) < 0.01)

instance Num (Point) where
  Point(ax, ay) + Point(bx, by) = Point(ax+bx, ay+by)
  Point(ax, ay) * Point(bx, by) = Point(ax*bx, ay*by) -- Does not make sense,
                                                      -- but needed for
                                                      -- overload.
  Point(ax, ay) - Point(bx, by) = Point(ax-bx, ay-by)
  abs(Point(x, y)) = Point(abs(x), abs(y))
  signum (Point (x,y)) = Point (signum x, signum y)
  fromInteger i = Point (fromInteger i, fromInteger i)

newtype Curve = Curve [Point]
  deriving (Show, Eq)

curve :: Point -> [Point] -> Curve
curve p ps = Curve (p : ps)

connect :: Curve -> Curve -> Curve
connect (Curve(xs)) (Curve(ys)) = Curve(xs ++ ys)

-- Pretty as can be :D
rotate :: Curve -> Double -> Curve
rotate (Curve(cs)) d = Curve (map (rotate' d) cs)
  where rotate' :: Double -> Point -> Point
        rotate' d (Point(x,y)) = Point(x * cos r - y * sin r,
                                       x * sin r + y * cos r)
          where r = (d * pi/180)

translate :: Curve -> Point -> Curve
translate (Curve(cs)) p = Curve (map (+ p) cs)

data Axis = Vertical | Horizontal

reflect :: Curve -> Axis -> Double -> Curve
reflect (Curve(ps)) a d = Curve(map (flip' a d) ps)
  where
    flip' :: Axis -> Double -> Point -> Point
    flip' Horizontal d (Point(x,y)) = Point(x           , y*(-1)+(2*d))
    flip' Vertical   d (Point(x,y)) = Point(x*(-1)+(2*d), y)

--bbox :: Curve -> (Point, Point)
-- TODO: Actual function, should be easy, linear search through all points,
-- record lowest and highest x and y values.

--width :: Curve -> Double
-- TODO: Use bbox and take the distance between the x coords.

--height :: Curve -> Double
-- TODO: What do you think? ;)

toList :: Curve -> [Point]
toList (Curve cs) = cs
