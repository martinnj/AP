--tests :: [Bool]
--tests = []
-- What where these?

newtype Point = Point (Double, Double)
  deriving (Show)

-- test points
p1 = Point(0,0)
p2 = Point(1,2)
p3 = Point(2,4)
p4 = Point(4,8)
p5 = Point(6,16)

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

-- assertion tests
-- (point(0,0) == point(0.009,0.009))
-- (point(0,0) /= point(0.01,0.01))

newtype Curve = Curve [Point]
  deriving (Show)

-- test curves
c1 = curve p1 [p2,p3]
c2 = curve p4 [p2,p5]

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
