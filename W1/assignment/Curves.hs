data Point = Point(Double, Double)
    deriving (Show)

point :: (Double, Double) -> Point
point (x, y) = Point(x, y)

instance Eq Point where
    Point(ax, ay) == Point(bx, by) = (abs(ax-bx) < 0.01) &&
                                     (abs(ay-by) < 0.01)

-- assertion tests
-- point(0,0) == point(0.009,0.009)
-- point(0,0) /= point(0.01,0.01)

data Curve = Curve([Point])
    deriving (Show)

curve :: Point -> [Point] -> Curve

