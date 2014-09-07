module Curves where

import Text.Printf (printf)

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
  Point(ax, ay) * Point(bx, by) = Point(ax*bx, ay*by) -- <3 Compiler warnings
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
        rotate' d (Point(x,y)) = Point(x * cosr - y * sinr,
                                       x * sinr + y * cosr)
          where (r, cosr, sinr) = ((360 - d) * pi/180, cos r, sin r)


-- Translate a Curve around the plane.
translate :: Curve -> Point -> Curve
translate (Curve(p:ps)) p1 = Curve (map (+ p') (p:ps))
  where p' = p1 - p

-- Axis enumerations.
data Axis = Vertical | Horizontal

-- Reflect a curve around an axis, the axis can be offset by offset "o".
reflect :: Curve -> Axis -> Double -> Curve
reflect (Curve(ps)) Vertical o = Curve (map (\(Point(x,y)) -> Point(-x+2*o, y)) ps)
reflect (Curve(ps)) Horizontal o   = Curve (map (\(Point(x,y)) -> Point(x, -y+2*o)) ps)

-- Calculate bounding box
bbox :: Curve -> (Point, Point)
bbox (Curve(p:ps)) = (foldl (cmp min) p (p:ps), foldl (cmp max) p (p:ps))
  where cmp f = \(Point(ax,ay)) (Point(bx,by)) -> Point(f ax bx, f ay by)

-- Get the width of the bounding box.
width :: Curve -> Double
width c = xmax - xmin
  where (Point(xmin,_), Point(xmax,_)) = bbox(c)

-- Get the height of the bounding box.
height :: Curve -> Double
height c = ymax - ymin
  where (Point(_,ymin), Point(_,ymax)) = bbox(c)

-- Returns the list of points contained in a Curve.
toList :: Curve -> [Point]
toList (Curve ps) = ps


-- PART 2 GENERATE SVG
-- Converts a curve to it's SVG-XML representation.
toSVG :: Curve -> String
toSVG c =
    let (Point(xmin,ymin),Point(xmax,ymax)) = bbox c

        pp = printf "%.2f"

        coordConvert :: Point
        coordConvert = Point(abs xmin,abs ymin)


        (Point(xmin',ymin'),Point(xmax',ymax')) = bbox screenCurve

        screenCurve = (translate c coordConvert)
        imgWidth = max ((width screenCurve) + xmin') 2
        imgHeight = max ((height screenCurve) + ymin') 2


        head = "<svg xmlns=\"http://www.w3.org/2000/svg\" " ++
               "width=\"" ++
               (show imgWidth) ++
               "\" " ++
               "height=\"" ++
               (show imgHeight) ++
               "\" " ++
               "version=\"1.1\">\n<g>\n" ++
               "<!-- x-min'=" ++ show xmin' ++ " -->\n" ++
               "<!-- y-min'=" ++ show ymin' ++ " -->\n" ++
               "<!-- x-max'=" ++ show xmax' ++ " -->\n" ++
               "<!-- y-max'=" ++ show ymax' ++ " -->\n"

        foot = "</g>\n</svg>"

        line :: Curve -> String
        line (Curve([]))        = ""
        line (Curve(_:[]))      = ""
        line (Curve(Point(x1,y1):Point(x2,y2):rest)) =
            "<line style=\"stroke-width:2px; stroke:black; fill:white\" " ++
            "x1=\"" ++ pp x1 ++ "\" " ++
            "y1=\"" ++ pp (imgHeight - y1) ++ "\" " ++
            "x2=\"" ++ pp x2 ++ "\" " ++
            "y2=\"" ++ pp (imgHeight - y2) ++ "\" " ++
            "/>\n" ++
            line (Curve(Point(x2,y2):rest))

    in head ++ line screenCurve ++ foot

-- Save a Curve object as an SVG file.
toFile :: Curve -> FilePath -> IO ()
toFile c p = writeFile p $ toSVG c

-- TODO: save result from toSVG(Curve) to a file.

-- PART 3 TEST WITH HILBERT CURVE

-- Creates the Hilbert curve.
hilbert :: Curve -> Curve
hilbert c = c0 `connect` c1 `connect` c2 `connect`c3
  where w = width c
        h = height c
        p = 6
        ch = reflect c Horizontal 0
        c0 = ch `rotate` (-90) `translate` (Point(w+p+w, h+p+h))
        c1 = c `translate` (Point(w+p+w, h))
        c2 = c
        c3 = ch `rotate` 90 `translate` (Point(0, h+p))

-- Test to see if the created picture is indeed the Hilbert Curve.
h :: Curve
h = hilbert $ hilbert $ hilbert $ hilbert $ curve (Point(0,0)) []
-- toFile h "hilbert.svg"


-- PART 4 PEANO AND OTHER CURVES (OPTIONAL)

-- PART 5 EXTENSIONS (OPTIONAL)
--scale :: Curve -> Double -> Curve

rev :: Curve -> Curve
rev (Curve(ps)) = Curve(reverse ps)
