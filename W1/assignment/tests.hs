--import Test.QuickCheck -- srsly, fuck quickcheck :P
import Curves


-- Written by Martin Jørgensen, tk173
-- and Casper B. Hansen, fvx507

test :: [Bool] -> Bool
test ts = foldl (&&) True ts

-- test points
p1 = Point(0,0)
p2 = Point(1,2)
p3 = Point(2,4)
p4 = Point(4,8)
p5 = Point(6,16)
p6 = Point(0,0)
p7 = Point(100,0)
p8 = Point(50, 100)
p9 = Point(50, 200)

p10 = Point(0,100)
p11 = Point(50,50)
p12 = Point(100,100)




-- test curves
c1 = curve p1 [p2,p3]
c2 = curve p4 [p2,p5]
c3 = Curve [p1, p2, p3, p4, p2, p5]
c4 = Curve [Point(1,1), Point(2,3), Point(3,5)]
c5 = Curve [Point(1,-1), Point(2,1), Point(3,3)]
c6 = Curve [p1, Point(1,-2), Point(2,-4)]
c7 = Curve [Point(0,2), Point(1,0), Point(2,-2)]
c8 = Curve [p1, Point(-1,2), Point(-2,4)]
c9 = Curve [Point(0,0), Point(2,-1), Point(4,-2)]
c10 = Curve [Point(-100,-100),Point(100,-100),Point(100,100),Point(-100,100),Point(-100,-100)]
c11 = Curve [p6,p7,p8,p6,p9,p7]
c12 = Curve [p1,p10,p11,p12,p7]

-- All tests below should return true.
tests :: [Bool]
tests = [
  -- Testing Eq
  p1 == p1,
  p1 /= p2,
  point(0,0) == point(0.009,0.009),
  point(0,0) /= point(0.01,0.01),

  -- Testing Point infix operators
  (p1 + p2) == p2,
  (p2 + p2) == p3,
  (p4 - p2) == Point(3,6),

  -- Testing connect
  (connect c1 c2) == c3,

  -- TODO: Testing rotate
  (rotate c1 90) == c9,

  -- Testing translate
  (translate c1 (Point(1,1))) == c4,
  (translate c1 (Point(1,-1))) == c5,

  -- Testing reflect
  (reflect c1 Horizontal 0) == c6,
  (reflect c1 Horizontal 1) == c7,
  (reflect c1 Vertical 0) == c8,

  -- Testing bbox
  (bbox c5) == (Point(1,-1), Point(3,3)),
  (bbox c8) == (Point(-2,0), Point(0,4)),

  -- Testing width
  (width c1) == 2.0,
  (width c3) == 6.0,
  (width c8) == 2.0,

  -- Testing height
  (height c1) == 4.0,
  (height c3) == 16.0,
  (height c8) == 4.0,

  -- Testing toList
  (toList c1) == [p1, p2, p3],
  (toList c2) == [p4, p2, p5]
  ]


-- Do a folding function that collapses the list to a single boolean, which
-- should be true if all tests passes.

-- More detail might be needed though. <--
-- Disregard, QuickCheck is mandatory from week 2.
