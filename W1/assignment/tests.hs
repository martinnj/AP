import Curves

-- test points
p1 = Point(0,0)
p2 = Point(1,2)
p3 = Point(2,4)
p4 = Point(4,8)
p5 = Point(6,16)


-- test curves
c1 = curve p1 [p2,p3]
c2 = curve p4 [p2,p5]
c3 = Curve [p1, p2, p3, p4, p2, p5]
c4 = Curve [Point(1,1), Point(2,3), Point(3,5)]
c5 = Curve [Point(1,-1), Point(2,1), Point(3,3)]
c6 = Curve [p1, Point(1,-2), Point(2,-4)]
c7 = Curve [Point(0,2), Point(1,0), Point(2,-2)]
c8 = Curve [p1, Point(-1,2), Point(-2,4)]

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

  -- Testing translate
  (translate c1 (Point(1,1))) == c4,
  (translate c1 (Point(1,-1))) == c5,

  -- Testing reflect
  (reflect c1 Horizontal 0) == c6,
  (reflect c1 Horizontal 1) == c7,
  (reflect c1 Vertical 0) == c8,

  -- TODO: Testing bbox
  -- TODO: Testing width
  -- TODO: Testing height

  -- Testing toList
  (toList c1) == [p1, p2, p3],
  (toList c2) == [p4, p2, p5]
  ]


-- Do a folding function that collapses the list to a single boolean,
-- which should be true if all tests passes.
-- More detail might be needed though.
