import Curves


-- test points
p1 = Point(0,0)
p2 = Point(1,2)
p3 = Point(2,4)
p4 = Point(4,8)
p5 = Point(6,16)


-- assertion tests
-- (point(0,0) == point(0.009,0.009))
-- (point(0,0) /= point(0.01,0.01))


-- test curves
c1 = curve p1 [p2,p3]
c2 = curve p4 [p2,p5]
