-- Assignment tests for Week 3 of AP
-- Martin Jørgensen, tzk173
-- Casper B. Hansen, fvx507

import CurveAST
import CurvySyntax
import Test.HUnit

--test0 = TestCase $ assertEqual "Simple assignment"
--        (parseString "c = (0,0)")
--        (Right [Def "c" (Single (Point (Const 0) (Const 0)))[]])

--test1 = TestCase $ assertEqual "Test from assignment"
--        (parseString "c = (0,0) ++ (5, 42.5)")
--        (Right [Def "c" (Connect (Single (Point (Const 0.0) (Const 0.0)))
--                        (Single (Point (Const 5.0) (Const 42.5)))) []])

--test2 = TestCase $ assertEqual "Precedence test"
--        (parseString "cc = (0,0) ^ (1,1) ++ (2,2)")
--        (Right [Def "cc" (Connect
--                          (Over (Single (Point (Const 0) (Const 0)))
--                                (Single (Point (Const 1) (Const 1))))
--                          (Single (Point (Const 2) (Const 2)))) []])
