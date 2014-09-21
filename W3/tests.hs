-- Assignment tests for Week 3 of AP
-- Martin Jrgensen, tzk173
-- Casper B. Hansen, fvx507

import CurveAST
import CurvySyntax

-- allowed:
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.String
import Text.Parsec.Combinator

exprTest0 = (parse expr "error0" "1+2")
exprTest1 = (parse expr "error1" "1*2")
exprTest2 = (parse expr "error2" "width (0,0)++(1,1)") -- Expected fail, currently.
exprTests = [exprTest0,
             exprTest1,
             exprTest2]

curveTest0 = (parse curve "error3" "(0,0)^(1,1)")
curveTest1 = (parse curve "error4" "(0,0)++(1,1)")
curveTest2 = (parse curve "error5" "(0,0)")
curveTest = [curveTest0,
             curveTest1,
             curveTest2]


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
