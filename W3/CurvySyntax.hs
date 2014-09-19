-- Assignment for Week 3 of AP
-- Martin Jørgensen, tzk173
-- Casper B. Hansen, fvx507

module CurvySyntax where

import CurveAST

import Text.Parsec

-- Errors can be added ad-libitum.
data Error = Some | Other




-- parseString "c = (0,0) ++ (5, 42.5)" --should be
-- Right [Def "c" (Connect (Single (Point (Const 0.0) (Const 0.0)))
--                         (Single (Point (Const 5.0) (Const 42.5)))) []]
parseString :: String -> Either Error Program
parseString str = Right []

-- Parses a file with a Curvy program into a proper Curve.
parseFile :: FilePath -> IO (Either Error Program)
parseFile filename = fmap parseString $ readFile filename
