-- Assignment for Week 3 of AP
-- Martin Jørgensen, tzk173
-- Casper B. Hansen, fvx507

module CurvySyntax where

import CurveAST

import Control.Applicative hiding ((<|>),Const)

-- <|> : either

-- allowed:
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.String
import Text.Parsec.Combinator

-- disallowed:
-- import Text.Parsec.Token
-- import Text.Parsec.Language
-- import Text.Parsec.Expr

-- utilities
(<++>) a b = (++) <$> a <*> b   -- concatenation
(<:>) a b = (:) <$> a <*> b     -- cons

-- Errors can be added ad-libitum.
data Error = Some | Other

-- parses identifiers
ident :: GenParser Char st String
ident = many1 letter <++> opt
    where opt = option "" $ (letter <|> digit <|> sym) <:> opt
          sym = oneOf "_"

-- parses floating-point numbers
number :: GenParser Char st Double
number = fmap rd (num <++> dec)
    where rd  = read :: String -> Double
          num = many1 digit
          dec = option "" $ char '.' <:> num

expr :: GenParser Char st Expr
expr =  (do n <- number
            return (Const n) ) <|>
        (do e1 <- expr
            char '+'
            e2 <- expr
            return (Add e1 e2 ))

-- matches terminals of a point, discarding symbols
point :: GenParser Char st Point
point = pointify <$> (char '(' *> expr) <*>
                     (char ',' *> expr <* char ')') 
    where pointify a b = Point a b


parser :: String -> Either ParseError Point
parser str = parse point "Error!" str 

-- parseString "c = (0,0) ++ (5, 42.5)" --should be
-- Right [Def "c" (Connect (Single (Point (Const 0.0) (Const 0.0)))
--                         (Single (Point (Const 5.0) (Const 42.5)))) []]
parseString :: String -> Either Error Program
parseString str = Right []

-- Parses a file with a Curvy program into a proper Curve.
parseFile :: FilePath -> IO (Either Error Program)
parseFile filename = fmap parseString $ readFile filename
