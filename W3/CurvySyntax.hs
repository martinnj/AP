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
ident :: GenParser Char st Ident
ident = (many1 letter <++> opt)
    where opt = option "" $ (letter <|> digit <|> sym) <:> opt
          sym = oneOf "_"

-- parses floating-point numbers
number :: GenParser Char st Number
number = fmap rd (num <++> dec)
    where rd  = read :: String -> Double
          num = many1 digit
          dec = option "" $ char '.' <:> num

-- matches terminals of a point, discarding symbols
point :: GenParser Char st Point
point = pointify <$> (char '(' *> expr) <*>
                     (char ',' *> expr <* char ')') 
    where pointify a b = Point a b

-- parses curves
curve :: GenParser Char st Curve
curve = id <$> ident <|>
        single <$> point <|>
    where single p      = Single p
          id s          = Id s

-- parses expressions
expr = width <|> height <|> e0 <|> e1
    where width     = (do string "width"
                          spaces
                          c <- curve
                          return $ Width c)
          height    = (do string "height"
                          spaces
                          c <- curve
                          return $ Height c)
          e0 = chainl1 e1 op0
          e1 = chainl1 t op1

-- parses terminal symbols
t = do n <- number
       return $ Const n

-- parses precedence level 0
op0 = (do string "+"
          return Add)

-- parses precedence level 1
op1 = (do string "*"
          return Mult)


parser :: String -> Either ParseError Curve
parser str = parse curve "Error!" str 

-- parseString "c = (0,0) ++ (5, 42.5)" --should be
-- Right [Def "c" (Connect (Single (Point (Const 0.0) (Const 0.0)))
--                         (Single (Point (Const 5.0) (Const 42.5)))) []]
parseString :: String -> Either Error Program
parseString str = Right []

-- Parses a file with a Curvy program into a proper Curve.
parseFile :: FilePath -> IO (Either Error Program)
parseFile filename = fmap parseString $ readFile filename
