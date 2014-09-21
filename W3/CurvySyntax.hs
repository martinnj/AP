-- Assignment for Week 3 of AP
-- Martin Jorgensen, tzk173
-- Casper B. Hansen, fvx507

module CurvySyntax where

import CurveAST

import Control.Applicative hiding ((<|>),Const,optional)

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
point = pointify <$>
    (char '(' *> expr) <*> (char ',' *> expr <* char ')') 
    where pointify a b = Point a b

-- parses curves
curve :: GenParser Char st Curve
curve = chainl1 c1 conn
    where c1   = chainl1 ct over
          conn = (do string "++"; return Connect)
          over = (do string "^";  return Over)
          -- point matches
          p0 = (do c <- e0; string "->"; a <- point;  return $ Translate c a)
          -- expression matches
          e0 = (do c <- e1; string "**"; a <- expr;   return $ Scale c a)
          e1 = (do c <- e2; string "refv"; a <- expr; return $ Refv c a)
          e2 = (do c <- e3; string "refh"; a <- expr; return $ Refh c a)
          e3 = (do c <- ct; string "rot"; a <- expr;  return $ Rot c a)
          ct = (id <$> ident) <|> (single <$> point)
              where single p = Single p
                    id s     = Id s

{- tried this, blew up '^' and '++' :'(
          p0 = do c <- ct
                  f <- many1 (oneOf "roefvh*->")
                  p <- optionMaybe point
                  e <- optionMaybe expr
                  case (f, p, e) of
                       ("->", Just a, _) -> return $ Translate c a
                       ("**", _, Just a) -> return $ Scale c a
-}

-- parses expressions
expr :: GenParser Char st Expr
expr = width <|> height <|> e0 <|> e1
    where width     = (do string "width"
                          c <- curve
                          return $ Width c)
          height    = (do string "height"
                          c <- curve
                          return $ Height c)
          e0 = chainl1 e1 op0
          e1 = chainl1 t op1
          t = (do n <- number; return $ Const n)
          op0 = (do string "+"; return Add)
          op1 = (do string "*"; return Mult)

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
