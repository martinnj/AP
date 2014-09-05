-- Exercise set 0
-- Martin made this ^_^

type Pos = (Int, Int)
data Direction = North | South | East | West

-- Perform a move.
move :: Direction -> Pos -> Pos
move North (x,y) = (x  , y+1)
move West  (x,y) = (x-1, y  )
move South (x,y) = (x  , y-1)
move East  (x,y) = (x+1, y  )


-- Perform several moves after one another.
moves :: [Direction] -> Pos -> Pos
moves [] p = p
moves (d : ds) p = moves ds (move d p)


-- Multiply natural numbers using this data type declaration:
data Nat = Zero | Succ Nat
    deriving (Eq, Show, Read, Ord)


addN :: Nat -> Nat -> Nat
addN n Zero = n
addN n (Succ m) = Succ(addN n m)


nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ(int2nat(n - 1))


multN :: Nat -> Nat -> Nat
multN _ Zero        = Zero
multN Zero _        = Zero
multN (Succ n) m    = addN m (multN n m)

-- Every element in the left subtree is <= node value.
-- Non polymorphic:
-- data Tree = Leaf | Node Int Tree Tree
--           deriving (Eq, Show, Read, Ord)

-- insert :: Int -> Tree -> Tree
-- insert n Leaf         = Node n Leaf Leaf
-- insert n (Node m l r) = if n <= m
--                         then Node m (insert n l) r
--                         else Node m l (insert n r)

-- Now polymorphic :)
data Tree t = Leaf | Node t (Tree t) (Tree t)
            deriving (Eq, Show, Read, Ord)

-- name shorts: type, value, current, left and right
insert :: Ord t => t -> Tree t -> Tree t
insert v Leaf         = Node v Leaf Leaf
insert v (Node c l r) = if v <= c
                        then Node c (insert v l) r
                        else Node c l (insert v r)


data Expr = Con Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving (Eq, Show, Read, Ord)

value :: Expr -> Int
value (Con n)   = n
value (Add n m) = value n + value m
value (Sub n m) = value n - value m
value (Mul n m) = value n * value m
value (Div n m) = value n `div` value m

-- *** Exception: divide by zero
-- Number range: -9223372036854775808..9223372036854775807

prettyExpr :: Expr -> String
prettyExpr (Con n) = show n
prettyExpr (Add n m) = prettyExpr n ++ " + " ++ prettyExpr m
prettyExpr (Sub n m) = prettyExpr n ++ " - " ++ prettyExpr m
prettyExpr (Mul n m) = "(" ++ prettyExpr n ++ ") * (" ++ prettyExpr m ++ ")"
prettyExpr (Div n m) = "(" ++ prettyExpr n ++ ") / (" ++ prettyExpr m ++ ")"

-- Morse dictionary
mdict = [
  ( 'A' , ".-"),
  ('B' , "-..."),
  ('C' , "-.-."),
  ('D' , ".,,"),
  ('E' , "."),
  ('F' , "..-."),
  ('G' , "--."),
  ('H' , "...."),
  ('I' , ".."),
  ('J' , ".---"),
  ('K' , "-.-"),
  ('L' , ".-.."),
  ('M' , "--"),
  ('N' , "-."),
  ('O' , "---"),
  ('P' , ".--."),
  ('Q' , "--.-"),
  ('R' , ".-."),
  ('S' , "..."),
  ('T' , "-"),
  ('U' , "..-"),
  ('V' , "...-"),
  ('W' , ".--"),
  ('X' , "-..-"),
  ('Y' , "-.--"),
  ('Z' , "--..")]

-- Look up the morse string for a character.
mlookup :: Char -> [(Char, String)] -> String
mlookup c ((t1,t2):[]) = if c == t1
                          then t2
                          else ""
mlookup c ((t1,t2):dict) = if c == t1
                          then t2
                          else mlookup c dict

-- Attempt to find character from morse string.
mrlookup :: String -> [(Char, String)] -> Char
mrlookup str ((t1,t2):[]) = if str == t2
                          then t1
                          else '*' -- TODO: What to do without a match?
mrlookup str ((t1,t2):dict) = if str == t2
                          then t1
                          else mrlookup str dict


encode :: String -> String
encode ""     = ""
encode (c:cs) = mlookup c mdict ++ encode cs

decode :: String -> String
decode _ = "IMMA CRY :'("
