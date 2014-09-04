type Pos = (Int, Int)
data Direction = North | South | East | West
    deriving(Eq, Show)

move :: Direction -> Pos -> Pos
move North(x,y) = (x, y+1)
move South(x,y) = (x, y-1)
move East(x,y)  = (x+1, y)
move West(x,y)  = (x-1, y)

moves :: [Direction] -> Pos -> Pos
moves [] p      = p
moves (d:ds) p  = moves ds (move d p)


data Nat = Zero | Succ Nat
    deriving (Eq, Show, Read, Ord)

nat2int :: Nat -> Int
nat2int Zero        = 0
nat2int (Succ n)    = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0   = Zero
int2nat n   = Succ (int2nat (n - 1))


-- note: make Node polymorphic
data Tree = Leaf | Node Int Tree Tree
    deriving (Eq, Show, Read, Ord)

insert :: Int -> Tree -> Tree
insert n Leaf           = Node n Leaf Leaf
insert n (Node m l r)   = if n <= m
                          then Node m (insert n l) r
                          else Node m l (insert n r)


data Expr = Con Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
    deriving (Eq, Show, Read, Ord)

value :: Expr -> Int
value (Con n)       = n
value (Add x y)     = value x + value y
value (Sub x y)     = value x - value y
value (Mul x y)     = value x * value y
value (Div x y)     = value x `div` value y

-- division by zero; *** Exception: divide by zero
-- Int range: -9223372036854775808 to 9223372036854775807

prettyExpr :: Expr -> String
prettyExpr (Con n)      = show n
prettyExpr (Add a b)    = "(" ++ prettyExpr a ++ " + " ++ prettyExpr b ++ ")"
prettyExpr (Sub a b)    = "(" ++ prettyExpr a ++ " - " ++ prettyExpr b ++ ")"
prettyExpr (Mul a b)    = "(" ++ prettyExpr a ++ " * " ++ prettyExpr b ++ ")"
prettyExpr (Div a b)    = "(" ++ prettyExpr a ++ " / " ++ prettyExpr b ++ ")"


