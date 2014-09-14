-- Assignment for Week 2 of AP
-- Martin Jørgensen, tzk173
-- Casper B. Hansen, fvx507

module MSM where

import Data.Map (Map)
import qualified Data.Map as Map -- used for registers

import Control.Monad

data Inst
    = PUSH Int      -- pushes the integer constant n on top of the stack
    | POP           -- removes the top element of the stack
    | DUP           -- duplicates the top element of the stack
    | SWAP          -- swaps the two top elements of the stack
    | NEWREG Int    -- allocates a new register n
    | LOAD          -- removes the top element n of the stack,
                    -- and pushes the content of register n on the stack
    | STORE         -- removes the two top elements m and n of the stack, and
                    -- and stores the value m in register n. That is, store
                    -- the top of the stack in the register denoted by the
                    -- second topmost element of the stack, and remove both
                    -- elements from the stack
    | NEG           -- negate the top element of the stack
    | ADD           -- removes the two top elements of the stack, adds them,
                    -- and pushes the result to the top of the stack
    | JMP           -- sets the PC (program counter) to the value of the top
                    -- element of the stack and removes it
    | CJMP Int      -- removes the top element of the stack, if it is less
                    -- than zero the PC is set to i, otherwise the PC (program
                    -- counter) is incremented by one
    | HALT          -- halt the machine without an error
    deriving (Eq,Show)

type Prog = [Inst]
type Stack = [Int]
type Regs  = Map Int Int  -- key-value mappings?
data State = State
             { prog     :: [Inst]
             , pc       :: Int
             , stack    :: [Int]
             , regs     :: Regs
             }
    deriving (Show)

data ErrorType = StackUnderflow
               | UnallocatedRegister Int
               | RegisterAlreadyAllocated
               | InvalidPC
               | Unspec String
    deriving (Show, Read, Eq)

data Error = Error { errorType :: ErrorType } -- could add generic error messages
    deriving (Show, Eq)

strToErr :: String -> Error
strToErr s = case s of
                "Invalid PC" -> Error { errorType=InvalidPC }

-- constructs the initial state of an MSM running the program
initial :: Prog -> State
initial p = State {prog=p, pc=0, stack=[], regs=Map.empty}

newtype MSM a = MSM (State -> Either Error (State, a)) -- a is a return-value

-- type ValidState = Right
-- type ErrorState = Left

instance Monad MSM where
    -- (>>=) :: MSM a -> (a -> MSM b) -> MSM b
    -- p s = Either Error State
    (MSM p) >>= f = MSM (\x -> case (p x) of
                                    Right (s, r) -> let (MSM m) = f r
                                                    in m s
                                    Left e -> Left e
                        )
        
    -- return :: a -> MSM a
    return a = MSM (\s -> Right (s, a))
    
    -- fail :: String -> MSM a
    fail s = MSM (\_ -> Left (strToErr s))

instance Functor MSM where
    -- fmap :: (Functor f) => (a -> b) -> f a -> f b
    fmap f xs = xs >>= return . f

-- monadic functions
get :: MSM State
get = MSM (\s -> Right (s,s))

set :: State -> MSM ()
set s = MSM (\_ -> Right (s,()))

modify :: (State -> State) -> MSM ()
modify f = do
    state <- get
    MSM (\_ -> Right (f state, () ) )

push :: Int -> MSM ()
push a = do
    state <- get
    set state{stack=a:stack state, pc=pc state + 1}

pop :: MSM ()
pop = do
    state <- get
    if length (stack state) < 1
    then fail "Stack Underflow"
    else set state{stack=tail (stack state), pc=pc state + 1}
    
-- PREVIOUS, trying to match return-types
--    State {prog=p, pc=i, stack=s:ss, regs=r} <- get
--    set State{prog=p, pc=i+1, stack=ss, regs=r}
--    return s

dup :: MSM ()
dup = do
    State {prog=p, pc=i, stack=s:ss, regs=r} <- get
    set State{prog=p, pc=i+1, stack=s:s:ss, regs=r}

swap :: MSM ()
swap = do
    State {prog=p, pc=i, stack=a:b:ss, regs=r} <- get
    set State{prog=p, pc=i+1, stack=b:a:ss, regs=r}

getInst :: MSM Inst
getInst = do
    state <- get
    if (pc state) < 0 || (pc state) > length (prog state)
    then fail "Invalid PC"
    else return $ (prog state) !! (pc state)

interpInst :: Inst -> MSM Bool
interpInst inst = do
    state <- get
    case inst of
        POP -> pop
    return True

interp :: MSM ()
interp = run
    where run = do inst <- getInst
                   cont <- interpInst inst
                   when cont run

runMSM :: Prog -> Either Error ()
runMSM p = let (MSM f) = interp
           in fmap snd $ f $ initial p

-- example program, expected to leave 42 on the top of the stack
pushPop = [PUSH 1, PUSH 2, POP, PUSH 3, HALT]
p42 = [NEWREG 0, PUSH 1, DUP, NEG, ADD, PUSH 40, STORE, PUSH 2, PUSH 0, LOAD, ADD, HALT]
