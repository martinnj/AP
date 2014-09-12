-- Assignment for Week 2 of AP
-- Martin Jørgensen, tzk173
-- Casper B. Hansen, fvx507

module MSM where

data Inst
    = PUSH Int      -- pushes the integer constant n on top of the stack
    | POP           -- removes the top element of the stack
    | DUP           -- duplicates the top element of the stack
    | SWAP          -- swaps the two top elements of the stack
    | NEWREG Int    -- allocates a new register n
    | LOAD          -- removes the top element n of the stack,
                    -- and pushes the content of register n on the stack
--  | STORE         -- removes the two top elements m and n of the stack, and
                    -- and stores the value m in register n. That is, store
                    -- the top of the stack in the register denoted by the
                    -- second topmost element of the stack, and remove both
                    -- elements from the stack
    | NEG           -- negatate the top element of the stack
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

data ErrorType = StackUnderflow
               | UnallocatedRegister Int
               | RegisterAlreadyAllocated
               | InvalidPC
               | Unspec String
