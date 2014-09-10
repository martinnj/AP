-- Assignment for Week 2 of AP
-- Martin Jørgensen, tzk173
-- Casper B. Hansen, XXXYYY

data Inst
    = PUSH Int
    | POP
    | DUP
    | SWAP
    | NEWREG Int
    | LOAD
    | NEG
    | ADD
    | JMP
    | CJMP Int
    | HALT
    deriving (Eq,Show)
type Prog = [Inst]
