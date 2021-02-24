--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone                                                --
--------------------------------------------------------------------------------

module Interpreter where

--------------------------------------------------------------------------------

import Language

--------------------------------------------------------------------------------

-- | In our memory, named locations map to values.
type Memory = [(String, Int)]

-- | Enumerates reasons for errors.
data Err
    = DivByZeroError                    -- ^ Division by zero was attempted.
    | NegativeExponentError             -- ^ Raising a number to a negative
                                        -- exponent was attempted.
    | UninitialisedMemory String        -- ^ Tried to read from a variable
                                        -- that does not exist.
    deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Given a program and the initial memory contents, determines
-- what evaluating the program does to the memory.
interpret :: Program -> Memory -> Either Err Memory
interpret [] mem = Right mem
interpret (x:xs) mem = do
    res <- evalStatement x mem
    interpret xs res

-- | Evaluates the given statement 
evalStatement :: Stmt  -> Memory -> Either Err Memory

-- Take the given expression and evaluate it, if it produces an error, return it otherwise
-- return the memory after setting the memory value to the given value from evaluating the
-- expression into the memory location with key string
evalStatement (AssignStmt string expr) mem = do
    res <- evalExpression expr mem
    return $ setMemory string res mem
        
-- First we take the given if and attach it and its program to the start of the list of else if's
-- We then run it through 'evalIf' which will return either a program or and error
-- If we encounter and error we pass it as the return, otherwise we call 'interpret' to run
-- the given list of statements (program) returning its result as it matches the same as this
-- functions return type, allowing the memory or error to be passed back and mitigated through
-- the program
evalStatement (IfStmt expr1 program1 ifelses elseProg) mem = do
    res <- evalIf ((expr1,program1):ifelses) elseProg mem
    interpret res mem

-- Take the given expression and evaluate it, this is how many times to repeat, unless the
-- expression evaluates to an error, in that case we return the error, otherwise we call
-- 'evalRepeat' which will repeat the given statements as many times as the expression evaluated
-- to, which we return either the error or value it returns from the function
evalStatement (RepeatStmt expression statements) mem = do
    res <- evalExpression expression mem
    evalRepeat statements mem res

-- | Returns the given program to run, based on the if
-- Sequentially executes each expression, if it is true (not 0) then we return the associated 
-- program. If we exhaust the list then we return the else case
-- We require a memory pass through as evaluating expressions requires memory access as it may 
-- need to pull a variable
evalIf :: [(Expr, Program)] -> Program -> Memory -> Either Err Program
evalIf [] elseProg _ = Right elseProg
evalIf ((condition,program):xs) elseProg mem = do
    res <- evalExpression condition mem
    if res /= 0 then return program else evalIf xs elseProg mem

-- | Repeats the given list of statements until the repeats hit 0.
-- Will return the memory straight away if 0 or a negative number is encountered
-- For each repeat we call 'interpret' with the current memory and list of statements
-- We then check to see if it produce an error or the memory for the next iteration
-- If it errors we return the error, otherwise we repeat with the returned memory with the
-- repeat count minus 1. When it terminates it returns either the current state of the memory
-- Or an error if it occurred (error is returned immediately when it happens)
evalRepeat :: Program -> Memory -> Int -> Either Err Memory 
evalRepeat _ mem 0 = Right mem
evalRepeat program mem repeats = if repeats < 0 then Right mem else do
    res <- interpret program mem
    evalRepeat program res (repeats-1)

-- | Evaluates the given expression
evalExpression :: Expr -> Memory -> Either Err Int
-- ValE just needs to return the value it is holding
evalExpression (ValE value) _ = Right value;

-- VarE needs to load the given value from memory
-- We attempt to get it from memory by calling 'lookup'
-- We then check to see if it returned a value or nothing
-- If a value we return the value from within the 'Just' otherwise we were given 'Nothing' so we
-- need to return the UninitialisedMemory error for the given string
-- Can't use <- as we need to change a return of Nothing to a error
evalExpression (VarE string) memory = do
    let found = lookup string memory
    case found of
        Just value -> Right value
        Nothing -> Left $ UninitialisedMemory string

-- BinOpE needs to first check that the given expression evaluate to numbers, if they don't we
-- need to return the respective error. 
-- If both return valid values we can call 'operation' with our values and return the result
-- We deal with the error of operation outside of this function so we don't need to deal with
-- it here
evalExpression (BinOpE op e1 e2) memory = do
    v1 <- evalExpression e1 memory
    v2 <- evalExpression e2 memory
    operation v1 v2 op
    
-- | Given two integers and an operation we first check the two error cases for dividing by 0
-- and a negative exponent. (We manually do the calc for neg exponent as its quicker)
-- Then we call 'toOp' which returns the actual operator associated with the type
-- Then we can use a case statement to use Left for arithmetic operations and Right for boolean
-- operations that require returning 1 or 0 (True or False) whereas arithmetic operations
-- need to return the actual value computed
operation :: Int -> Int -> Op -> Either Err Int
operation _ 0 Div = Left DivByZeroError
operation v1 v2 Pow = if v2 < 0 then Left NegativeExponentError else Right $ v1 ^ v2
operation v1 v2 op = case toOp op of
    Left arith -> Right $ arith v1 v2
    Right bool -> Right $ if bool v1 v2 then 1 else 0

-- | Will set the value in memory if it doesn't exist, otherwise it will update it
-- Loops through all pairs. If a pair has a matching key we update its value, otherwise
-- if it doesn't exist we wil exhaust the list and then add it onto the end
setMemory :: String -> Int -> Memory -> Memory
setMemory key value [] = [(key, value)]
setMemory key value ((k,v):xs)
    | key == k = (k, value) : xs
    | otherwise = (k,v) : setMemory key value xs

--------------------------------------------------------------------------------