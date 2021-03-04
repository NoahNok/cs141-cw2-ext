--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone                                                --
--------------------------------------------------------------------------------

module Interpreter where

--------------------------------------------------------------------------------

import Language
import Data.List


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
    | FunctionMissing String -- Custom error for missing function
    | FunctionShouldHaveReturn String -- Custom error for missing function return
    | FunctionMissingEarlyReturn
    deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Starting point of the interpreter, forcefully calls the `start` main method
interpretStart :: Functions -> Memory -> Either Err Memory
interpretStart (Functions funcs) mem = do
    (Func _ prog) <- maybeToEither (FunctionMissing "start") (lookup "start" funcs)
    interpret prog mem (Functions funcs)

-- | Given a program and the initial memory contents, determines
-- what evaluating the program does to the memory.
interpret :: Program -> Memory -> Functions -> Either Err Memory
interpret [] mem _ = Right mem
interpret (x:xs) mem funcs = do
    res <- evalStatement x mem funcs
    if hasEarlyRet res then Right res else interpret xs res funcs


-- | Evaluates the given statement 
evalStatement :: Stmt  -> Memory -> Functions -> Either Err Memory

-- Take the given expression and evaluate it, if it produces an error, return it otherwise
-- return the memory after setting the memory value to the given value from evaluating the
-- expression into the memory location with key string
evalStatement (AssignStmt string expr) mem f = do
    res <- evalExpression expr mem f
    return $ setMemory string res mem

-- First we take the given if and attach it and its program to the start of the list of else if's
-- We then run it through 'evalIf' which will return either a program or and error
-- If we encounter and error we pass it as the return, otherwise we call 'interpret' to run
-- the given list of statements (program) returning its result as it matches the same as this
-- functions return type, allowing the memory or error to be passed back and mitigated through
-- the program
evalStatement (IfStmt expr1 program1 ifelses elseProg) mem f = do
    res <- evalIf ((expr1,program1):ifelses) elseProg mem f
    interpret res mem f

-- Take the given expression and evaluate it, this is how many times to repeat, unless the
-- expression evaluates to an error, in that case we return the error, otherwise we call
-- 'evalRepeat' which will repeat the given statements as many times as the expression evaluated
-- to, which we return either the error or value it returns from the function
evalStatement (RepeatStmt expression statements) mem f = do
    repeats <- evalExpression expression mem f
    evalRepeat statements repeats mem f

-- Run a given procedure returning memory 
evalStatement (RunProc name params) mem f = runProc name params mem f

evalStatement (EarlyRet condition toRet) mem f = do
    res <- evalExpression condition mem f
    if res == 0 then Right mem else do
        ret <- evalExpression toRet mem f
        let newMem = setMemory "PROC_EARLY_RETURN" ret mem
        Right newMem

-- | Returns the given program to run, based on the if
-- Sequentially executes each expression, if it is true (not 0) then we return the associated 
-- program. If we exhaust the list then we return the else case
-- We require a memory pass through as evaluating expressions requires memory access as it may 
-- need to pull a variable
evalIf :: [(Expr, Program)] -> Program -> Memory -> Functions -> Either Err Program
evalIf [] elseProg _ _ = Right elseProg
evalIf ((condition,program):xs) elseProg mem f = do
    res <- evalExpression condition mem f
    if res /= 0 then return program else evalIf xs elseProg mem f

-- | Repeats the given list of statements until the repeats hit 0.
-- Will return the memory straight away if 0 or a negative number is encountered
-- For each repeat we call 'interpret' with the current memory and list of statements
-- We then check to see if it produce an error or the memory for the next iteration
-- If it errors we return the error, otherwise we repeat with the returned memory with the
-- repeat count minus 1. When it terminates it returns either the current state of the memory
-- Or an error if it occurred (error is returned immediately when it happens)
evalRepeat :: Program -> Int -> Memory -> Functions -> Either Err Memory
evalRepeat _ 0 mem _ = Right mem
evalRepeat program repeats mem funcs
    | repeats < 0 = Right mem
    | otherwise = do
    res <- interpret program mem funcs
    if hasEarlyRet res then Right res else evalRepeat program (repeats-1) res funcs


-- | Evaluates the given expression
evalExpression :: Expr -> Memory -> Functions -> Either Err Int
-- ValE just needs to return the value it is holding
evalExpression (ValE value) _ _ = Right value;

-- VarE needs to load the given value from memory
-- We attempt to get it from memory by calling 'lookup'
-- We then check to see if it returned a value or nothing
-- If a value we return the value from within the 'Just' otherwise we were given 'Nothing' so we
-- need to return the UninitialisedMemory error for the given string
-- Can't use <- as we need to change a return of Nothing to a error
evalExpression (VarE string) memory _ = do
    maybeToEither (UninitialisedMemory string) (lookup string memory)


-- BinOpE needs to first check that the given expression evaluate to numbers, if they don't we
-- need to return the respective error. 
-- If both return valid values we can call 'operation' with our values and return the result
-- We deal with the error of operation outside of this function so we don't need to deal with
-- it here
evalExpression (BinOpE op e1 e2) memory f = do
    v1 <- evalExpression e1 memory f
    v2 <- evalExpression e2 memory f
    operation v1 v2 op

-- RUns a given function returning a value
evalExpression (RunFunc name params) memory f = runFunc name params memory f

-- | Given two integers and an operation we first check the two error cases for dividing by 0
-- and a negative exponent. (We manually do the calc for neg exponent as its quicker)
-- Then we call 'toOp' which returns the actual operator associated with the type
-- Then we can use a case statement to use Left for arithmetic operations and Right for boolean
-- operations that require returning 1 or 0 (True or False) whereas arithmetic operations
-- need to return the actual value computed
operation :: Int -> Int -> Op -> Either Err Int
operation _ 0 Div = Left DivByZeroError
operation v1 v2 Pow = if v2 < 0 then Left NegativeExponentError else Right $ v1 ^ v2
operation v1 v2 op = case opToOp op of
    Left arith -> Right $ arith v1 v2
    Right bool -> Right $ if bool v1 v2 then 1 else 0


-- | Will set the value in memory if it doesn't exist, otherwise it will update it
-- Loops through all pairs. If a pair has a matching key we update its value, otherwise
-- if it doesn't exist we wil exhaust the list and then add it onto the end
-- Could use a Map, but from testing converting the memory to a map at the start and then back at
-- the end is actually slower overall according to the bench method, due to O(n) being faster than
-- O(n * log(n)) for larger values of n
setMemory :: String -> Int -> Memory -> Memory
setMemory key value [] = [(key, value)]
setMemory key value ((k,v):xs)
    | key == k = (k, value) : xs
    | otherwise = (k,v) : setMemory key value xs


-- ███████ ██   ██ ████████ ███████ ███    ██ ███████ ██  ██████  ███    ██ 
-- ██       ██ ██     ██    ██      ████   ██ ██      ██ ██    ██ ████   ██ 
-- █████     ███      ██    █████   ██ ██  ██ ███████ ██ ██    ██ ██ ██  ██ 
-- ██       ██ ██     ██    ██      ██  ██ ██      ██ ██ ██    ██ ██  ██ ██ 
-- ███████ ██   ██    ██    ███████ ██   ████ ███████ ██  ██████  ██   ████ 


-- Extension Methods

-- | Runs the given procedure
-- First we check that the given function exists, if it doesn't we throw a FunctionMissing error
-- Otherwise we execute the initial function statements if any, binding the valid result to
-- `newMem`, with that we then interpret the functions body and return its memory once finished
-- We can ignore the first param of `Func` since we don't care about a return type for a procedure
runProc :: String -> [Stmt] -> Memory -> Functions -> Either Err Memory
runProc funcName initVars mem (Functions funcs) = do
    (Func _ prog) <- maybeToEither (FunctionMissing funcName) (lookup funcName funcs)

    newMem <- execInitialExpressions initVars mem (Functions funcs)
    finalMem <- interpret prog newMem (Functions funcs)
    if hasEarlyRet finalMem then Right $ deleteMem "PROC_EARLY_RETURN" finalMem else Right finalMem -- Cleans up memory to be shown


-- ABSTRACT OUT DUP CODE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- | Runs the given function
-- This is very similar to above, with the only different being that we return a value based on the
-- final memory. 
-- This will either be a integer from `ValE` or an integer from the final memory via `VarE`
-- If it is missing a return type then we return a error as it requires one to function, otherwise
-- We evaluate the return expression with the final memory returning either an Int or an Error
-- 
-- A nice part about this is that we don't have to modify the memory structure since we can just 
-- forget about the memory when we return a value, thus each function call operates on its given
-- memory in what is essentially a local state. Thus recursive method calls don't affect each other
runFunc :: String -> [Stmt] -> Memory -> Functions  -> Either Err Int
runFunc funcName initVars mem (Functions funcs) = do
    (Func ret prog) <- maybeToEither (FunctionMissing funcName) (lookup funcName funcs)

    newMem <- execInitialExpressions initVars mem (Functions funcs)
    finalMem <- interpret prog newMem (Functions funcs)

    if hasEarlyRet finalMem then processEarlyReturn finalMem else do
        retF <- maybeToEither (FunctionShouldHaveReturn funcName) ret
        evalExpression retF finalMem (Functions funcs)


-- | Executes the initial function values
-- It is possible for a function to take initial values (params) so here we take each of them and
-- set them in memory so that the function can use them
execInitialExpressions :: [Stmt] -> Memory -> Functions -> Either Err Memory
execInitialExpressions [] mem _ = Right mem
execInitialExpressions (x:xs) mem f = do
    res <- evalStatement x mem f
    execInitialExpressions xs res f

hasEarlyRet :: Memory -> Bool
hasEarlyRet mem = fst (last mem) == "PROC_EARLY_RETURN"

processEarlyReturn :: Memory -> Either Err Int
processEarlyReturn mem = do
    let pair = last mem
    case fst pair of
        "PROC_EARLY_RETURN" -> Right $ snd pair
        _ -> Left FunctionMissingEarlyReturn

-- | Allows key deletion from memory
deleteMem :: String -> Memory -> Memory
deleteMem key = filter ((key /=) . fst)

-- | Converts a maybe to an either with a default value
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither err = maybe (Left err) Right
--------------------------------------------------------------------------------

