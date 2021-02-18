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
-- | what evaluating the program does to the memory.
interpret :: Program -> Memory -> Either Err Memory
interpret [] mem = Right mem
interpret (x:xs) mem = do
    let result = evalStatement x mem
    case result of
        Left err -> Left err
        Right memory -> interpret xs memory


-- | Evaluates the given statement 
evalStatement :: Stmt  -> Memory -> Either Err Memory

-- | Take the given expression and evaluate it, if it produces an error, return it otherwise
-- | return the memory after setting the memory value to the given value from evaluating the
-- | expression into the memory location with key string
evalStatement (AssignStmt string expr) mem = do
    let result = evalExpression expr mem
    case result of
        Left err -> Left err
        Right value -> do
            Right $ setMemory string value mem
        
evalStatement (IfStmt _ _ _ _) mem = Right mem

-- | Take the given expression and evaluate it, this is how many times to repeat, unless the
-- | expression evaluates to an error, in that case we return the error, otherwise we call
-- | 'evalRepeat' which will repeat the given statements as many times as the expression evaluated
-- | to, which we return either the error or value it returns from the function
evalStatement (RepeatStmt expression statements) mem = do
    let expressionResult = evalExpression expression mem
    case expressionResult of
        Left err -> Left err
        Right repeatTimes -> evalRepeat statements mem repeatTimes
            

-- | Repeats the given list of statements until the repeats hit 0.
-- | Will return the memory straight away if 0 or a negative number is encountered
-- | For each repeat we call 'interpret' with the current memory and list of statements
-- | We then check to see if it produce an error or the memory for the next iteration
-- | If it errors we return the error, otherwise we repeat with the returned memory with the
-- | repeat count minus 1. When it terminates it returns either the current state of the memory
-- | Or an error if it occurred (error is returned immediately when it happens)
evalRepeat :: Program -> Memory -> Int -> Either Err Memory
evalRepeat _ mem 0 = Right mem
evalRepeat program mem repeats = do
    if repeats < 0 then Right mem else do
        let res = interpret program mem
        case res of
            Left err -> Left err
            Right memory -> evalRepeat program memory (repeats-1)


-- | Evaluates the given expression
evalExpression :: Expr -> Memory -> Either Err Int
-- | ValE just needs to return the value it is holding
evalExpression (ValE value) _ = Right value;

-- | VarE needs to load the given value from memory
-- | We attempt to get it from memory by calling 'getMemory'
-- | We then check to see if it returned a value or nothing
-- | If a value we return the value from within the 'Just' otherwise we were given 'Nothing' so we
-- | need to return the UninitialisedMemory error for the given string
evalExpression (VarE string) memory = do
    let found = getMemory string memory
    case found of
        Just value -> Right value
        Nothing -> Left $ UninitialisedMemory string

-- | BinOpE needs to first check that the given expression evaluate to numbers, if they don't we
-- | need to return the respective error. 
-- | If both return valid values we can call 'operation' with our values and return the result
-- | We deal with the error of operation outside of this function so we don't need to deal with
-- | it here
evalExpression (BinOpE op e1 e2) memory = do
    let e1Value = evalExpression e1 memory
    let e2Value = evalExpression e2 memory
    
    case e1Value of
        Left err -> Left err
        Right value1 -> do
            case e2Value of
                Left err -> Left err
                Right value2 -> operation value1 value2 op



    -- Operate on values

-- Change to mapping constructor to operation, to reduce code duplication
operation :: Int -> Int -> Op -> Either Err Int
operation a b Add = Right $ a + b
operation a b Sub = Right $ a - b
operation a b Mul = Right $ a * b
operation _ 0 Div = Left DivByZeroError
operation a b Div = Right $ div a b
operation a b Pow = if b >= 0 then Right $ a^b else Left NegativeExponentError
operation a b Equal = if a == b then Right 1 else Right 0
operation a b Neq = if a /= b then Right 1 else Right 0
operation a b LessThan = if a < b then Right 1 else Right 0
operation a b LessOrEqual = if a <= b then Right 1 else Right 0
operation a b GreaterThan = if a > b then Right 1 else Right 0
operation a b GreaterOrEqual = if a >= b then Right 1 else Right 0

toInt :: Float -> Int
toInt = round


-- Will set the value in memory if it doesn't exist, otherwise it will update it
-- Loops through all pairs. If a pair has a matching key we update its value, otherwise
-- if it doesn't exist we wil exhaust the list and then add it onto the end
setMemory :: String -> Int -> Memory -> Memory
setMemory key value [] = [(key, value)]
setMemory key value ((k,v):xs)
    | key == k = (k, value) : xs
    | otherwise = (k,v) : setMemory key value xs

-- Well return the value associated with a key if it exists or Nothing
-- Loops all paris to find the one with a matching key, if so it returns it, else it exhausts the 
-- list and returns Nothing
getMemory :: String -> Memory -> Maybe Int
getMemory _ [] = Nothing
getMemory key ((k,v):xs)
    | key == k = Just v
    | otherwise = getMemory key xs


--------------------------------------------------------------------------------
