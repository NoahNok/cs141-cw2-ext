--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone                                                --
--------------------------------------------------------------------------------

-- | This module contains the types for the abstract syntax tree.
module Language where
import Data.Foldable
--------------------------------------------------------------------------------


-- | Stores all the programs functions
newtype Functions = Functions [(String, Func)]

instance Show Functions where -- Show implementation for debugging (And to show I can implement :))
    show _ = "Functions"


-- | A program consists of a sequence of statements.
type Program = [Stmt]

-- | A program is a sequence of statements.
data Stmt
    = AssignStmt {
        assignVar  :: String,
        assignExpr :: Expr
    }
    | IfStmt {
        ifCond   :: Expr,
        ifBody   :: [Stmt],
        ifElseIf :: [(Expr,[Stmt])],
        ifElse   :: [Stmt]
    }
    | RepeatStmt {
        repeatTimesExpr :: Expr,
        repeatBody      :: [Stmt]
    }
    | RunProc String [Stmt] -- Will run a procedure that returns memory
    | EarlyRet Expr Expr -- Execute a early return from a function
    deriving Show


--------------------------------------------------------------------------------

-- | Operators.
data Op
    = Add                               -- ^ The + operator.
    | Sub                               -- ^ The - operator.
    | Mul                               -- ^ The * operator.
    | Div                               -- ^ The / operator.
    | Pow                               -- ^ The power of operator.
    | Equal                             -- ^ The == operator.
    | Neq                               -- ^ The /= operator.
    | LessThan                          -- ^ The < operator.
    | LessOrEqual                       -- ^ The <= operator.
    | GreaterThan                       -- ^ The > operator.
    | GreaterOrEqual                    -- ^ The >= operator.
    deriving (Eq, Enum, Bounded, Show)

-- | Converts the Op data type to the actual function,
-- | Returns either an integer or boolean result that can be sorted
opToOp :: Op -> Either (Int -> Int -> Int) (Int -> Int -> Bool)
opToOp Add = Left (+)
opToOp Sub = Left (-)
opToOp Mul = Left (*)
opToOp Div = Left div
opToOp Pow = Left (^)
opToOp Equal = Right (==)
opToOp Neq = Right (/=)
opToOp LessThan = Right (<)
opToOp LessOrEqual = Right (<=)
opToOp GreaterThan = Right (>)
opToOp GreaterOrEqual = Right (>=)




-- | Expressions.
data Expr
    = ValE Int
    | VarE String
    | BinOpE Op Expr Expr
    | RunFunc String [Stmt] -- Will run a function that returns a value for use in expressions
    | MissingRet
    deriving Show

-- | Function Definition
-- Func (Possible Return Value) Body
data Func = Func (Maybe Expr) Program

instance Show Func where -- Quick show instance to satisfy Func type
    show _ = "Function"

--------------------------------------------------------------------------------
