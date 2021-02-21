--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone                                                --
--------------------------------------------------------------------------------

-- | This module contains the types for the abstract syntax tree.
module Language where
import Data.Foldable
--------------------------------------------------------------------------------

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
toOp :: Op -> Either (Int -> Int -> Int) (Int -> Int -> Bool)
toOp Add = Left (+)
toOp Sub = Left (-)
toOp Mul = Left (*)
toOp Div = Left div
toOp Pow = Left (^)
toOp Equal = Right (==)
toOp Neq = Right (/=)
toOp LessThan = Right (<)
toOp LessOrEqual = Right (<=)
toOp GreaterThan = Right (>)
toOp GreaterOrEqual = Right (>=)




-- | Expressions.
data Expr
    = ValE Int
    | VarE String
    | BinOpE Op Expr Expr
    deriving Show

--------------------------------------------------------------------------------
