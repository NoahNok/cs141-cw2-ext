--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 2: Scratch clone                                                --
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Data.List (nub)

import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Ingredients.Basic
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.Tasty.Runners.AntXML

import Language
import Interpreter

--------------------------------------------------------------------------------

instance Arbitrary Op where
    arbitrary = elements [minBound..maxBound]

safeOps :: [Op]
safeOps = [op | op <- [minBound..maxBound], op /= Div, op /= Pow]

instance Arbitrary Expr where
    arbitrary = resize 10 $ sized arbExpr
        where
            arbExpr :: Int -> Gen Expr
            arbExpr 0 = do
                n <- arbitrary
                return (ValE n)
            arbExpr n = do
                m <- elements [0..n-1]
                op <- elements safeOps
                l <- arbExpr (n-1-m)
                r <- arbExpr m
                return $ BinOpE op l r

--------------------------------------------------------------------------------

isSuccessful :: Either a b -> Bool
isSuccessful (Right _) = True
isSuccessful _         = False

hasMemory :: Memory -> Either Err Memory -> Bool
hasMemory xs (Right ys) = all (`elem` ys) xs
hasMemory _  _          = False

-- | `terminates` @result@ essentially does nothing except pattern-match on 
-- @result@. If pattern-matching succeeds (i.e. the computation calculating
-- the argument terminates), `True` is returned.
terminates :: Either a b -> Bool 
terminates (Left _)  = True
terminates (Right _) = True

--------------------------------------------------------------------------------

-- | Constructs a test for n-many iterations.
repeatTest :: Int -> Program
repeatTest n =
    [ RepeatStmt (ValE n)
        [ AssignStmt "x" (BinOpE Add (VarE "x") (ValE 1)) ]
    ]

-- | Constructs a test for an arbitary expression in the repeat statement.
repeatArbitraryTest :: Expr -> Program
repeatArbitraryTest expr =
    [ RepeatStmt expr
        [ AssignStmt "x" (BinOpE Add (VarE "x") (ValE 1)) ]
    ]

-- | Constructs a test for if statements.
rawIfTest :: Expr -> Int -> Program
rawIfTest expr n =
    [ IfStmt expr
        [ AssignStmt "x" (ValE n) ]
        []
        []
    ]

-- | Constructs a test for if statements.
ifTest :: Expr -> Int -> Program
ifTest expr n = rawIfTest (BinOpE Equal expr expr) n

-- | Constructs a test for else.
elseTest :: Expr -> Int -> Program
elseTest expr n =
    [ IfStmt (BinOpE Neq expr expr)
        [ AssignStmt "x" (ValE (n+1)) ]
        []
        [ AssignStmt "x" (ValE n) ]
    ]

-- | Constructs a test for if else.
rawIfElseTest :: Expr -> Int -> Program
rawIfElseTest expr n =
    [ IfStmt (BinOpE Neq (ValE 0) (ValE 0))
        [ AssignStmt "x" (ValE (n+1)) ]
        [ (expr, [ AssignStmt "x" (ValE n) ]) ]
        [ AssignStmt "x" (ValE (n-1)) ]
    ]

-- | Constructs a test for if else.
ifElseTest :: Expr -> Int -> Program
ifElseTest expr n = rawIfElseTest (BinOpE Equal expr expr) n

-- | 'fibs' is a stream of fibonacci numbers.
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- | 'fib' @n@ constructs a program which calculates the @n@th fibonacci number.
fib :: Int -> Program
fib n =
    [ RepeatStmt (ValE n)
        [
            AssignStmt "x" (VarE "y"),
            AssignStmt "y" (VarE "z"),
            AssignStmt "z" (BinOpE Add (VarE "x") (VarE "y"))
        ]
    ]

missingFunc :: Functions
missingFunc = Functions [("start", Func Nothing [RunProc "missing" []])]

validFunc :: Functions
validFunc = Functions [("start", Func Nothing [RunProc "valid" []]), ("valid", Func Nothing [])]

missingFuncRet :: Functions
missingFuncRet = Functions [("start", Func Nothing [AssignStmt "x" (RunFunc "valid" [])]), ("valid", Func Nothing [])]

hasFuncRet :: Functions
hasFuncRet = Functions [("start", Func Nothing [AssignStmt "x" (RunFunc "valid" [])]), ("valid", Func (Just $ ValE 1) [])]


newFib :: Int -> Functions
newFib n = Functions [("start", Func Nothing (fib n))]

--------------------------------------------------------------------------------

-- | The tests.
tests :: TestTree
tests = localOption (Timeout (5*1000000) "5s") $ testGroup "Interpreter.interpret" 
    [
        testCase "handles missing start" $
        isSuccessful (interpretStart (Functions []) []) @?= False,
        testCase "handles the empty program" $
        isSuccessful (interpretStart (Functions [("start", Func Nothing [])]) []) @?= True,
        testCase "handles missing function" $
        isSuccessful (interpretStart missingFunc []) @?= False,
        testCase "handles existing function" $
        isSuccessful (interpretStart validFunc []) @?= True,
        testCase "handles function missing return type" $
        isSuccessful (interpretStart missingFuncRet []) @?= False,
        testCase "handles function with return type" $
        isSuccessful (interpretStart hasFuncRet []) @?= True,
        testGroup "Example programs" [
            QC.testProperty "computes fibonacci numbers in extension format" $ \(Positive n) ->
                hasMemory [("x", fibs !! (n-1))] $ interpretStart (newFib n) [("x",0),("y",0),("z",1)]
        ]

        
    ]

-- | The list of tasty ingredients. Note: the order seems to matter, 
-- anyXMLRunner won't work at all if placed last in the list.
ingredients :: [Ingredient]
ingredients = [antXMLRunner, listingTests, consoleTestReporter]

-- | The main entry point to the test suite.
main :: IO ()
main = defaultMainWithIngredients ingredients tests

--------------------------------------------------------------------------------