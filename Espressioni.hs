module Espressioni where

data Expr   =   Const Integer
            |   X
            |   Add Expr Expr
            |   Sub Expr Expr
            |   Mul Expr Expr
            |   Div Expr Expr
            |   Pow Expr Integer
    deriving Show

printExpr :: Expr -> String
printExpr   X           =   "x"
printExpr   (Const n)   =   show n
printExpr   (Add e f)   =   wrap $ printExpr e ++ "+" ++ printExpr f
printExpr   (Sub e f)   =   wrap $ printExpr e ++ "-" ++ printExpr f
printExpr   (Mul e f)   =   wrap $ printExpr e ++ "*" ++ printExpr f
printExpr   (Div e f)   =   wrap $ printExpr e ++ "`div`" ++ printExpr f
printExpr   (Pow e n)   =   wrap $ printExpr e ++ "^" ++ show n

wrap :: String -> String
wrap s = "(" ++ s ++ ")"






expr = Add (Pow X 2) (Add (Mul (Const 2) X) (Const 1))
