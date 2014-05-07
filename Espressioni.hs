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


evalExpr :: Float -> Expr -> Float
evalExpr x X            =   x
evalExpr x (Const n)    =   fromIntegral n
evalExpr x (Add e f)    =   evalExpr x e + evalExpr x f
evalExpr x (Sub e f)    =   evalExpr x e - evalExpr x f
evalExpr x (Mul e f)    =   evalExpr x e * evalExpr x f
evalExpr x (Div e f)    =   evalExpr x e / evalExpr x f
evalExpr x (Pow e n)    =   evalExpr x e ** fromIntegral n


derive :: Expr -> Expr
derive (Const n)    =   Const 0
derive X            =   Const 1
derive (Pow e n)    =   Mul (Mul (Const n) (Pow e (n-1))) (derive e)
derive (Add e f)    =   Add (derive e) (derive f)
derive (Sub e f)    =   Sub (derive e) (derive f)
derive (Mul e f)    =   Add (Mul (derive e) f) (Mul e (derive f))
derive (Div e f)    =   Div (Sub (Mul (derive e) f) (Mul f (derive e))) (Pow f 2)

simplify :: Expr -> Expr
simplify    (Add e f)   =   case (simplify e, simplify f) of
                                (Const m, Const n) -> Const (m + n)
                                (Const 0, f')   -> f'
                                (e', Const 0)   -> e'
                                (e', f')        -> Add e' f'



{-
 -
 - FARE SIMPLIFY
 -
integrate :: Expr -> Expr
integrate (Const n) =   Mul(Const n X)
integrate X         =   Pow X (Const 2)
 -}

expr =  Add (Pow X (-2)) (Add ( Mul (Const 2) X) (Const 1))




