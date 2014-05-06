module Combinatori where

data Comb   =   K | S | I | W | B | C
            |   App ( Comb, Comb )
            |   Var Integer
    deriving Show

eval :: Comb -> Comb
eval (Var n)    =   Var n
eval K  =   K
eval S  =   S
eval I  =   I
eval W  =   W
eval B  =   B
eval (App( App( K,x ),y))           =   eval x
eval (App( App ( App(S,x), y), z))  =   eval (App(App(x,z), App(y,z)))
eval (App( App( App( B,x ),y ),z )) =   eval (App( x, App( y,z )))
eval (App( App( App( C,x ),y ), z)) =   eval (App( App( x,z ),y ))
eval (App( I,x ))                   =   x


x = Var 1
y = Var 2
z = Var 3

b = App( App( Combinatori.S, App( K, Combinatori.S) ), K)
