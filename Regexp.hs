module Regexp where

data Regexp     =   Empty
                |   Epsilon
                |   Symbol Char 
                |   Sec Regexp Regexp
                |   Star Regexp
                |   Or  Regexp Regexp
instance Show Regexp where
    show Empty      =   "()"
    show Epsilon    =   "ε"
    show (Symbol c) =   show c 
    show (Sec e f)  =   show e ++ "." ++ show f
    show (Star e)   =   "(" ++ show e ++ ")*"
    show (Or e f)   =   "(" ++ show e ++ " union " ++ show f ++ ")"

eps :: Regexp -> Bool
eps Empty       =   False
eps Epsilon     =   True
eps (Star e)    =   True
eps (Symbol c)  =   False
eps (Sec e f)   =   eps e && eps f
eps (Or e f)    =   eps e || eps f

derive :: Char -> Regexp -> Regexp
derive _ Empty      =   Empty
derive _ Epsilon    =   Empty
derive c (Symbol d) |   c == d       =   Epsilon
                    |   otherwise   =   Empty
derive c (Or e f)   =   Or (derive c e) (derive c f)
derive c (Sec e f)  |   eps e       =   Or (derive c f) (Sec (derive c e) f)
                    |   otherwise   =   Sec (derive c e) f
derive c (Star e)   =   Sec (derive c e) (Star e)

is  ::   [Char] -> Regexp -> Bool
is  []  e       =   eps(e)
is  (c:cs)  e   =   is cs (derive c e)

isFold  ::   [Char] -> Regexp -> Bool
isFold l e = eps((foldl (\x xs -> derive xs x) e) l)

lmatch  []  e       =   e
lmatch (s : ss) e   |   not (eps e)   =   e 
                    |   otherwise   =   lmatch ss (derive s e)


{--
 -  lmatch s e = (t, e') dove t è il piu lungo prefisso di s e e' è derivata di e rispetto a t
 -  modificare la f.ne di matching e invece di rest. vero o falso mangia la stringa e quando
 -  non genera piu stringa vuota ritorna resto espr res
 -
 -      s = "abc" 
 -      e = (a + b)*
 -      ris -> ("ab",(a+b)*)
 -
 -      s = "aaa"
 -      e = (aa)*
 -      ris -> ("aa", a(aa)*)
 -
 -      s = "aaaa"
 -      e = (aa)*
 -      ris -> ("aaaa",(aa)*)
 -
 -  sub E1 E2 (E1 E2 senza stellina)
 -      dice se L(E1) incluso in L(E2)
 -
 -      sub (aa + epsilon) (epsilon + a +aa)    =   true
 -      sub (epsilon + a + aa) (aa + epsilon)   =   false
 -  
 -}






apiubs = Star(Or (Symbol 'a') (Symbol 'b'))
empty   =   Empty
epsilo  =   Epsilon
espr    =   (Star (Or (Symbol 'a') (Sec (Symbol 'b') (Symbol 'c'))))
stringa = "cbabc"




