module Algebra where

import Data.List (elemIndex)

{--
 - nome tipo dato (tutto lettere maiuscole): costruttori
 - come sono fatti i valori / costruire
 - meccanismo per analizzare / esaminare  valori
 -
 -
 -
 -
 -
 -
 -  i tipi di dati piu' semplici sono empty (nessun valore) e unit (un unico valore)
 -  questi tipi hanno senso nel momento in cui la funzione che li calcola ha dei 
 -  side effect. essendo che nei linguaggi lazy e' difficile implementare operazioni
 -  accessorie (side effect) nella libreria haskell NON ci sono funzioni che 
 -  ritornano tipo Unit. Per farlo si usano estensioni aggiuntive (eg Monadi)
 -
 -}

data Boolean = F | T
--  deriving (Show,Eq,Ord)

instance Show Boolean where
    show T = "Vero"
    show F = "Falso"

instance Eq Boolean where
    (==) F F = True
    (==) T T = True
    (==) _ _ = False

myAnd :: Boolean -> Boolean -> Boolean
myAnd F _   =   F
myAnd T b   =   b

myOr ::  Boolean -> Boolean -> Boolean
myOr T _    =   T
myOr F b    =   b



data Cardinale = Nord | Sud | Ovest | Est
    deriving (Show, Eq)

giraSinistra :: Cardinale -> Cardinale
giraSinistra Nord   =   Ovest
giraSinistra Ovest  =   Sud
giraSinistra Sud    =   Est
giraSinistra Est    =   Nord

tornaIndietro :: Cardinale -> Cardinale
tornaIndietro = giraSinistra . giraSinistra

{--
 - giorni della settimana:
 -  ieri
 -  domani
 -}

data Giorni = Lun | Mar | Mer | Gio | Ven | Sab | Dom
    deriving (Show, Eq, Ord)

settimana = (Lun : Mar : Mer : Gio : Ven : Sab : Dom : [])

domani :: Giorni -> Giorni
domani g  =   case elemIndex g settimana of
                Just n -> settimana !! ((n+1) `mod` 7)
ieri :: Giorni -> Giorni
ieri g  =   case elemIndex g settimana of
                Just n -> settimana !! ((n-1) `mod` 7)

dopo    =   (.) domani



{-
 - [Undefined, Infinite, Finite 0, Finite 1, Finite -1, ...]
 -}

data INat   =   Finite Int
            |   Infinite
            |   Undefined

instance Show INat where
    show (Finite n) = show n
    show Undefined  = "bottom"
    show Infinite   = "∞"

instance Eq INat where
    (Finite n) == (Finite m) = (m == n)
    Infinite == Infinite     = True
    _ == _                   = False

instance Ord INat where
    Finite n < Finite m  =  n < m
    Undefined < Infinite =  True
    Finite n > Finite m  =  n > m

idiv :: INat -> INat -> INat
idiv (Finite n) (Finite m)  |   m /= 0   =   Finite (div n m)
                            |   n /= 0   =   Infinite
                            |   otherwise   =   Undefined

idiv Undefined _    =   Undefined
idiv _ Undefined    =   Undefined
idiv (Finite _) Infinite = Finite 0
idiv Infinite Infinite = Undefined


data Tree a =   Leaf a 
            |   Node a (Tree a) (Tree a)
    deriving (Show,Eq)

depth :: Tree a -> Integer
depth (Leaf _)      =   1
depth (Node _ b c)  =   max (depth b) (depth c) + 1

preorder :: Tree a -> [a]
preorder (Leaf a)       =   [a]
preorder (Node a b c)   =   a : (preorder b) ++ (preorder c)

postorder :: Tree a -> [a]
postorder (Leaf a)      =   [a]
postorder (Node a b c)  =   (postorder b) ++ (postorder c) ++ [a]

inorder :: Tree a -> [a]
inorder (Leaf a)        =   [a]
inorder (Node a b c)    =   inorder b ++ a : (inorder c)

member :: (Eq a) => a -> Tree a -> Bool
member a (Leaf b)       =   a == b
member a (Node x b c)   =   x == a || (member a b) || (member a c)

tree = Node 0 (Node 1 (Node 2 (Node 4 (Leaf 8) (Node 9 (Leaf 10) (Leaf 11) )) (Leaf 5)) (Node 3 (Leaf 6) (Leaf 7)) ) (Leaf 0)
