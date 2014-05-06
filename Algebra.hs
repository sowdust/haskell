module Algebra where

import Data.List (elemIndex)

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


data Valore =   I Int
            |   S String
    deriving (Show)


data Tree a =   Empty
            |   Node a (Tree a) (Tree a)
    deriving (Show, Eq)


data MyTree a   =   Leaf a 
                |   Nodo a (MyTree a) (MyTree a)
    
depth :: MyTree a -> Integer
depth (Leaf _)      =   1
depth (Nodo _ b c)  =   max (depth b) (depth c) + 1

preorder :: MyTree a -> [a]
preorder (Leaf a)       =   [a]
preorder (Nodo a b c)   =   a : preorder b ++ preorder c

postorder :: MyTree a -> [a]
postorder (Leaf a)      =   [a]
postorder (Nodo a b c)  =   postorder b ++ postorder c ++ [a]

inorder :: MyTree a -> [a]
inorder (Leaf a)        =   [a]
inorder (Nodo a b c)    =   inorder b ++ a : inorder c

member :: (Eq a) => a -> MyTree a -> Bool
member a (Leaf b)       =   a == b
member a (Nodo x b c)   =   x == a || member a b || member a c

labels :: Tree a -> [a]
labels Empty        =   []
labels (Node x l r) =   x : labels l ++ labels r

memberB :: (Ord a, Eq a) => a -> Tree a -> Bool
memberB _ Empty         =   False
memberB a (Node x b c)  |   x == a   =   True
                        |   a < x   =   memberB a b
                        |   a > x   =   memberB a c

insertB :: (Ord a, Eq a) => a -> Tree a -> Tree a
insertB x Empty         =   Node x Empty Empty
insertB x (Node n l r)  |   n == x   =   Node n l r
                        |   x < n   =   Node n (insertB x l) r
                        |   x > n   =   Node n l (insertB x r)   

leastB :: (Ord a, Eq a) => Tree a -> a
leastB (Node n Empty _) =   n
leastB (Node _ l _)     =   leastB l

deleteB :: (Ord a, Eq a) => a -> Tree a -> Tree a
deleteB _ Empty             =   Empty
deleteB x (Node n l Empty)  |   x == n       =   l
                            |   otherwise   =   Node n (deleteB x l) Empty
deleteB x (Node n Empty r)  |   x == n       =   r
                            |   otherwise   =   Node n Empty (deleteB x r)
deleteB x (Node n l r)      |   x < n   =   Node n (deleteB x l) r
                            |   x > n   =   Node n l (deleteB x r)
                            |   x == n   =   Node (leastB r) l (deleteB (leastB r) r)

tmap :: (a -> b) -> Tree a -> Tree b
tmap _ Empty        =   Empty
tmap f (Node n l r) =   Node (f n) (tmap f l) (tmap f r)

tfold :: (a -> b -> b -> b) -> b -> Tree a -> b
tfold _ b Empty                 =   b
tfold f b (Node n l r)          =   f n (tfold f b l) (tfold f b r) 

sumFold :: (Num a) => Tree a -> a
sumFold t   =   tfold (\x ->  \y -> \z -> x + y + z) 0 t

depthFold :: Tree a -> Integer
depthFold t =   tfold (\x -> \y -> \z -> (1 + (max y z)) ) 0 t

labelsFold :: Tree a -> [a]
labelsFold t    =   tfold (\x -> \y -> \z -> x : (y ++ z)) [] t




treeB = Node 10 (Node 5 (Node 2 Empty Empty) (Node 7 Empty Empty) ) (Node 15 (Node 12 Empty Empty) (Node 18 Empty Empty))
tree = Nodo 0 (Nodo 1 (Nodo 2 (Nodo 4 (Leaf 8) (Nodo 9 (Leaf 10) (Leaf 11) )) (Leaf 5)) (Nodo 3 (Leaf 6) (Leaf 7)) ) (Leaf 0)
