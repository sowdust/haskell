module MyList where

myLength :: [a] -> Integer
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x : xs) ys = x : append xs ys

myHead :: [a] -> a
myHead (x:_) = x

myElem ::  Eq a => a -> [a] -> Bool
myElem n []     =   False
myElem n (x:xs) |   x == n       =   True
                |   otherwise   =   myElem n xs

myReverse :: [a] -> [a]
myReverse []        =   []
myReverse (x:xs)    =   myReverse xs ++ [x]

myTail :: [a] -> [a]
myTail []           =   []
myTail (x:xs)       =   xs

myConcat :: [[a]] -> [a]
myConcat [x]    = x
myConcat (x:xs) = x ++ (myConcat xs)

myNElem :: [a] -> Int -> a
myNElem l n =   aux l 1
    where
    aux (x:xs) c  |   (c == n)    =   x
                  |   (c < n)    =   aux xs (c+1)

myOrd :: (Ord a) => [a] -> Bool
myOrd []        =   True
myOrd (x:xs)    =   aux x xs
    where
    aux _ []          =   True
    aux l (a:as)    |   (l <= a)     =   aux a as
                    |   otherwise   =   False

myNull :: [a] -> Bool
myNull  []  =   True
myNull  x   =   False

myDrop :: Int -> [a] -> [a]
myDrop 0 l  =   []
myDrop n l  =   aux l n []
    where
    aux r 0 _       =   r
    aux (x:xs) c s  =   aux xs (c-1) (s ++ [x])

myTake :: Int -> [a] -> [a]
myTake 0 l  =   []
myTake n (x:xs) =   x:(myTake (n-1) xs)

mySum ::  Num a => [a] -> a
mySum   []      =   0
mySum   (x:xs)  =   x + (mySum xs)

myMax :: Ord a => [a] -> a
myMax (x:[])    =   x
myMax (x:xs)    | (x >= (myMax xs))   =   x
                | otherwise             =   myMax xs

myDuplicate :: Eq a => [a] -> [a]
myDuplicate []      =   []
myDuplicate (x:[])  =   [x]
myDuplicate (x:(y:ys))  |   x==y         =   myDuplicate(x:ys)
                        |   otherwise   =   x:(myDuplicate (y:ys))

myDuplicate2    :: Eq a => [a] -> [a]
myDuplicate2 [] =   []
myDuplicate2 (x:xs) |   myElem x xs =   myDuplicate2 xs
                    |   otherwise   =   x : (myDuplicate2 xs)
