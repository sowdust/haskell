module MyList where


append :: [a] -> [a] -> [a]
append [] ys = ys
append (x : xs) ys = x : append xs ys

myLength :: [a] -> Integer
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

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

myConcatList2   ::   [[a]] -> [a]
myConcatList2   =  \(x : xs) ->  foldl (++) x xs

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

myFilter    ::   (a -> Bool) -> [a] -> ([a],[a])
myFilter p []       =   ([], [])
myFilter p (x : xs) =   let (ys, zs) = myFilter p xs in
                            if p x then (x : ys, zs)
                            else        (ys, x : zs)

myFoldL ::  (t1 -> t -> t1) -> t1 -> [t] -> t1
myFoldL f a []          =   a
myFoldL f a (x : xs)    =   myFoldL f (a `f` x) xs

myFoldR ::  (t -> t1 -> t1) -> t1 -> [t] -> t1
myFoldR f a []          =   a
myFoldR f a (x : xs)    =   x `f` myFoldR f a xs

myFoldIdentity ::  [a] -> [a]
myFoldIdentity xs = foldr (:) [] xs

myFoldReverse ::  [a] -> [a]
myFoldReverse xs = foldl snoc [] xs
    where   snoc xs x = x : xs

myFoldMax ::  Ord t => [t] -> t
myFoldMax (x : [])  =   x
myFoldMax (x : xs)  =   foldr max x xs

myFoldLength ::  Num b => [a] -> b
myFoldLength x   =   foldr (+) 0 (map (\x->1) x)

mySumList   ::   Num a => [a] -> a
mySumList   =   foldl ( \x -> \y -> x + y ) 0

myProductList   ::   Num a => [a] -> a
myProductList   =   foldl ( \x -> \y -> x * y ) 1

myConcatList    ::   [[a]] -> [a]
myConcatList    =   foldr ( \x -> \y -> x ++ y ) []

myLengthList    ::   [a] -> Integer
myLengthList    =   foldr (\x -> \y -> y + 1) 0

myReverseList   ::   [a] -> [a]
myReverseList   =   foldl   ( \x -> \y -> y : x ) []

myMaxList   ::   Ord a => [a] -> a
myMaxList   =   \(x : xs) ->  foldl ( \y -> \z -> max y z ) x xs

myAnyList   ::   ( a -> Bool ) -> [a] -> Bool
myAnyList   =   \f -> (foldr (\x -> \y -> (f x) || y) False)

myAllList   ::   ( a -> Bool ) -> [a] -> Bool
myAllList   =   \f-> (foldr (\x -> \y -> (f x) && y) True)


forall    :: [a] -> (a -> a -> a) -> a -> [a]
forall    [] _ _      = []
forall    (x:xs) f a  = (f x a) : (forall xs f a)

cicla :: [a] -> [a]
cicla []        =   []
cicla (x : xs)  =   xs ++ [x]

cicli :: [a] -> [[a]]
cicli   []  =   [[]]
cicli   l   =   faiciclare l (length l - 1)
                where
                faiciclare l n  |   (n==0)       = [cicla l]
                                |   otherwise   =
                                    [cicla l] ++ faiciclare (cicla l) (n-1)
permutazioni :: [a] -> [[a]]
permutazioni []             =   [[]]
permutazioni (x : y : [])   =   cicli (x:y:[])
permutazioni (x : xs)       =   ((foldr (++) []) . (map cicli))
                                ( map (++ [x]) (permutazioni xs) )

permutations ::  [a] -> [[a]]
permutations []         =   [[]]
permutations (x : xs)   =   concat (map spalma (permutations xs))
    where 
    spalma []       =   [[x]]
    spalma (y : ys) =   (x : y : ys) : prependAll y (spalma ys)

    prependAll _ [] = []
    prependAll x (y : ys) = (x : y) : prependAll x ys

collapse :: String -> String
collapse [] =   []
collapse (x : y : [])   |   [x] == " " && [y] == " "  = " "
                        |   otherwise               =  (x:y :[])
collapse (x : y :xs)    |   [x]==" " && [y] ==" " =   collapse (x:xs)
                        |   otherwise       =   x:(collapse (y:xs))

collassa :: String -> String
collassa [] = []
collassa (x : [])       =   x : []
collassa (' ':' ': x)   =   collassa(' ' : x)
collassa (x : y : xs)   =   x :(collassa (y  : xs))

digitDec2Hex :: Integer -> String
digitDec2Hex  n |   n == 15  =   "F"
                |   n == 14  =   "E"
                |   n == 13  =   "D"
                |   n == 12  =   "C"
                |   n == 11  =   "B"
                |   n == 10  =   "A"
                |   n < 10  =   show n

digitHex2Dec ::  Char -> Integer
digitHex2Dec x  |   x == 'A' || x == 'a'  = 10
                |   x == 'B' || x == 'b'  = 11
                |   x == 'C' || x == 'c'  = 12
                |   x == 'D' || x == 'd'  = 13
                |   x == 'E' || x == 'e'  = 14
                |   x == 'F' || x == 'f'  = 15
                |   otherwise = read [x] :: Integer

hexToDec :: String -> Integer
hexToDec x =    (trasf x (length x - 1))
                where
                    trasf [] _          =   0
                    trasf (x : xs) n    =   digitHex2Dec x * (16 ^ n) + (trasf xs (n-1))

decToHex :: Integer -> String
decToHex x   =  aux x []
                where
                aux n l |   n < 16      = digitDec2Hex n ++ l
                        |   otherwise   = aux (n `div` 16)  (digitDec2Hex (n `mod` 16) ++ l)



