{-# LANGUAGE UnicodeSyntax #-}
import Data.Char (chr, ord)

-- il costrutto let _ e' un espressione
-- il costrutto where puo' essere usato solo in definizioni di funzioni

divide	:: Integer -> Integer -> Bool
cross	:: (a -> a1, b -> b1) -> (a, b) -> (a1, b1) 
chop	:: [a] -> [a]
qsort	:: (Ord a) => [a] -> [a]
hyp	:: Float -> Float -> Float
fib	:: Int -> Int
lower	:: Char -> Char
upper	:: Char -> Char
fact	:: Integer -> Integer
mcd	:: Integer -> Integer -> Integer
add	:: Nat -> Nat -> Nat 
mult	:: Nat -> Nat -> Nat
eleva	:: Nat -> Nat -> Nat
foldn 	:: (Eq a, Num a) => (t -> t) -> t -> a -> t
last_d ::  Integral a => a -> a
lastd ::  Integral a => a -> a
lastd1 ::  Integral a => a -> a
nnulltail ::  Integral a => a -> a
factc ::  (Eq a, Num a) => a -> (a -> t) -> t



bottom = bottom



cross(f, g)(x, y) = (f x, g y)
left (x , y ) = x
right (x , y ) = y
pair (f , g ) x = ( f x , g x )

--succ :: (Num a) => [a] -> [a]
--succ xs = map (+ 1) xs

chop = reverse . tail . reverse

qsort [] = []
qsort (x : xs) = qsort left ++ [x] ++ qsort right
	where
		left = filter (< x) xs
		right = filter (>= x) xs

hyp x y = sqrt (x*x + y*y)

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

lower c | c >= 'A' && c <= 'Z' = chr (ord c - ord 'A' + ord 'a')
	| otherwise = c
	
upper c | c >= 'a' && c <= 'z' = chr (ord c - ord 'a' + ord 'A')
	| otherwise = c

fact 0 = 1
fact n = n * fact (n-1)

factc 0 c = c 1
factc n c = factc n ( \x -> c( (n+1) * x) )

--fact n =	if n == 0 then 1
--			else n * fact (n-1)

--fact n	| n==0 = 1
--		| otherwise = n * fact (n-1)

nnulltail 0 = 0
nnulltail n	| mod n 10 == 0 = nnulltail (div n 10)
		| div n 10 == 0 = n
		| otherwise =   (n - ((div n 10)*10))

lastd1 n	| n == 0 = 0
		| mod n 10 /= 0 = mod n 10
		| otherwise = lastd1 (div n 10)

lastd 0 = 0
lastd n	= let r = mod n 10 in -- definisco r e poi lo uso in tutta l'area "in"
		if r /= 0 then r
		else lastd (div n 10)

last_d n	| n == 0 = 0
		| r /= 0 = r
		| otherwise = last_d (div n 10)
	where
		r = mod n 10

divide 0 _ = False
divide m n = n `mod` m == 0

mcd 0 n =	n
mcd m n |	m < n =	mcd n m
	| 	otherwise = mcd (m - n) n



{-
	*	forall x `elem` N.sx = 0
	*	forall xy `elem` N.x = y -> sx = s(y )
	*	forall G ⊆ N.(0 `elem` G && forall x `elem` N.x `elem` G -> s(x) `elem` G ) -> G = N
-}

data Nat	=	Zero
	|	Succ Nat

instance Eq Nat where
	Zero == Zero		=	True
	Zero ==	Succ n		=	False
	(Succ n) == Zero		=	False
	(Succ n) == (Succ m)	=	(n == m)

instance Ord Nat where
	Zero < Zero 		= False
	Zero < (Succ n) 	= True
	(Succ n) < Zero 	= False
	(Succ n) < (Succ m)	= (n < m)

instance Show Nat where
	show Zero = "Zero"
	show (Succ Zero)	= "Succ(" ++ show Zero ++ ")"
	show (Succ(Succ n))	= "Succ(" ++ show (Succ n) ++ ")"

add a Zero 	= a
add a (Succ n)	= (Succ . add a) n

mult a Zero	= Zero
mult a (Succ n)	= (add . mult a) n a

eleva a Zero	= Succ Zero
eleva a (Succ n)	= (mult . eleva a) n a

foldn f a 0 	= a
foldn f a n 	= f (foldn f a (n-1))




