{-# LANGUAGE UnicodeSyntax #-}

import Data.Char (chr, ord)
import MyList
import Algebra
import Combinatori
import Espressioni

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
fibo ::  Integer -> Integer
f1 ::  Integer -> [Integer]
f2 ::  Integer -> [Integer]
f3 ::  Integer -> [Integer]


bottom = bottom

fibo n = aux 0 1 n
    where
        aux :: Integer -> Integer -> Integer -> Integer
        aux m _ 0 = m
        aux _ n 1 = n
        aux m n k = aux n (m + n) (k-1)
{-

    fi n = [1,2,...,n]
    f2 n = [n,n-1,n-2,....,1]
    f3 n = [p1,p2,....,pk] dove p sono divisori di n
-}


-- f2 complessita' costante in tempo perche' (:) costante
-- lineare anche in spazio perche' ricorsione non e' in coda e quindi
-- si caricano n frame sullo stack - non puo' essere trasformato in 
-- iterativo


f2 0 = []
f2 n = n:(f2 (n-1))

f1 = reverse . f2

--- f12 complessita' quadratica perche' (++) lineare
f12 ::  (Eq a, Num a) => a -> [a]
f12 0 = []
f12 n = f12 (n-1) ++ [n]

f3  n   =   n:aux (n `div` 2)
    where
    aux 1   =   [1]
    aux b   |  n `mod` b == 0    =   b:(aux (b-1))
            |  otherwise        =   aux (b-1)

{-
 - IMPLEMENTAZIONE F.NI SU LISTE IN MODO ITERATIVO
 -
 - l = []
 - while (n > 0) {
 -  l = n : l;
 -  n = n - 1;
 - }
 -
 -}
--  aux corrisponde al ciclo while
forward n = aux [] n
    where
    aux l n |   n == 0       =   l
            |   otherwise   =   aux (n : l) (n-1)

{-
 - l = []
 - m = 0
 - while (m < = n) {
 -  l = n : l
 -  n = n + 1
 - }
 -}

backward n = aux [] 1
    where
    aux l k |   k > n       =   l
            |   otherwise    =   aux (k : l) (k+1)




primo :: Integer -> Bool
primo n = n > 1 && noDivisori (div n 2)
    where
        noDivisori :: Integer -> Bool
        noDivisori 1 = True
        noDivisori k = n `mod` k /= 0 && noDivisori (k-1)

pow _ 0 =   1
pow a n |   n `mod` 2 == 0   =   b*b
        |   otherwise       =   a*b*b
    where
        b = a `pow` (n `div` 2)

powm _ 0    =   (1,0,0,1)
powm a n    |   n `mod` 2 == 0   = b `times` b
            |   otherwise       = a `times` (b `times` b)
    where
        b = a `powm` (n `div` 2)


fibo_log n =    snd4 ( powm (1,1,1,0) n)
    where       snd4 (_,x,_,_) = x

{-
    a b     k l    =      a*k + b*m     a*l + b*n
    c d  X  m n           c*k + d*m     c*l + d* n
-}

times ::  Num t => (t, t, t, t) -> (t, t, t, t) -> (t, t, t, t)
times (a,b,c,d) (k,l,m,n) = (a*k+b*m,a*l+b*n,c*k+d*m,c*l+d*n)

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
ivide m n = n `mod` m == 0

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









sinCos :: Bool -> Float -> Float
--sinCos b x  =   if b then sin x else cos x
--sinCos b x  =   (if b then sin else cos) x
sinCos b    =   if b then sin else cos



--  HASKELL ghci trasforma ogni funzione che prende un parametro
--  in lampa Notazione

successore :: Integer -> Integer
--successore x    =   x + 1
successore  =   \x -> x + 1


--  alcune funzioni implementate nel linguaggio

compose ::  (t1 -> t) -> (t2 -> t1) -> t2 -> t
compose f g = \x -> f (g x)

-- apply e' implementato nel linguaggio come ($)

apply1 :: (t1 -> t) -> t1 -> t
apply1 f x = f x

apply2 ::  t -> t
apply2 f = f

apply3 ::  a -> a
apply3 = id

curry ::  ((a, b) -> c) -> a -> b -> c
curry f     = \x -> \y -> f (x, y)

uncurry ::  (a -> b -> c) -> (a, b) -> c
uncurry f   = \(x,y) -> f x y

plusu ::  Num a => (a, a) -> a
plusu (x, y)    = x + y

plusc ::  Num a => a -> a -> a
plusc x y       = x + y



