import Data.Char (chr, ord)

-- il costrutto let _ e' un espressione
-- il costrutto where puo' essere usato solo in definizioni di funzioni

bottom = bottom

--	coppie

cross :: (a -> a1, b -> b1) -> (a, b) -> (a1, b1) 
cross(f, g)(x, y) = (f x, g y)
left (x , y ) = x
right (x , y ) = y
pair (f , g ) x = ( f x , g x )

succ :: (Num a) => [a] -> [a]
succ xs = map (+ 1) xs

chop :: [a] -> [a]
chop = reverse . tail . reverse

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort left ++ [x] ++ qsort right
	where
		left = filter (< x) xs
		right = filter (>= x) xs

hyp :: Float -> Float -> Float
hyp x y = sqrt (x*x + y*y)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

lower :: Char -> Char
lower c | c >= 'A' && c <= 'Z' = chr (ord c - ord 'A' + ord 'a')
		| otherwise = c
	
upper :: Char -> Char
upper c | c >= 'a' && c <= 'z' = chr (ord c - ord 'a' + ord 'A')
		| otherwise = c

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)

--fact n =	if n == 0 then 1
--			else n * fact (n-1)

--fact n	| n==0 = 1
--		| otherwise = n * fact (n-1)

nnulltail 0 = 0
nnulltail n | mod n 10 == 0 = nnulltail (div n 10)
			| div n 10 == 0 = n
			| otherwise =   (n - ((div n 10)*10))

lastd1 n	| n == 0 = 0
			| mod n 10 /= 0 = mod n 10
			| otherwise = lastd1 (div n 10)

lastd 0 =	0
lastd n =	let r = mod n 10 in -- definisco r e poi lo uso in tutta l'area "in"
			if r /= 0 then r
			else lastd (div n 10)

last_d n	| n == 0 = 0
			| r /= 0 = r
			| otherwise = last_d (div n 10)
		where
			r = mod n 10

divide :: Integer -> Integer -> Bool
divide 0 _ = False
divide m n = n `mod` m == 0

mcd :: Integer -> Integer -> Integer
mcd 0 n =	n
mcd m n |	m < n =	mcd n m
		| 	otherwise = mcd (m - n) n


