ones = 1:ones
nats = 0 : (zipWith (+) ones nats)
fattoriales = 1: (zipWith (*) [1..] fattoriales)
from n = n:from(n+1)

-- num pitagorici
pyths n = [(a,b,c) | a<-[1..n], b<-[1..n], c<-[1..n], (a*a + b * b) == (c*c)]
divisori n = [ x | x <- [1..n`div`2], mod n x == 0]
perfetti = [ y | y <- [1..], y == sum (divisori y)]
sololunghe xss = [ xs | xs <- xss, (length xs) == maximum (map length xss)]
find k t = [ v | (c,v) <- t, c == k]
findr k []      = []
findr k ((c,v):xs)  | c == k     =   v:(findr k xs)
                    | otherwise =   findr k xs
findf k  =  (map snd) .filter ((==k).fst)

scalarp xs ys = sum [ x*y | (x,y) <- zip xs ys]
scalarr [] []   = 0 
scalarr (x:xs) (y:ys) = x * y + (scalarr xs ys)

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y     = x:(merge xs (y:ys))
                    | otherwise = y:(merge (x:xs) ys)

--halve zs = [ xs | xs <- take n zs, ys <- drop n zs, n = ((length zs) `div` 2)]
halve zs = [ take n zs, drop n zs] where n = ((length zs) `div` 2)
msort [] = []
msort [x] = [x]
msort l = merge (msort a) (msort b) where [a,b] = halve l 


filterf p = foldr (f p) [] where
        f p =  \x -> if p x then (:) x else \xs -> xs 

mapf f = foldr ((:) . (\x -> f x) ) []

sumsqreven xs = sum [ y*y | y <- xs, even y ]

unfold p f g b  |   p b         =   []
                |   otherwise   =   (f b) : (unfold p f g (g b))

dec2bin = reverse . (unfold (== 0) (`mod` 2) (`div` 2))

mapunfold f = unfold null (f . head) tail

pos p xs = [ k | (k,v) <- (zip [0..] xs), p v]

type Set a = [a]
intersection xs ys = [ x | x <- xs, y <- ys, x == y]

concatc xss = [ x | xs <- xss, x <- xs]

crivello (p:xs) = p: crivello [ x | x <- xs, x `mod` p /= 0 ]
sieve (p:xs) = p: sieve ( filter (\x -> x `mod` p /= 0) xs)


fib = 0 : 1 : [ p + q | (p,q) <- zip fib (tail fib)] 

reverse'' [] ys = ys
reverse'' (x:xs) ys = reverse'' xs (x:ys)

reverse' l = reverse'' l []
reversef = foldl (\xs x -> x : xs)

factsWith = 1 : zipWith (*) (from 1) factsWith


 -- se sei simpatico è solo perchè sei sbronzo!! e basta@@ altrimenti
 -- seit antipatico!!!!https://twitter.com/
