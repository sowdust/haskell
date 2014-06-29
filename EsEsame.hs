module EsEsame where

es1 :: [Integer] -> Bool
es1 l    =   aux l 0  where
            aux (x:[]) n    =   x == n
            aux (x:xs) n    =   aux xs (n+x)

es2 :: [Integer] -> Bool
es2 xs = head(reverse xs) == sum (tail(reverse xs))


es4 :: Ord a => [a] -> ([a],[a])
es4 (x:xs)  =   aux [] (x:xs) x
         where
            --aux prefisso rimanente ultimo
            aux m [] _    =   (m,[])
            aux m (x:xs) y  |   x < y       =   (m,x:xs)
                            |   otherwise   =   aux (m++[x]) xs x

es7 :: (a -> Bool) -> [a] -> [Int]
es7 p xs    =  map snd (filter (\x->(p$fst x)) (zip xs [0..(length xs)]))


-- es8:
type Set a = [a]
inter :: (Ord a) => Set a -> Set a -> Set a
inter _ []  =   []
inter [] _  =   []
inter (x:xs) (y:ys) |   x == y       =   x:inter xs ys
                    |   x < y       =   inter xs (y:ys)
                    |   otherwise   =   inter (x:xs) ys
diff :: (Ord a) => Set a -> Set a -> Set a
diff xs [] = xs 
diff [] ys = []
diff (x:xs) (y:ys)  |   x < y   =   x:(diff xs (y:ys))
                    |   x == y   =   diff xs ys
                    |   x > y   =   diff (x:xs) ys

es9 :: Eq a => [a] -> [a] -> Bool
es9 xs ys = foldr ((||) . (\x->(fst x == snd x))) False (zip xs ys)

es10 :: [Char] -> Bool
es10 (x:xs) = bilancia True 0 (x:xs)
        where
            bilancia b n []         =   b && n==0
            bilancia _ 0 (')':xs)   =   False
            bilancia b n ('(':xs)   =   b && (bilancia b (n+1) xs)
            bilancia b n (')':xs)   =   b && (bilancia b (n-1) xs)


es11 :: [a] -> [[a]]
es11 []     =   [[]]
es11 (x:xs) =   (map (\ys->(x:ys)) (es11 xs)) ++ (es11 xs)

es12 :: [[a]] -> [[a]]
es12 l   =   filter ( \x -> (length x >= maxl) ) l
                    where maxl = maximum (map (\x-> (length x)) l)


es13 :: Int -> Bool
es13 n = sum (divisori n 1) == n
        where
            divisori :: Int -> Int -> [Int]
            divisori n x    | x >  div n 2 = []
                            | n == 1 = 1:(divisori n 2)
                            | otherwise =  if n `mod` x == 0 then x:(divisori n (x+1)) else (divisori n (x+1))

perfetti = filter es13 [1..]
--  DA FARE:
--  es13bis :: [Int]  Ritorna la lista di tutti i numeri perfetti
--  farlo in modo che questa lista sia in effetti utilizzabile con take!

fp n = aux n primes
    where
        aux n (x:xs)| mod n x == 0 = x:(aux (div n x)  (x:xs))
                    |   otherwise = aux n xs

from n = n:from (n+1)
sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x > 0]
primes = sieve (from 2)
