mix x []    =   x
mix [] y    =   y
mix (x:xs) ys   =   x:(mix ys xs)

sottoliste :: [a] -> [[a]]                                                              
sottoliste []     =   [[]]                                                            
sottoliste (x:xs) =   (map (\ys->(x:ys)) (sottoliste xs)) ++ (sottoliste xs) 

sublMaxN :: Int -> [a] -> [[a]]
sublMaxN 0 _ = [[]]
sublMaxN n [] = [[]]
sublMaxN n (x:xs) =   (subl n xs) ++ ( map (\xs -> x:xs) (subl (n-1) xs) )

subl1 n xs = filter (\xs -> length xs  == n) (sottoliste xs)
subl n xs = [ l | l <- sublMaxN n xs, length l == n]
