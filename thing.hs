irepeat :: (a -> a) -> a -> Int -> a
irepeat f x i = iterate f x !! i

dub :: String -> String
dub [] = [] 
dub (x:xs)   
    | x == 'a' = x:x:(dub xs)  
    | otherwise = x:(dub xs)

replace :: [a] -> (Int, Int) -> [a] -> [a]
replace new (lower, upper) list  = x ++ new ++ w
  where
    (x, y) = splitAt lower list
    (z, w) = splitAt ((upper - lower)+ 1) y

--found this improvised algorithm at http://stackoverflow.com/questions/21276844/prime-factors-in-haskell
factor :: Int -> [Int]
factor 1 = []
factor n
  | factors == []  = [n]
  | otherwise = factors ++ factor (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]