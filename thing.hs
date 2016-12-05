irepeat :: (a -> a) -> a -> a
irepeat f x = f(f(f(x)))

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