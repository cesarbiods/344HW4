irepeat :: (a -> a) -> a -> a
irepeat f x = f(f(f(x)))

dub :: String -> String
dub [] = [] 
dub (x:xs)   
    | x == 'a' = x:x:(dub xs)  
    | otherwise = x:(dub xs)