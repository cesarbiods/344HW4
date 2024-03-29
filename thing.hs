--import System.Random

irepeat :: (a -> a) -> a -> Int -> a
irepeat f x i = iterate f x !! i

dub :: String -> Char -> String
dub [] c = [] 
dub (x:xs) c  
    | x == c = x:x:(dub xs c)  
    | otherwise = x:(dub xs c)

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

--functions for use in monte carlo
under :: (Double -> Double) -> (Double, Double) -> Bool
under f (x, y) = y < a
  where
    a = f(x)

--ran :: (Double, Double) -> (Double, Double) -> Int-> (Double, Double)
--ran (mi, ma) (low, high) = (x, y)
  --do
    --g <- newStdGen 
    --h <- newStdGen
    --where
      --(x, t) = randomR (mi, ma) g
      --(y, u) randomR (low, high) h

--rans :: (Double, Double) -> (Double, Double) -> Int -> [(Double, Double)]

--monte :: (Double -> Double) -> (Double, Double) -> Double -> IO Double
--monte = do
  --let i = 5 :: Double
  --r <- random :: IO Double
  --let M = max(f(x) | x ∈ [a,b])