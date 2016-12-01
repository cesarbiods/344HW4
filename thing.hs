irepeat :: (a -> a) -> a -> a
irepeat f x = f(f(f(x)))

