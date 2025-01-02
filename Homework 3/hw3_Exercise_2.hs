module Golf where
    -- we need to find something that is greater than the element before and after, it, this can be easily done by taking three elments at a time,
    localMaxima :: [Integer]->[Integer]
    localMaxima [] = []
    localMaxima [x] = []
    localMaxima [x1,x2] = []
    localMaxima (x1:x2:x3:xs)
        | x1<x2 && x2>x3 = x2: localMaxima (x3:xs)
        | otherwise = localMaxima (x2:x3:xs)