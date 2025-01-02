getQuotient :: Integer->Integer --gets all the number except last digit
getQuotient n = div n 10


getRemainder :: Integer->Integer --gets last number
getRemainder n = mod n 10

toDigitsRev :: Integer -> [Integer] --gets reversed digits as list
toDigitsRev n
    |n<=0  = []
    |otherwise = getRemainder n : toDigitsRev (getQuotient n)

toDigits :: Integer->[Integer] --gets digits as list
toDigits n
    |n<=0 = []
    |otherwise = toDigits (getQuotient n) ++ [getRemainder n]

--Exercise 1 Ends--

doubleEveryOther :: [Integer]->[Integer]
doubleEveryOther n = reverse $ zipWith (*) (reverse n) (cycle [1,2])

--Exercise 2 Ends--

sumDigits :: [Integer]->Integer
sumDigits [] = 0
sumDigits [x]
    |mod x 10==x = x
    |otherwise = sumDigits $ (toDigits x)
sumDigits (x:xs)
    |mod x 10==x = x + sumDigits xs
    |otherwise = sumDigits (toDigits x) + sumDigits xs

--Exercise 3 ends--

validate :: Integer->Bool
validate n
    | mod ( sumDigits $ doubleEveryOther $ toDigits n) 10 == 0 = True
    | otherwise = False

--Exercise 4 ends--

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n src dest temp = hanoi (n-1) src temp dest ++ 
                       [(src, dest)] ++ 
                       hanoi (n-1) temp dest src