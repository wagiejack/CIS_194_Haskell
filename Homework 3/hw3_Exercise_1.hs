module Golf where
import Data.Maybe (catMaybes)

-- aim is to get the same list as first element of the new list, the second list will contain ith iteration elements only from the original list
-- "A B C D E F G"
-- "B D F"
-- "C F"
-- "D"
-- what if we had a function that iterates by index, so if i = 2 we iterate 2nd element from starting, i.e iterate in multiples of 2
-- we can make a data structure like N 3 [a] then when we are doing a recursion, we pick the 3rd elment from the list and send the rest of the list 
data NonNullableList n = L Int [n]
pickNthElement :: NonNullableList a -> [a]
pickNthElement (L n xs) = go 1 xs
    where
        go _ [] = []
        go count (x:xs)
            | mod count n==0 = x: go(count+1) xs
            | otherwise = go (count+1) xs

-- we want to go from 1 to a position where we get no result, we wont get it at length of the list, this is the first solution
skips::[a]->[[a]]
skips [] = []
skips xs = lambda 1 (length xs)
    where 
        lambda i len
            | i>len = []
            | otherwise = pickNthElement (L i xs): lambda (i+1) len 


-- this is the second method, where we generate a sequence as (<el-i>,<id-i>) where el-i is element at index i in the list and id-i is the index at that point, now, we wnat to filter one-by-one for each index
skipss :: [a]->[[a]]
skipss xs = [operateOnEvery x xs | x<-[1..length xs]]
        where 
            operateOnEvery x xs = [n|(n,i) <- zip xs [1..],mod i x==0]

-- This is the third approach, will now take the help of Maybe values and filter them out
skipsss :: [a]->[[a]]
skipsss xs = [processNth n | n<-[1..length xs] ]
             where processNth n = catMaybes $ zipWith
                                        (\x i -> if i `mod` n ==0 then Just x else Nothing) xs [1..]

