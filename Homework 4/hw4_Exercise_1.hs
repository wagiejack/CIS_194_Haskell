--1st part
fun1' :: [Integer]->Integer
-- in the function part, first we check if it is even, then we do (x - 2) * fun1` xs else we have to do fun1' xs
-- fun1 (x:xs) = foldr' function base-value list_to_operate_on
-- the base value will be 1, we can set up the recursive call to subtract 2 from even values, we can filter out to get the even values since they are the only ones we will be performing operation on
-- the filtered list will have to be operated from right to left where we subtract it by 2 and multiply by 1 and then the result is applied to the element on the left  
fun1' xs = foldr (\a b->(a-2)*b) 1 (filter even xs)


--2nd part
-- the base case is that for integer 1, it will return 0,for any other integer, if it is even, we return integer+rec(div n 2) otherwise rec(3*n+1)
-- the function wants to add up only even numbers, we recurse upon the solution and generate the values for both cases if they are even or odd, then we take values upto the base case, then we filter and sum the even elements
fun2' :: Integer->Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate (\x->if even x then div x 2 else 3*x+1)