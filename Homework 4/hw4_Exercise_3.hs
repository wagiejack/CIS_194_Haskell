xor::[Bool]->Bool
xor = foldr (\x acc->if x then not acc else acc) False

map' :: (a->b) ->[a]->[b]
map' f = foldr (\x acc-> f x:acc ) []