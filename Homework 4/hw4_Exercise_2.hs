data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

-- Get height of a tree node
height :: Tree a -> Integer 
height Leaf = -1  -- Using -1 helps simplify height calculations
height (Node h _ _ _) = h

-- Single function to handle insertion while maintaining balance
insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node _ left val right) 
    | height left <= height right = 
        let newLeft = insert x left
        in Node (1 + max (height newLeft) (height right)) newLeft val right
    | otherwise = 
        let newRight = insert x right
        in Node (1 + max (height left) (height newRight)) left val newRight

-- Build tree using foldr
foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf