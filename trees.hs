data Tree a = Empty | Node a (Tree a) (Tree a) 
              deriving (Show, Eq)

leaf x = Node x Empty Empty

flatten :: Tree a -> [a]
flatten t = flatten' t []
    where
        flatten' Empty xs = xs
        flatten' (Node x l r) xs = flatten' l (x:(flatten' r xs))


-- problem 56
mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror Empty _ = False
mirror _ Empty = False
mirror (Node _ l1 r1) (Node _ l2 r2) = mirror l1 r2 && mirror r1 l2

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Node _ l r) = mirror l r

-- problem 57 (first one constructs from a sorted list)
constructBal :: (Ord a) => [a] -> Tree a
constructBal [] = Empty
constructBal lst = Node v l r
    where
        v = lst !! midPoint lst
        l = constructBal $ take (midPoint lst) lst
        r = constructBal $ drop ((midPoint lst)+1) lst

midPoint :: [a] -> Int
midPoint [] = 0
midPoint [x] = 0
midPoint lst = quot (length lst) 2

-- only really works on unsorted lists
add :: (Ord a) => a -> Tree a -> Tree a
add x Empty = Node x Empty Empty
add x t@(Node v l r)
    | x < v  = Node v (add x l) r
    | x > v  = Node v l (add x r)
    | x == v = t

construct lst = foldl (flip add) Empty lst
