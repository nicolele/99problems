-- problem 1
myLast :: [a] -> a
myLast [] = error "empty list!"
myLast [x] = x
myLast (x:xs) = myLast xs

-- problem 2
myButLast :: [a] -> a
myButLast [] = error "empty list!"
myButLast [x] = error "too few elements!"
myButLast [x,y] = x
myButLast (x:xs) = myButLast xs

-- problem 3
elementAt :: [a] -> Int -> a -- 1 indexed
elementAt (x:xs) i = (x:xs) !! (i-1)

elementAt' :: [a] -> Int -> a
elementAt' [] _ = error "out of bounds"
elementAt' (x:_) 1 = x
elementAt' (x:xs) k 
    | k < 1 = error "index out of bounds"
    | otherwise = elementAt' xs (k-1)

-- problem 4
numElements :: [a] -> Int
numElements [] = 0
numElements (x:xs) = 1 + numElements xs

numElements' :: [a] -> Int -- tail recursive
numElements' list = numElements_acc list 0 
    where 
        numElements_acc [] n = n
        numElements_acc (_:xs) n = numElements_acc xs (n + 1)

-- problem 5
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = xs ++ [x]

-- problem 6
palindrome :: (Eq a) => [a] -> Bool
palindrome [] = True
palindrome lst = lst == rev lst

-- problem 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- problem 8
compress :: (Eq a) => [a] -> [a] 
compress [] = []
compress (x:xs) = [x] ++ compress rest
    where (_, rest) = span (==x) xs

-- problem 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:first) : pack rest
    where (first, rest) = span (==x) xs

-- problem 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode lst = map (\x -> (length x, head x)) $ pack lst 

-- problem 11
data ListItem a = Single a | Multiple Int a deriving Show

encode' :: (Eq a) => [a] -> [ListItem a]
encode' = map encodeHelper . encode
    where 
        encodeHelper (1, x) = Single x
        encodeHelper (n, x) = Multiple n x

-- problem 12
decode :: [ListItem a] -> [a]
decode = concatMap $ decodeHelper
    where
        decodeHelper (Single x) = [x]
        decodeHelper (Multiple n x) = replicate n x

-- probelm 13

-- probelm 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = replicate 2 x ++ dupli xs

-- problem 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = replicate n x ++ repli xs n
