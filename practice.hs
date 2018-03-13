-- |Tail recursive list reverser
reverser:: (Ord a) => [a] -> [a]
reverser xs
    | xs == [] = []
    | otherwise = reverser' (tail xs) [(head xs)]

reverser' :: (Ord a) => [a] -> [a] -> [a]
reverser' xs ys
    | length xs == 1 = xs++ys
    | otherwise = reverser' (tail xs) ((head xs):ys)

-- |Recursive list reverser
rev:: (Ord a) => [a] -> [a]
rev xs
    | xs == [] = []
    | length xs == 1 = xs
    | otherwise = (last xs):(rev (init xs)) 

-- |Recursive List Multiplicator
multi::(Num a) => [a] -> a
multi xs
    | length xs == 1 = head xs
    | otherwise = (head xs) * (multi (tail xs))

-- |Tail Recursive List Multiplicator
multiTr::(Num a) => [a] -> a
multiTr xs
    | length xs == 1 = head xs
    | otherwise = multiTr' (tail xs) (head xs)

multiTr'::(Num a) => [a] -> a -> a
multiTr' xs s
    | length xs == 1 = head xs * s
    | otherwise = multiTr' (tail xs) (head xs * s)

-- |Recursive Fibonacci X
fibo:: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

-- |Tail Recursive Fibonacci
fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib' 2 1 2
  where
      fib' i y z | i == x = y
      fib' i y z          = fib' (i+1) z (z+y)


-- |Quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerOrEqual = [a | a <- xs, a <= x]
        larger =  [a | a <- xs, a > x]
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger

-- |Map by foldr
mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f xs = foldr (\x acc -> f x : acc)[] xs

-- |Map by foldl
mapFoldl :: (a -> b) -> [a] -> [b]
mapFoldl f xs = foldl (\acc x -> acc ++ [f x]) [] xs



