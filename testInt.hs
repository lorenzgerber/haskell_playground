module Huffman
( statistics,
  encode,
  decode
) where


import Data.List
import Data.Char
import Data.Ord

data Htree = Leaf Char | Branch Htree Htree deriving (Show)
data Wtree = L Int Char | B Int Wtree Wtree deriving (Show, Ord)

instance Eq Wtree where
    x == y = getWeight x == getWeight y

statistics :: String -> [(Integer, Char)]
statistics a = zip (map (timesFound (map fromEnum a)) (nub (map fromEnum a))) [ toEnum x :: Char | x <- (nub (map fromEnum a))]

timesFound :: Eq a => [a] -> a -> Integer
timesFound xs a = toInteger $ (length . filter (==a)) xs

maketree :: [(Integer, Char)] -> Htree
maketree a = maketree' $ sort $ map (\(a, b) -> (L (fromIntegral a) b)) a  

maketree' :: [Wtree] -> Htree
maketree' [x] = maketree'' x
maketree' (x:xs) = maketree' (sortBy (comparing getWeight) ((addNode x (head xs)):(tail xs)))

maketree'' :: Wtree -> Htree
maketree'' (B a b c) = (Branch (maketree'' b) (maketree'' c))
maketree'' (L a b) = (Leaf b)

getWeight :: Wtree -> Int
getWeight (L a _) = a
getWeight (B a _ _) = a

addNode :: Wtree -> Wtree -> Wtree
addNode (L a b) (L c d) = B (a + c) (L a b) (L c d )
addNode (B a b c) (L d e) = B (a + d) (B a b c) (L d e)
addNode (L a b) (B c d e) = B (a + c) (L a b) (B c d e)
addNode (B a b c) (B e f g) = B (a + e) (B a b c) (B e f g)

encode :: String -> (Htree, [Integer])
encode a = (huffTree, foldr (\x acc -> getBinary huffTree x ++ acc) [] a)
    where huffTree = maketree  $ statistics a

getBinary :: Htree -> Char -> [Integer]
getBinary (Leaf a) b = [0]
getBinary a b = getBinary' b [] a 

getBinary' :: Char -> [Integer] -> Htree -> [Integer]
getBinary' c x (Branch l r) = getBinary' c (x++[0]) l ++ getBinary' c (x++[1]) r
getBinary' c x (Leaf a)
    | c == a = x
    | otherwise = []

decode :: Htree -> [Integer] -> String
decode a b = decode' a a [] b

decode' :: Htree -> Htree -> String -> [Integer] -> String
decode' (Leaf a) (Leaf b) c [] = reverse c
decode' a (Leaf b) c [] = reverse (b:c)
decode' (Leaf a) (Leaf b) c (x:xs) = decode' (Leaf a) (Leaf b) (b:c) xs
decode' a (Branch l r) c (x:xs)
    | x == 0 = decode' a l c xs 
    | x == 1 = decode' a r c xs
decode' a (Leaf b) c d = decode' a a (b:c) d
