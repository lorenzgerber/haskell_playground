module Huffman
( statistics,
  maketree,
  encode,
  decode
) where


import Data.List
import Data.Char
import Data.Ord

-- |Defintion of datatypes Wtree and Htree according specifications. 
-- Wtree (weight tree) nodes are used to contruct the huffman coding. 
-- Htree (huffman tree) nodes represent a finished huffman tree.  
data Wtree = L Int Char | B Int Wtree Wtree deriving (Show, Ord)
data Htree = Leaf Char | Branch Htree Htree deriving (Show)

-- |Wtree is made an instance of Eq by making the weight values
-- comparable. This is a pre-condition for Wtree to be an instance
-- of Ord. 
instance Eq Wtree where
    x == y = getWeight x == getWeight y

-- |The 'statistics' function calculates the frequencies of each
-- character in a string. A helper function (timesFound) is used
-- to count the occurences of each character. 
statistics :: String -> [(Integer, Char)]
statistics a = zip (map (timesFound (map fromEnum a)) (nub (map fromEnum a))) $ nub a

-- |Helper function 'timesFound' to determine the number of occurences
-- of a specific value in a list. 
timesFound :: Eq a => [a] -> a -> Integer
timesFound xs a = toInteger $ (length . filter (==a)) xs

-- |The main 'makeTree' function takes a frequency list produced by the
-- statistics function and constructs an Htree from it. The whole process
-- is split up into three functions whereoff the present is the first one.
-- 'maketree' produces a sorted list of Wtree leafs from the provided
-- frequency distribution and calls maketree' on it.  
maketree :: [(Integer, Char)] -> Htree
maketree a = maketree' $ sort $ map (\(a, b) -> (L (fromIntegral a) b)) a  

-- |The "maketree'" helper function is a recursive function that connects
-- the Wtree elements to a connected tree using the 'addNode' helper function.
-- If only one Wtree element is left in the provided list, the next level
-- of maketree helper function, "maketree''" is called. 
maketree' :: [Wtree] -> Htree
maketree' [x] = maketree'' x
maketree' (x:xs) = maketree' (sort ((addNode x (head xs)):(tail xs)))

-- |The recursive "maketree''" helper function converts a Wtree into a Htree
-- by removing all weights. 
maketree'' :: Wtree -> Htree
maketree'' (B a b c) = (Branch (maketree'' b) (maketree'' c))
maketree'' (L a b) = (Leaf b)

-- |Helper function to access the weight of a Wtree.
getWeight :: Wtree -> Int
getWeight (L a _) = a
getWeight (B a _ _) = a

-- |The 'addNode' helper function takes two Wtree instances and
-- adds them to a single Wtree. The resulting Wtree weight
-- is the sum of the two combined Wtree weight values. 
addNode :: Wtree -> Wtree -> Wtree
addNode (L a b) (L c d) = B (a + c) (L a b) (L c d )
addNode (B a b c) (L d e) = B (a + d) (B a b c) (L d e)
addNode (L a b) (B c d e) = B (a + c) (L a b) (B c d e)
addNode (B a b c) (B e f g) = B (a + e) (B a b c) (B e f g)

-- |The 'encode' function takes a string and uses the statistics and
-- maketree function to construct the corresponding Huffman coding 
-- tree. Then the getBinary helper function is applied to the provided
-- string to obtain a pair with a Htree and a list containing the 
-- actual binary coded huffman code of the initial string. 
encode :: String -> (Htree, [Integer])
encode a = (huffTree, foldr (\x acc -> getBinary huffTree x ++ acc) [] a)
    where huffTree = maketree  $ statistics a

-- |The 'getBinary' helper functions are used to obtain the binary huffman
-- code of the provided character. The present function handles the special
-- case when the Htree consists of only one Leaf. Else the "getBinary'" helper
-- function is called with an additional parameter, an empty list as 
-- accumulator. 
getBinary :: Htree -> Char -> [Integer]
getBinary (Leaf a) b = [0]
getBinary a b = getBinary' b [] a 

-- |The recursive "getBinary'" helper function does the main part of encoding 
-- characters to the binary huffman code. Besides a char and a Htree, it also 
-- takes a list of Integers as accumulator.   
getBinary' :: Char -> [Integer] -> Htree -> [Integer]
getBinary' c x (Branch l r) = getBinary' c (x++[0]) l ++ getBinary' c (x++[1]) r
getBinary' c x (Leaf a)
    | c == a = x
    | otherwise = []

-- |The 'decode' function is used as intermediate step to call the actual
-- decoding function "decode'".
decode :: Htree -> [Integer] -> String
decode a b = decode' a a [] b

-- |The recursive "decode'" helper function takes among others two Htree's 
-- as parameter. One is used to store in the recursive calls the current
-- position in tree traversal while the other stores the entry position to 
-- the whole Htree to be used after a Character has been successfully 
-- retrieved. Two patterns (first and last) are used for the special case
-- when the Htree consist of only one Leaf.
decode' :: Htree -> Htree -> String -> [Integer] -> String
decode' (Leaf a) (Leaf b) c [] = reverse c
decode' a (Leaf b) c [] = reverse (b:c)
decode' (Leaf a) (Leaf b) c (x:xs) = decode' (Leaf a) (Leaf b) (b:c) xs
decode' a (Branch l r) c (x:xs)
    | x == 0 = decode' a l c xs 
    | x == 1 = decode' a r c xs
decode' a (Leaf b) c d = decode' a a (b:c) d
