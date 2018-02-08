import Data.List
import Data.Char
import Data.Ord

testText = "This is a lousy text and i want to have a really nice and new one very very very soon please!"

testText2 = "aaaaaaaaaaaa"

numbers = map fromEnum testText

numTimesFound xs x = (length . filter (==x)) xs

counts = map (numTimesFound numbers) (nub numbers)
backChar = [ toEnum x :: Char | x <- (nub numbers)]
zipped = zip counts backChar

data Htree = Leaf Char | Branch Htree Htree deriving (Show)

data Wtree = L Int Char | B Int Wtree Wtree deriving (Show)

pairToTree :: (Int, Char) -> Wtree
pairToTree (a,b) = L a b

listOfWtree = map pairToTree zipped

sortedListOfWtree = sortBy (comparing getWeight) listOfWtree

getWeight :: Wtree -> Int
getWeight (L a _) = a
getWeight (B a _ _) = a

addTrees :: Wtree -> Wtree -> Wtree
addTrees (L a b) (L c d) = B (a + c) (L a b) (L c d )
addTrees (B a b c) (L d e) = B (a + d) (B a b c) (L d e)
addTrees (L a b) (B c d e) = B (a + c) (L a b) (B c d e)
addTrees (B a b c) (B e f g) = B (a + e) (B a b c) (B e f g)

processList :: [Wtree] -> Wtree
processList [x] = x
processList (x:xs) = processList (sortBy (comparing getWeight) ((addTrees x (head xs)):(tail xs)))

removeWeights :: Wtree -> Htree
removeWeights (B a b c) = (Branch (removeWeights b) (removeWeights c))
removeWeights (L a b) = (Leaf b)

getBinary :: Char -> Htree -> [Int]
getBinary a (Leaf b) = [0]
getBinary a b = getBinary' a [] b 

getBinary' :: Char -> [Int] -> Htree -> [Int]
getBinary' c x (Branch l r) = getBinary' c (x++[0]) l ++ getBinary' c (x++[1]) r
getBinary' c x (Leaf a)
    | c == a = x
    | otherwise = []
 
