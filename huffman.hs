import Data.List
import Data.Char

testText = "This is a lousy text"

numbers = map fromEnum testText

numTimesFound xs x = (length . filter (==x)) xs

counts = map (numTimesFound numbers) (nub numbers)
backChar = [ toEnum x :: Char | x <- (nub numbers)]
zipped = zip counts backChar

data Htree = Leaf Char | Branch Htree Htree

data Wtree = L Int Char | B Int Wtree Wtree

