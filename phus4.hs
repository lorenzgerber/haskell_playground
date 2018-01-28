import Register
import Data.List
import Data.Ord

data Time' = Time' Integer Integer deriving (Show)
data PhusEvent = PhusEvent String Bool Time' deriving (Show)


type PhusList = [PhusEntry]
type PhusEntry = (String, Bool, Time)

getReg :: PhusEntry -> String
getReg (a,_,_) = a

type RegTime = (String, Time)

getTime :: RegTime -> Time
getTime (_,b) = b

type Time = (Integer, Integer)
timeAdd :: Time -> Time -> Time 
(a, b) `timeAdd` (c, d) = (a + c + div (b+d) 60, mod (b + d) 60)

timeSub :: Time -> Time -> Time
(a, b) `timeSub` (c, d) = (a - c + div (b-d) 60, mod (b - d) 60) 

first [] = []
first (x:xs) = x:second xs

second [] = []
second (x:xs) = first xs

testSum :: [PhusEntry] -> [PhusEntry]
testSum a = zipWith subTime (second a) (first a)

subTime :: PhusEntry -> PhusEntry -> PhusEntry
subTime (a,b,c) (d,e,f) = (d, b, c `timeSub` f)

sumTime :: PhusEntry -> PhusEntry -> PhusEntry
sumTime (a,b,c) (d,e,f) = (d, b, c `timeAdd` f) 

getSummedList :: [PhusEntry] -> [RegTime]
getSummedList a = map (\(a,b,c)->(a,c)) $
                  map (foldl sumTime ([],True, (0,0))) $
                  map testSum $
                  groupBy (\a b -> getReg a == getReg b) $
                  sortBy (comparing getReg) a

phus :: [PhusEntry] -> (String, [RegTime])
phus a = 
    let summedList = getSummedList a
        longest =  fst $ head $ reverse . sortBy (comparing getTime) $ summedList
    in (longest, summedList)

