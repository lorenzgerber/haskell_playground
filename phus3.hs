import Register
import Data.List
import Data.Ord

phus :: [([Char], Bool, (Integer, Integer))] -> ([Char], [([Char], (Integer, Integer))])
phus a = 
    let summedList = getSummedList a
        longest =  fst $ head $ reverse . sortBy (comparing getTime) $ summedList
    in (longest, summedList)

type PhusEntry = (String, Bool, Time)
type RegTime = (String, Time)
type Time = (Integer, Integer)

getReg :: PhusEntry -> String
getReg (a,_,_) = a

getTime :: RegTime -> Time
getTime (_,b) = b

timeAdd :: Time -> Time -> Time 
(a, b) `timeAdd` (c, d) = (a + c + div (b+d) 60, mod (b + d) 60)

timeSub :: Time -> Time -> Time
(a, b) `timeSub` (c, d) = (a - c + div (b-d) 60, mod (b - d) 60) 

first [] = []
first (x:xs) = x:second xs

second [] = []
second (x:xs) = first xs

zipSum :: [PhusEntry] -> [PhusEntry]
zipSum a = zipWith subTime (second a) (first a)

subTime :: PhusEntry -> PhusEntry -> PhusEntry
subTime (a,b,c) (d,e,f) = (d, b, c `timeSub` f)

sumTime :: PhusEntry -> PhusEntry -> PhusEntry
sumTime (a,b,c) (d,e,f) = (d, b, c `timeAdd` f) 

getSummedList :: [PhusEntry] -> [RegTime]
getSummedList a = map (\(a,b,c)->(a,c)) $
                  map (foldl sumTime ([],True, (0,0))) $
                  map zipSum $
                  groupBy (\a b -> getReg a == getReg b) $
                  sortBy (comparing getReg) a


