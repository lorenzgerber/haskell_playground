import Register
import Data.List
import Data.Ord

-- |The 'phus' function exhibits the signature as demanded
-- in the assignment. It uses the help function getSummedList
-- to do the main processing. A let construction is used to
-- compose the return data with the Registration for the 
-- longest total park time.
phus :: [([Char], Bool, (Integer, Integer))] -> ([Char], [([Char], (Integer, Integer))])
phus a = 
    let summedList = getSummedList a
        longest =  fst $ head $ reverse . sortBy (comparing getTime) $ summedList
    in (longest, summedList)

-- |A range of type synonyms were defined for a more
-- verbose code style
type PhusEntry = (String, Bool, Time)
type RegTime = (String, Time)
type Time = (Integer, Integer)

-- |The 'getReg' function extracts the registration
-- from a PhusEntry type
getReg :: PhusEntry -> String
getReg (a,_,_) = a

-- |The 'getTime' function extracts the Time from
-- a RegTime type
getTime :: RegTime -> Time
getTime (_,b) = b

-- |The 'timeAdd' function adds two time value.
timeAdd :: Time -> Time -> Time 
(a, b) `timeAdd` (c, d) = (a + c + div (b+d) 60, mod (b + d) 60)

-- |The 'timeSub' function subtracts the second from the first time
-- value.
timeSub :: Time -> Time -> Time
(a, b) `timeSub` (c, d) = (a - c + div (b-d) 60, mod (b - d) 60) 

-- |The 'sumTime' function sums the time from two PhusEntry values.
-- Based on the Bool value, either subtraction or addition is used.
sumTime :: PhusEntry -> PhusEntry -> PhusEntry
sumTime (a,b,c) (d,e,f)
    | e == True = (d, b, c `timeSub` f)
    | e == False = (d, b, c `timeAdd` f) 

-- |The 'getSummedList' function does the main processing to calculate
-- sum and extract parking times for each registration number. First
-- the in data is sorted and grouped by registration which results in a
-- list of list. Then a fold function for summing all time values is mapped.
-- Finally, the bool value is discarded.
getSummedList :: [PhusEntry] -> [RegTime]
getSummedList a = map (\(a,b,c)->(a,c)) $
                  map (foldl sumTime ([],True, (0,0))) $
                  groupBy (\a b -> getReg a == getReg b) $
                  sortBy (comparing getReg) a


