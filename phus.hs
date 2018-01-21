import Register
import Data.List
import Data.Ord

hoursMinutesToMinutes :: (Integer, Integer) -> Integer
hoursMinutesToMinutes (h,m) = h * 60 + m

minutesToHoursMinutes :: Integer -> (Integer, Integer)
minutesToHoursMinutes m = (div m 60, mod m 60)

extractTime :: ([Char], Bool, (Integer, Integer)) -> ([Char], Integer, Integer)
extractTime (a,_,(b,c)) = (a, b, c)

sortPhus :: [([Char], Bool, (Integer, Integer))] -> [([Char], Bool, (Integer, Integer))]
sortPhus a = sortBy (comparing extractTime) a

modTimePhus :: ([Char], Bool, (Integer, Integer)) -> ([Char], Integer)
modTimePhus (a,True,(c,d)) = (a, (- hoursMinutesToMinutes (c,d)))
modTimePhus (a,False, (c,d)) = (a, hoursMinutesToMinutes (c,d))
 
groupedPhus :: [([Char], Bool, (Integer, Integer))] -> [[([Char], Integer)]]
groupedPhus a = groupBy (\a b -> fst a == fst b) $ map modTimePhus $ sortPhus a

getFirstName :: [([Char], Bool, (Integer, Integer))] -> [[Char]] 
getFirstName a = map fst $ map head $ groupedPhus a

getTimes :: [([Char], Bool, (Integer, Integer))] -> [Integer]
getTimes a = map (\ a -> sum $ map snd a ) $ groupedPhus a

timeList :: [([Char], Bool, (Integer, Integer))] -> [([Char], Integer)]
timeList a = zip (getFirstName a) (getTimes a)

maxTimeReg :: [([Char], Bool, (Integer, Integer))] -> [Char]
maxTimeReg a = fst $ maximumBy(comparing snd) (timeList a)

timeListHHMM :: [([Char], Bool, (Integer, Integer))] -> [([Char], (Integer, Integer))]
timeListHHMM a = zip (getFirstName a) (map minutesToHoursMinutes $ getTimes a)

phus :: [([Char], Bool, (Integer, Integer))] -> ([Char], [([Char], (Integer, Integer))])
phus a = ((maxTimeReg a), (timeListHHMM a))
