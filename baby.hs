import Register
import Data.List
import Data.Ord

hoursMinutesToMinutes :: (Integer, Integer) -> Integer
hoursMinutesToMinutes (h,m) = h * 60 + m

minutesToHoursMinutes :: Integer -> (Integer, Integer)
minutesToHoursMinutes m = (div m 60, mod m 60)

extractIncoming :: ([Char], Bool, (Integer, Integer)) -> Bool
extractIncoming (_,a,(_,_)) = a

extractReg :: ([Char], Bool, (Integer, Integer)) -> [Char]
extractReg (a,_,(_,_)) = a

extractTime :: ([Char], Bool, (Integer, Integer)) -> ([Char], Integer, Integer)
extractTime (a,_,(b,c)) = (a, b, c)


extractTime' :: ([Char], Bool, (Integer, Integer)) -> (Integer, Integer)
extractTime' (_,_,(b,c)) = (b, c)

sortByTime :: [([Char], Bool, (Integer, Integer))] -> [([Char], Bool, (Integer, Integer))]
sortByTime a = sortBy (comparing extractTime) [(a,b,(c,d)) | (a,b,(c,d)) <- a]

outgoingSorted :: [([Char], Bool, (Integer, Integer))] -> [([Char], Bool, (Integer, Integer))]
outgoingSorted a = sortBy (comparing extractReg) [(a,b,(c,d)) | (a,b,(c,d)) <- a, b == False]


incomingSorted :: [([Char], Bool, (Integer, Integer))] -> [([Char], Bool, (Integer, Integer))]
incomingSorted a = sortBy (comparing extractReg) [(a,b,(c,d)) | (a,b,(c,d)) <- a, b == True]




groupAndSort :: [([Char], Bool, (Integer, Integer))] -> [[([Char], Bool, (Integer, Integer))]]
groupAndSort a = groupBy (\a b -> extractReg a == extractReg b) $ sort a 


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
 


doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs) 

replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate' (n-1) x

take' :: Int -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ []   = []
take' n ( x : xs ) = x : take' (n-1) xs 

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x1:xs1) (x2:xs2) = (x1,x2) : zip' xs1 xs2

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs

qs :: (Ord a) => [a] -> [a]
qs [] = []
qs (x:xs) = qs [a | a <- xs, a <= x] ++ [x] ++ qs [a | a <- xs, a > x]

qs' :: (Ord a) => [a] -> [a]
qs' [] = []
qs' (x:xs) =
    let smallerOrEqual = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]
    in qs' smallerOrEqual ++ [x] ++ qs' larger



multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

