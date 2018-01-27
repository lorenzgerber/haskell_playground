import Register
import Data.List
import Data.Ord

--data Time a = Time (Integer, Integer) deriving (Show)
type Time = (Integer, Integer)

timeAdd :: Time -> Time -> Time 
(a, b) `timeAdd` (c, d) = (a + c + div (b+d) 60, mod (b+d) 60)

timeSub :: Time -> Time -> Time
(a, b) `timeSub` (c, d) = (a - c + div (b-d) 60, mod (b - d) 60) 

type PhusEntry = (String, Bool, Time)

getTime :: PhusEntry -> Time
getTime (_,_,a) = a

isStart :: PhusEntry -> Bool
isStart (_,a,_) = a

isEnd :: PhusEntry -> Bool
isEnd (_,a,_) = not a

type PhusList = [PhusEntry]



