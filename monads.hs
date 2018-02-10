import Control.Monad


powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x  in (y, log ++ newLog)



justH :: Maybe Char
justH = do
    (x:xs) <- Just "hello"
    return x

wopwop :: Maybe Char
wopwop = do
    (x:sx) <- Just ""
    return x

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c', r') <- [(c+2, r-1),(c+2,r+1), (c-2,r-1),(c-2,r+1),(c+1,r-2), (c+1,r+2),(c-1,r-2),(c-1,r+2)]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c',r')

in3 :: KnightPos -> [KnightPos]
in3 start = do
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem`in3 start

