import Data.List

-- Guess 1 3590424
-- Guess 2 41833111

itr :: Integer
itr = 50*(10^6)

solveA:: Int
solveA = let (p,l) = spinLock [0] 0 2017
        in l !! p

main :: IO()
main = do
    let res = solveB
    print $ show res


solveB:: (Int, Int)
solveB = spinLock3 (0,0) 0 (50*(10^6)) 1

spinLock :: [Int] -> Int -> Int-> (Int, [Int])
spinLock xs p 0 = (p, xs)
spinLock xs p cnt = let ip = (p + 394) `mod` l
                        l = (length xs)
                        (a,b) = splitAt ip xs
                        in spinLock (a++(l:b)) (ip+1) (cnt-1)

-- This solves the problem to find value in position one in the
-- circular buffer, Unfortunatly that was not the question.
spinLock2 :: (Int, Int) -> Int -> Int -> Int -> (Int, Int)
spinLock2 r _ 0 _ = r
spinLock2 r@(f, _) p i l
                        | ip == (l - 1) = spinLock2 (l, f) (ip+1) (i-1) (l+1)
                        | ip == 0 = spinLock2 (f, l) (ip+1) (i-1) (l+1)
                        | otherwise = spinLock2 r (ip+1) (i-1) (l+1)
                        where
                        ip = (p + 394) `mod` l

spinLock3 :: (Int, Int) -> Int -> Int -> Int -> (Int, Int)
spinLock3 r _ 0 _ = r
spinLock3 r@(p0, _) p i l
                        | ip == p0 = spinLock3 (p0, l) (ip+1) (i-1) (l+1)
                        | otherwise = spinLock3 r (ip+1) (i-1) (l+1)
                        where
                        ip = (p + 394) `mod` l
