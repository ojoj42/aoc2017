import Numeric (showHex)
import Data.Bits
import Data.Char
--import Debug.Trace

main :: IO ()
main = do
    let input_2 = [165,1,255,31,87,52,24,113,0,91,148,254,158,2,73,153]
    let input_1 = [3, 4, 1, 5]
    let res1 = solve 5 input_1
    let res2 = solve 256 input_2
         --product $ take 2 $ last $ adjustPos $ twist 0 [3,4,1,5] [0..4] 0

    print $ "Part 1"
    print $ "With test input " ++ show res1 --12
    print $ "With real input " ++ show res2 --4114
    --let res3 = (solve3 input_2)

    print $ "Part 2"
    let ok1 = solveB ""  == "a2582a3a0e66e6e86e3812dcb672a272"
    let ok2 = solveB "AoC 2017"  == "33efeb34ea91902bb2f59c9920caa6cd"
    let ok3 = solveB "1,2,3"  == "3efbe78a8d82f29979031a4aa0b16a9d"
    let ok4 = solveB "1,2,4"  == "63960835bcdc130f0b66d7ff4f6a5a8e"
    print $ "Part 2, tests are " ++ show  (ok1 && ok2 && ok3 && ok4)

    let resB = solveB "165,1,255,31,87,52,24,113,0,91,148,254,158,2,73,153"
    print $ "Answer for part 2 is " ++ show resB

-- Standard lenfth suffix, Whatever that is


-- Moving 3 steps forward in  cyclic list
-- take 5 $ drop 3 $ cycle [0..4]


-- splitAt 3 $ take 5 $ drop 3 $ cycle [0..4]

-- Convert [Char] to [Int] array based on ASCII values of each Char
-- [fromEnum i | i <- "1,2,3"]

-- Apply function n times (10)
-- foldr (.) id (replicate 10 f)

--foldr1 (xor)
-- [foldr1 (xor) (take 16 $ drop i [0..255])| i <- [0..15]]
-- foldr1 (xor) [65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0 , 2 , 5 , 68 , 22] == 64

toHex :: Int -> [Char]
toHex n = intToDigit (shiftR ((.&.) n 0xF0) 4) : intToDigit ((.&.) n 0x0F) : ""

toNiceHash :: [Int] -> [Char]
toNiceHash (i:is) =  toHex i ++ toNiceHash is
toNiceHash [] = []

makeDense :: [Int] -> [Int]
makeDense input = [foldr1 (xor) (take 16 $ drop (i*16) input)| i <- [0..15]]

appendSuffix :: [Int] -> [Int]
appendSuffix input = input ++ [17, 31, 73, 47, 23]

charsToInts :: [Char] -> [Int]
charsToInts cs = [fromEnum i | i <- cs]

solve :: Int -> [Int] -> Int
solve n steps = solve2 n steps

solve1 :: Int -> [Int] -> Int
solve1 n steps = product $ take 2 $ last $ adjustPos $ twist 0 steps [0..(n-1)] 0

solve2 :: Int -> [Int] -> Int
solve2 n steps = let (l, _,_) =  twist2 steps ([0..(n-1)], 0, 0) in product $ take 2 $ l

solveB :: [Char] -> [Char]
solveB input = let
                 d = appendSuffix $ charsToInts input
                 (l, _,_) =  foldr (.) id (replicate 64 $ twist2 d) ([0..255], 0, 0)
                 dense = makeDense l
                in toNiceHash dense

solve3 :: [Int] -> Int
solve3 steps = let (l, _,_) =  foldr (.) id (replicate 64 $ twist2 steps) ([0..255], 0, 0) in product $ take 2 $ l

adjustPos :: [([Int], Int)] -> [[Int]]
adjustPos ((l, o):xs) = (take (length l) $ drop ((length l)-o) $ cycle l) : adjustPos xs
adjustPos [] =  []

twist :: Int -> [Int] -> [Int] -> Int -> [([Int], Int)]
twist l (s:rs) d pos =  let (h, t) = splitAt s $ take (length d) $ cycle d
                            revH = (reverse h ++ t)
                            newList = take (length d) $ drop (s+l) $ cycle revH -- Modifies list, So that current pos is first in list
                            acutalPos = (pos + s + l) `mod` (length d)
                    in  (newList,  acutalPos) : (twist (l+1) rs newList acutalPos)
twist _ [] _  _  = []
--trace ("S " ++ show (reverse l) ++ show (r))
--trace ("S " ++ show (list') ++ show (list''))

-- Solution keeps order in window
twist2:: [Int] -> ([Int], Int, Int) -> ([Int], Int, Int)
twist2 (l:ls) (list, skip, pos) =
                            let
                                list' = take (length list) $ drop pos $ cycle list
                                (h, t) = splitAt l $ take (length list) $ cycle list'
                                list'' = reverse h ++ t
                                pos' = (pos + skip + l) `mod` (length list)
                                list''' = take (length list) $ drop ((length list)-pos) $ cycle list''
                            in (twist2 ls (list''', (skip+1), pos'))
twist2 [] (list, skip, pos) = (list, skip, pos)