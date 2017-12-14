import Data.Bits
import Data.Char

main :: IO ()
main = do
    let key = "wenycdww"
    let test = hashToInt $ knotHash "flqrgnkx-0"

    let a = sum [sum [popCount d | d <-(hashToInt $ knotHash t)] | i <- [0..127], let t = key ++ "-" ++ show i]

    print a


--popCount counts bits in argument

hashToInt :: [Char] -> [Int]
hashToInt h = [digitToInt i | i <- h]

-- Knots hash from day 10
knotHash :: [Char] -> [Char]
knotHash input = let
                 d = appendSuffix $ charsToInts input
                 (l, _,_) =  foldr (.) id (replicate 64 $ twist2 d) ([0..255], 0, 0)
                 dense = makeDense l
                in toNiceHash dense

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