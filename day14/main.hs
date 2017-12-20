import Data.Bits
import Data.Char
import Numeric (showHex, showIntAtBase)

import Control.Applicative
import Control.Arrow
import Data.Graph.Inductive

main :: IO ()
main = do
    let key = "wenycdww"
    let test = hashToInt $ knotHash "flqrgnkx-0"

    -- part 1
    let a = sum [sum [popCount d | d <-(hashToInt $ knotHash t)] | i <- [0..127], let t = key ++ "-" ++ show i]

    let m = [hexToBinary (hashToInt $ knotHash t) | i <- [0..127], let t = key ++ "-" ++ show i]
    let ns =   [(j*128 + i, Just 1)| i <-[0..127], j <-[0..127], ((m!!j)!!i) /= 0]
    let nes =   concat [[(j*128 + i,b*128 + a, ()) | (a,b) <- (neigh (i, j) (127, 127) m)] | i <-[0..127], j <-[0..127], ((m!!j)!!i) /= 0]
    --g :: Gr (Maybe Int) ()
    let g = (mkGraph ns nes) :: Gr (Maybe Int) ()
    print $ show $ length ns
    print $ show g
    --print ns

    --print a

graph :: Gr (Maybe Int) ()
graph =  mkGraph [(1, Just 1),(2, Just 2), (3, Just 3)] [(1,2,())]


hexToBinary :: [Int] -> [Int]
hexToBinary input = concat [[foo((.&.) i d) | d <- [1, 2, 4, 8]] | i <- input, let foo d = if d > 0 then 1 else 0]

neigh :: (Int, Int) -> (Int, Int) -> [[Int]] -> [(Int, Int)]
neigh (x,y) (w, h) m = [(x + sx, y + sy) | (sx, sy)  <- [(0,1),(0,-1), (1, 0),(-1, 0)], valid((x + sx, y + sy))]
                    where valid (a, b) = a >= 0 && a <= w && b >= 0 && b <= h && ((m !! b) !! a) == 1


--popCount counts set bits in argument

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