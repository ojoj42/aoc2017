import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)
import Data.Bits

-- 2147483647  = (2^31 -1), Is the eighth marsenne prim
-- 2147483647 i also a number in Binary from all ones.
n :: Integer
n = 2147483647

main :: IO ()
main = do
    let resTest = solve (10^6 * 5) 65 8921
    print $ show resTest
    --Generator A starts with 883
    --Generator B starts with 879
    let res = solve (10^6 * 5) 883 879
    print $ show res



gen :: Integer -> Integer -> Integer
gen f s = (s*f) `mod` n

genM :: Integer -> Integer -> Integer -> Integer
genM f m s  = let s' = (s*f) `mod` n
            in if (s' `mod` m) == 0
               then s'
               else genM f m s'

genA :: Integer -> Integer
genB :: Integer -> Integer
--part 1
--genA = (gen 16807)
--genB = (gen 48271)

--part 2
genA = (genM 16807 4)
genB = (genM 48271 8)

binMod :: Integer -> Integer -> Integer
binMod  a b = if (a+1) == b
            then 1
            else ((.&.) a b)

solve :: Integer -> Integer -> Integer -> Integer
solve 0 _ _ = 0
solve cnt sa sb = let sa' = genA sa
                      sb' = genB sb
                      e = if ((.&.) sa' 0xFFFF) == ((.&.) sb' 0xFFFF) then 1 else 0
                 in e + solve (cnt - 1) sa' sb'

