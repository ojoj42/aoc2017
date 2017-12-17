import Data.Char
import Data.List
import Data.Maybe
import Data.List.Split (splitWhen)
import Debug.Trace

-- guess two "klpceabfighdnmjo"
-- guess thee "ojhdkcinglabmefp" cycle length of by one..
-- guess four eoagncdkihlmbpfj
-- guess five fdnphiegakolcmjb

main :: IO ()
main = do
    content <- readFile "input.txt"
    let input = head $ lines content
    let dancers = ['a'..'p']
    --let input = "s1,x3/4,pe/b"
    --let dancers = ['a'..'e']


    let moves = splitWhen (','==)  input

    let res = dance dancers moves
    let res2 = longDance (10^9) dancers moves []
--    print moves
--    print ['a'..'e']

    print res2

replace :: [Char] -> Int -> Char -> [Char]
replace src n c = let (a, b) =  splitAt n src
                    in a ++ c:(drop 1 b)

longDance :: Integer -> [Char] -> [[Char]] -> [([Char],Integer)]-> [Char]
longDance   n dancers steps _ | trace ("dance " ++ show dancers ++ " " ++ show n) False = undefined
longDance 0 dancers _ _= dancers
longDance n dancers moves hs = let dancers' = (dance dancers moves)
                                   t = find (\x -> (fst x) == dancers') hs
                                    in case t of
                                      Just a  -> let (c, n0) = a
                                                     skipToN = n `mod` (n0 - n)
                                                     res = fst ((reverse hs) !! fromInteger (skipToN-1))
                                                     in trace ("dance " ++ show a ++ " ->" ++ show n ++ dancers' ++ show skipToN) longDance 0 res [] []
                                      Nothing -> trace ("dance " ++ dancers' ++ " ->" ++ show n) longDance (n-1) dancers' moves ((dancers', n):hs)




dance :: [Char] -> [[Char]] -> [Char]
--dance  dancers steps | trace ("dance " ++ show dancers ++ " " ++ show steps) False = undefined
dance  dancers (s:steps) = dance (step s dancers) steps
dance  dancers []        = dancers


step :: [Char] -> [Char] -> [Char]
step ('s':n) dancers = let (a,b) = splitAt (length dancers - (read n)) dancers -- Spin
                            in b++a
step ('x':xs) dancers = let -- Swap
                            (a:b:[]) = splitWhen ('/'==) xs
                            i1 = read a
                            i2 = read b
                            c1 = dancers !! i1
                            c2 = dancers !! i2
                            res = replace (replace dancers i2 c1) i1 c2
                          in res
step ('p':xs) dancers = let
                            (a:b:[]) = splitWhen ('/'==) xs
                            i1 = fromJust (elemIndex (head a) dancers)
                            i2 = fromJust (elemIndex (head b) dancers)
                            c1 = dancers !! i1
                            c2 = dancers !! i2
                            res = replace (replace dancers i2 c1) i1 c2
                          in res


--dance ('x':ids) dancers = let