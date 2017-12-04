import System.IO
import Data.List
import Data.Maybe


run :: IO (Int)
run = do 
    -- For some reason you have to ommit the `let` when reading from a file.
    input <- readFile "input.txt"

    -- Solve it
    let cnt  = sum[1 | p <- [words phrase | phrase <- [line | line <- lines input]], checkPhrase(p)]

    return cnt

-- Check if we have a valid passphrase, Meaning now word in phrase is
-- allowed to be the same, Not even as anagrams.
checkPhrase (x:xs) = if isJust(find (hasSameElements x) xs)
                      then False
                      else checkPhrase xs
checkPhrase [] = True

-- Remove element e from x:xs if it exsits
filterOneElement::(Char, [Char]) -> [Char]
filterOneElement(e, x:xs) = if e == x
            then xs
            else x : filterOneElement(e, xs)
filterOneElement(_, []) = []
    
-- Compare two lists and determine if they contain the same elements.
hasSameElements :: [Char] -> [Char] -> Bool
hasSameElements (x:xs) ys = if elem x ys
    then hasSameElements xs (filterOneElement(x, ys))
    else False
hasSameElements [] [] = True
hasSameElements _ _ = False
