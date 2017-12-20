import Data.Array
import Data.List
import Data.Maybe
import Debug.Trace
import Text.Read

main :: IO()
main = do
    --contents <- readFile "test_input.txt"
    contents <- readFile "input.txt"

    let input = [words l |l <- lines contents];
    let program = array (1, length input) (zip [1,2..] input)

    let res = run program 1 [] 0

    --print $ show program
    print $ show res




run :: Array Int [[Char]] -> Int -> [([Char], Int)] -> Int -> Int
run program pc regs snds
                | pc' /= 0 = run program pc' regs' snds'
                | otherwise = freq
                where (freq, pc', regs', snds') = execute (program!pc) pc regs snds
--dance  dancers steps | trace ("dance " ++ show dancers ++ " " ++ show steps) False = undefined

execute :: [[Char]] -> Int -> [([Char], Int)] ->  Int -> (Int, Int, [([Char], Int)], Int)
--execute cmd pc regs snds | trace ("cmd=" ++ show cmd ++ "pc=" ++ show pc ++ "snds=" ++ show snds) False = undefined
execute ("snd":x:[]) pc regs snds  = let snds' = (valueOf regs x)
                                    in (0, (pc+1), regs, snds')
execute ("set":x:y:[]) pc regs snds  = (0, (pc+1), (setReg regs x (valueOf regs y)), snds)
execute ("add":x:y:[]) pc regs snds  = (0, (pc+1), (setReg regs x ((valueOf regs x) + (valueOf regs y))), snds)
execute ("mul":x:y:[]) pc regs snds  = (0, (pc+1), (setReg regs x ((valueOf regs x) * (valueOf regs y))), snds)
execute ("mod":x:y:[]) pc regs snds  = (0, (pc+1), (setReg regs x ((valueOf regs x) `mod` (valueOf regs y))), snds)
execute ("rcv":x:[]) pc regs snds
                                | s == 0 = (0, (pc+1), regs, snds)
                                | otherwise = (f, 0, regs, snds)
                                where s = getReg regs x
                                      f = (snds)
execute ("jgz":x:y:[]) pc regs snds
                                | s > 0 = (0, (pc+(valueOf regs y)), regs, snds)
                                | otherwise = (0, pc + 1, regs, snds)
                                where s = (valueOf regs x)



valueOf :: [([Char], Int)] -> [Char] -> Int
valueOf regs k = case d of
             Just a -> a
             Nothing -> getReg regs k
             where d = (readMaybe k :: Maybe Int)

setReg :: [([Char], Int)] -> [Char] -> Int -> [([Char], Int)]
setReg r c  v | trace ("set r=" ++ show r ++ "c=" ++ show c  ++ "v=" ++ show v) False = undefined
setReg regs c v = case r of
                    Just a -> (c, v):filter (\x -> fst x /= fst a) regs
                    Nothing -> (c, v):regs
                where r = find (\x -> fst x == c) regs

getReg :: [([Char], Int)] -> [Char] -> Int
getReg r c | trace ("get r=" ++ show r ++ "c=" ++ show c) False = undefined
getReg regs c = case r of
                Just a -> (snd a)
                Nothing -> 0
                where r = find (\x -> fst x == c) regs

