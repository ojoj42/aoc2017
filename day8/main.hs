import System.IO
import Data.List
import Data.Maybe

main :: IO(Int)
main = do

    contents <- readFile "input.txt"
    --contents <- readFile "test_input.txt"
    let input = [words line | line <- lines contents]
    let stash = parse input []
    
    let res = sortBy (sortOnSnd) stash

    print res
    return 0

parse :: [[[Char]]] -> [([Char], Int)] -> [([Char], Int)]
parse  (x:xs) stash = let res = (parseLn x stash) in 
            case res of 
                Just a  -> parse xs (stashSet a stash)
                Nothing -> parse xs stash
parse  [] stash = stash 

--b inc 5 if a > 1
parseLn :: [[Char]] -> [([Char], Int)] -> Maybe ([Char], Int)
parseLn (a:b:c:"if":d:e:f:[]) stash
        |  pass = Just (fst opL, op (snd opL) opR) 
        |  otherwise = Nothing
        where pass = cndOp (snd cndL) cndR
              cndOp = parseCndOp e
              cndR = read f
              cndL = stashGet d stash
              op = parseOp b
              opL = stashGet a stash
              opR = read c

sortOnSnd (_, a) (_, b) = compare a b

parseInt :: [Char] -> Int
parseInt s = read s

stashGet k s = let res = find  (\x -> k == fst x) s in
             case res of
                Just a -> a
                Nothing -> (k, 0)

stashSet :: ([Char], Int) ->  [([Char], Int)] -> [([Char], Int)]
stashSet (k, v) s = let res = find  (\x -> k == fst x) s in
        case res of
            Just a -> (k, v) : filter (\x -> k /= fst x) s
            Nothing -> (k, v) : s

parseOp :: Num a => [Char] -> a -> a -> a    
parseOp "inc" = (+)
parseOp "dec" = (-)

parseCndOp :: [Char] -> Int -> Int -> Bool    
parseCndOp ">" = (>)
parseCndOp ">=" = (>=)
parseCndOp "<" = (<)
parseCndOp "<=" = (<=)
parseCndOp "==" = (==)
parseCndOp "!=" = (/=)

    --data Tree a = Leaf a | Branch [Tree a]
    
    -- instance (Eq a) => Eq (Tree a) where
    --    (Leaf a)   == (Leaf b)   = a == b
    --    (Branch a) == (Branch b) = a == b
        --_          == _          = False
--    instance (Num a,Num b) => Num (Pair a b) where
        --Pair (a,b) + Pair (c,d) = Pair (a+c,b+d)
        --Pair (a,b) * Pair (c,d) = Pair (a*c,b*d)
        --Pair (a,b) - Pair (c,d) = Pair (a-c,b-d)
        --abs    (Pair (a,b)) = Pair (abs a,    abs b) 
        --signum (Pair (a,b)) = Pair (signum a, signum b) 
        --fromInteger i = Pair (fromInteger i, fromInteger i)



--data DNum a = DInt (a) deriving ( Show)

--instance (Eq a) => Eq (DNum a) where
--    DInt (a) ==  DInt(b) =  a == b
