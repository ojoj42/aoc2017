import Data.List

main :: IO(Int)
main = do

    contents <- readFile "input.txt"
    --contents <- readFile "test_input.txt"
    let input = [words line | line <- lines contents]
    let stash = parse input [] 0
    --print (snd stash)
    let res = sortBy (sortOnSnd) (fst stash)

    print res
    print (snd stash)
    return 0

parse :: [[[Char]]] -> [([Char], Int)] -> Int -> ([([Char], Int)], Int)
parse  (x:xs) stash h = let res = (parseLn x stash) in 
            case res of 
                Just a  -> let m = (max (snd a) h) in parse xs (stashSet a stash) m
                Nothing -> parse xs stash h
parse  [] stash h = (stash, h)

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

sortOnSnd :: ([Char], Int) ->  ([Char], Int) -> Ordering
sortOnSnd (_, a) (_, b) = compare a b

parseInt :: [Char] -> Int
parseInt s = read s

stashGet :: [Char] -> [([Char], Int)] -> ([Char], Int)
stashGet k s = let res = find  (\x -> k == fst x) s in
             case res of
                Just a -> a
                Nothing -> (k, 0)

stashSet :: ([Char], Int) ->  [([Char], Int)] -> [([Char], Int)]
stashSet (k, v) s = let res = find  (\x -> k == fst x) s in
        case res of
            Just _ -> (k, v) : filter (\x -> k /= fst x) s
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
