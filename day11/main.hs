import Debug.Trace
import Data.List

data Dir = NE | NW | SW | SE | S | N deriving (Show, Eq, Bounded, Enum, Ord)


main :: IO ()
main = do
    contents <- readFile "input.txt"

    let ok1 = solveA "ne,ne,ne" == 3
    let ok2 = solveA "ne,ne,sw,sw" == 0
    let ok3 = solveA "ne,ne,s,s" == 2
    let ok4 = solveA "se,sw,se,sw,sw" == 3

    print $ "Part 1"
    print $ "Test ok " ++ show (ok1 && ok2 && ok3 && ok4)
    let resA = solveA $ head $ lines contents
    print $ "Steps home " ++ show resA

    let resB = solveB $ head $ lines contents
    print $ "Max steps away from home " ++ show resB

distanceO :: (Int, Int) -> Int
distanceO (a,b) = abs a + abs  b

solveA :: [Char] -> Int
solveA input = let dirs = parseInput input
                   pos = calcPos dirs
                in findPath pos

solveB :: [Char] -> Int
solveB input = let dirs = parseInput input
                   -- inits will create a list on how to get to every step on the way.
                   -- That is very iefficient, But the fastest to code.
                   poss = [calcPos a | a <- (drop 1 $ inits dirs)]
                   steps = [findPath a | a <- poss]
                in maximum steps


-- (trace $ "s " ++ show dir ++ show pos ++ show newPos)
findPath :: (Int, Int) -> Int
findPath (0, 0) = 0
findPath pos = let
                        (_,dir) = minimum [ (distanceO $ addC pos $ step d, d) |
                                            d <- [minBound .. maxBound] :: [Dir]]
                        newPos = addC pos $ step dir
                      in 1 + findPath newPos



calcPos :: [Dir] -> (Int, Int)
calcPos  dirs = foldr1 (addC) [ step d | d <- dirs]

addC :: (Int, Int) -> (Int, Int) -> (Int, Int)
addC (a, b) (a1, b1) = (a+a1, b+b1)

subC :: (Int, Int) -> (Int, Int) -> (Int, Int)
subC (a, b) (a1, b1) = (a-a1, b-b1)


parseInput::[Char]->[Dir]
parseInput (',':cs) = parseInput(cs)
parseInput ('s':'w':cs) = SW:parseInput(cs)
parseInput ('s':'e':cs) = SE:parseInput(cs)
parseInput ('s':cs) = S:parseInput(cs)
parseInput ('n':'w':cs) = NW:parseInput(cs)
parseInput ('n':'e':cs) = NE:parseInput(cs)
parseInput ('n':cs) = N:parseInput(cs)
parseInput [] = []
parseInput _ = error "Error"

step :: Dir -> (Int, Int)
step d = case d of
    N  -> (0, 2)
    NE -> (1, 1)
    NW -> (-1, 1)
    S  -> (0, -2)
    SW -> (-1, -1)
    SE -> (1, -1)