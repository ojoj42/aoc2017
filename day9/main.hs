import Test.HUnit

-- Tests
test1 = TestCase (assertEqual "for {}" 1 $ solveA "{}")
test2 = TestCase (assertEqual "for {{{}}}" 6 $ solveA "{{{}}}")
test3 = TestCase (assertEqual "for {{},{}}" 5 $ solveA "{{},{}}")
test4 = TestCase (assertEqual "for {{{},{},{{}}}}" 16 $ solveA "{{{},{},{{}}}}")
test5 = TestCase (assertEqual "for {<a>,<a>,<a>,<a>}" 1 $ solveA "{<a>,<a>,<a>,<a>}")
test6 = TestCase (assertEqual "for {{<ab>},{<ab>},{<ab>},{<ab>}}" 9 $ solveA "{{<ab>},{<ab>},{<ab>},{<ab>}}")
test7 = TestCase (assertEqual "for {{<!!>},{<!!>},{<!!>},{<!!>}}" 9 $ solveA "{{<!!>},{<!!>},{<!!>},{<!!>}}")
test8 = TestCase (assertEqual "{{<a!>},{<a!>},{<a!>},{<ab>}}" 3 $ solveA "{{<a!>},{<a!>},{<a!>},{<ab>}}")

tests = TestList [test1,
                   test2,
                   test3,
                   test4,
                   test5,
                   test6,
                   test7,
                   test8]

main :: IO ()
main = do 
    contents <- readFile "input.txt"
    runTestTT tests
    let input = head $ lines contents
    print input

    let res = solveAB input
    -- 13154
    print res

solveA :: [Char] -> Int
solveA input = fst (solveAB input)

solveAB :: [Char] -> (Int, Int)
solveAB input = solve input 1

skipTo :: [Char] -> Char -> ([Char], Int)
skipTo (x:xs) y
        | x == y = (xs,0)
        | x == '!' = (skipTo (tail xs) y)
        | otherwise = addCIT ("",1) (skipTo (xs) y)


addCIT :: ([Char], Int) -> ([Char], Int) -> ([Char], Int)
addCIT (_,i) (l, i2) = (l, i+i2)

addT :: (Int, Int) -> (Int, Int) -> (Int, Int)
addT (a0,b0) (a1, b1) = (a0 + a1, b0 + b1)

solve :: [Char] -> Int -> (Int, Int)
solve ('{':ds) level = addT (level, 0) (solve ds (level + 1))
solve ('<':ds) level = let (r, i) = skipTo ds '>' in 
                    addT (0, i) (solve r level)
solve ('}':ds) level = (solve ds (level - 1))
solve (_:ds) level = solve ds (level)
solve [] _ = (0, 0)
