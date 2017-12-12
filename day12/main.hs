
import Data.Char
import Data.List
import Debug.Trace

main :: IO ()
main = do

    --content <- readFile "test_input.txt"
    content <- readFile "input.txt"
    -- Product a list of tupples where each tupple is (n, [n]) the node and a list of its neighbours
    let input = [let r = (splitAt 1 [read d :: Int | d <- words c]) in (head $ fst r, snd r)| c <- [ filter (\x -> isDigit(x) || x == ' ') l | l <- lines content]]
    print input

    --let sn = head input
    --let res = findReachableNodes sn input [fst sn]

    -- Part 1
    let res = solveA input
    print res

    let res2 = solveB input
    print res2
    --input = []
    print "hi"

solveB :: [(Int, [Int])] -> Int
solveB input = countGroups [fst a | a <- input] input

solveA :: [(Int, [Int])] -> Int
solveA input = let Just (n,_) = find (\x -> fst x == 0) input
               in (length $ nub $ visit n [] input )

countGroups :: [Int] -> [(Int, [Int])] -> Int
countGroups a@(i:_) input = let g = nub $ visit i [] input
                               in 1 + countGroups (a \\ g) input
countGroups [] _ = 0

findReachableNodes :: (Int, [Int]) -> [(Int, [Int])] -> [Int] -> [Int]
findReachableNodes  node@(c, n:ns) nodes visited | trace("V =" ++ show node ++ " vs =" ++ show visited) False = undefined
findReachableNodes  node@(c, n:ns) nodes visited = if elem n visited
                                        then findReachableNodes (c, ns) nodes visited
                                        else let (Just nn) = find (\x -> fst x == n) nodes
                                             in  n:findReachableNodes nn nodes (n:visited)
findReachableNodes  (_, []) _ _ =  []


childrenOfNode :: [(Int, [Int])] -> Int -> [Int]
childrenOfNode g n = let Just r = find (\x -> fst x == n) g in snd r

-- visit :: [Int] -> [(Int, [Int])] -> Int -> ([Int], Int)
-- visit  vs g n = if n `elem` vs
--                 then (vs, n)
--                 else let chs = childrenOfNode g n in mapAccumL (visit g) vs chs

visitChildren :: [Int] -> [Int] -> [(Int, [Int])] -> [Int]
visitChildren (c:cs) vs g = if c `elem` vs
                    then visitChildren cs vs g
                    else visitChildren cs (visit c vs g) g
visitChildren [] vs _ = vs

visit :: Int -> [Int]-> [(Int, [Int])] -> [Int]
visit n vs g = let cs = childrenOfNode g n in n:visitChildren cs (n:vs) g