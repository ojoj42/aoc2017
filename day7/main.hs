
import Data.Char
import Data.List
import Data.Maybe
import Debug.Trace

data Tree a = EmptyTree | Node a [Tree a] deriving (Show, Eq)

main :: IO()
main = do 
    --contents <- readFile "test_input.txt"
    contents <- readFile "input.txt"
    let  d = [(parseHead (ws), parseChilds(ws)) | l <- lines contents, let ws = words l]
    
    --print d
    --let t1 = findNameInChilds "padx" d
    --print t1

    -- Solution for part 1
    let root = fromJust $ find (\((n,_),_) -> not $ hasParent n d) d
--    print root

    -- Solution for part 2
    let tree = buildSubTree root d
    let weight = weightOfNode(tree);
    print weight
--    let badNode = findUnbalancedNode(tree)
    
    --
    
    let funres = fun(tree)
    print (getName $ snd funres)
    let weights = [(weightOfNode c, getWeight c) | c <- (getChildren (snd funres))]
    print $ fst funres
    print weights
    

    --print weights
    --print badNode
    

    --hasParent((fst $ fst x) d)
    --print root
    --let node = (Node 1 [(Node 2 [EmptyTree])])
    --let node2 = Node 3 [node]
    --let testTree = buildSubTree t1 d
    --print testTree
--trace ("b " ++ show weight ++" -- "++ show ([fst t | t <- cws])) 
fun :: Tree ([Char], Int) -> (Int, Tree ([Char], Int))
fun c@(Node (_, w) children)
                | not $ null ubc = trace ("S " ++ show (getName $ head ubc) ++" -- "++ show ([fst t | t <- cws])) (w, head ubc)
                | not balanced = trace ("u "++ show (getName c) ++"-"++show w++" -- " ++ show weight ++" -- "++ show ([fst t | t <- cws])) (w, c) 
                | balanced = (weight, EmptyTree)
                    where weight = w + foldr (\x y -> y + (fst x)) 0 cws
                          cws = [(fun t) | t <- children]
                          ubc = filter (/=EmptyTree) [snd t | t <- cws]
                          balanced = (length $ nub [(w + fst t) | t <- cws]) <= 1
fun EmptyTree = (0, EmptyTree)

getChildren :: Tree ([Char], Int) -> [Tree ([Char], Int)]
getChildren (Node _ c) = c

getWeight :: Tree ([Char], Int) -> Int
getWeight (Node (_,w) _) = w

getName :: Tree ([Char], Int) -> [Char]
getName (Node (n,_) _) = n

findUnbalancedNode :: Tree ([Char], Int) -> [Tree ([Char], Int)]
findUnbalancedNode curr@(Node (_, w) children)
            | not balanced =  trace ("S " ++ show (cws)) [curr]
            | balanced = head [findUnbalancedNode c | c <- children]
                where cws = [(weightOfNode c) | c <- children]
                      balanced = (length $ nub cws) <= 1
findUnbalancedNode EmptyTree = []

weightOfNode :: Tree ([Char], Int) -> Int
weightOfNode (Node (_, w) children) = w + sum [weightOfNode c | c <- children]
weightOfNode EmptyTree = 0


buildSubTree :: (([Char], Int), [[Char]]) -> [(([Char], Int), [[Char]])] -> Tree ([Char], Int)
buildSubTree ((n, w), cs) list = Node (n, w) dt
    where dt
            | (null cs) = [EmptyTree]
            | otherwise = [buildSubTree (findForName c list) list | c <- cs]

    
parseHead :: [[Char]] -> ([Char], Int)
parseHead (i:a:_) = (i, read $ filter (isDigit) a)

parseChilds :: [[Char]] ->[[Char]]
parseChilds (_:_:_:xs) = [filter (','/=) x | x<-xs]
parseChilds (_:_:[]) = []
parseChilds [] = []

findNameInChilds :: [Char] -> [(([Char], Int), [[Char]])] -> (([Char], Int), [[Char]])
findNameInChilds name ns = head [n | n <- ns, elem name $ snd n]

findForName :: [Char] -> [(([Char], Int), [[Char]])] -> (([Char], Int), [[Char]])
findForName name ns = head [n | n <- ns, name == (fst $ fst n)]

hasParent :: [Char] -> [(([Char], Int), [[Char]])] -> Bool
hasParent n list = isJust $ find (\x -> elem n $ snd x) list
