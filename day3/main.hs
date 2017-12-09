import Data.List

-- Moves
data Move = MUp | MDown | MRight | MLeft deriving Show
data Coord = C2D (Int, Int)

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:y:xs) = x : y : if p x then takeWhileInclusive p xs
                                         else []

main :: IO ()
main = do
    let res = takeWhileInclusive (\(_,_,c) -> c <  265149) $ go moves (0,0) [((0,0),1)]
    print res
    print "hi"

--directions = [ replicate s d | s<-([1, 2 ..] >>= (\x -> [x, x])), d<-[MRight, MUp, MLeft, MDown]]
--directions :: Move -> [Move]
--directions move step 0 = move:
--directions move step 1 =  
    --(replicate move step):(replicate next_move(move) step)directions next_move move

relN = [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]

go::[Move]->(Int, Int)-> [((Int, Int), Int)]->[(Int, Int, Int)]
go (m:ms) (x, y) stash = let (x',y') = (move m (x,y))
                             val = (calcN stash (x',y'))
                             in (x',y', val):(go ms  (x',y') (((x',y'), val):stash))

move :: Move -> (Int, Int) -> (Int, Int)
move MUp (x,y) = (x,y+1)
move MDown (x,y) = (x,y-1)
move MRight (x,y) = (x+1,y)
move MLeft (x,y) = (x-1,y)

calcN :: [((Int, Int), Int)] -> (Int, Int) -> Int
calcN m (x,y) = sum [valueForN m (x+a, y+b) | (a,b) <- relN]

valueForN :: [((Int, Int), Int)] -> (Int, Int) -> Int
valueForN values pos = let res = find (\x -> (fst x) == pos) values
                       in case res of
                        Just ((_,_), v) -> v
                        Nothing -> 0

moves:: [Move]
moves = moves' MRight 1

moves'::Move -> Int -> [Move]
moves' m n = let m1 = next_move(m)
                 m2 = next_move(m1)
            in (addMoveN m n) ++ (addMoveN m1 n) ++ moves' m2 ( n + 1)

addMoveN::Move -> Int -> [Move]
addMoveN _ 0 = []
addMoveN m n = m:addMoveN m (n-1)

next_move :: Move -> Move
next_move MRight = MUp
next_move MUp = MLeft 
next_move MLeft = MDown 
next_move MDown = MRight 

-- take 10 [replicate m n| n <- cycle [MRight, MUp, MRight, MLeft]| m <- ([1, 2 ..] >>= (\x -> [x, x]))]
-- [replicate n m| n <- cycle [MRight, MUp, MRight, MLeft], [1, 2 ..] >>= (\x -> [x, x])]
--take 10 [1, 2 ..] >>= (\x -> [x, x])
--take 10 $ cycle [MRight, MUp, MRight, MLeft]

-- Move numer pattern in spiral e.g 1,1,2,2,3,3
--moves :: Move -> [Move]
--moves

