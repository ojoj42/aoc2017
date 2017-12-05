import System.IO
import Debug.Trace
import Data.Array
import Control.DeepSeq


main :: IO (Int)
main = do
    file_input <- readFile "input.txt"
    
    -- Solution using lists
    let input = [read a | a <- lines file_input]
    let res = (move 0 [] input) - 1;
    
    --print res
    -- Solution using arrays
    --let maze =  array (1,5) (zip [1,2..] [0, 3, 0, 1, -3])
    let maze = array (1, length input) (zip [1,2..] input)
    let res2 = runWithArrays 1 maze
    print res2

    return res2;

-- trace ("S " ++ show (pos) ++ show (maze)) 
runWithArrays :: (Int) -> (Array Int Int) -> (Int)
runWithArrays pos maze
    | inMaze = 1 + runWithArrays (pos+steps) (maze//[(pos, steps')])
    | not inMaze = 0
    where inMaze = inRange(bounds maze) pos
          steps = maze!pos
          steps'
              | steps >= 3 = steps -1
              | steps < 3 = steps + 1

--Lists eq slow in this case at least.
--trace ("S " ++ show (reverse l) ++ show (r))
move s l@(y:ys) r@(x:xs)
    | s == 0 = 1 + move (x) (y:ys) ((x + 1):xs)
    | s < 0 = move (s + 1) (ys) (y:x:xs)
    | s > 0 = move (s - 1) (x:y:ys) (xs)
move s l@([]) r@(x:xs)
    | s == 0 = 1 + move (x) ([]) ((x + 1):xs)
    | s > 0 = move (s - 1) (x:[]) (xs)
    | s < 0 = 0
move s l@(y:ys) r@([])
    | s == 0 = 1 + move (y) ((y+1):ys) ([])
    | s < 0 = 0
    | s > 0 = move (s - 1) (ys) (y:[])
