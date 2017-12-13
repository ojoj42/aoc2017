import Data.Char
import Data.List
import Debug.Trace

data Layer = Layer {depth :: Int, range :: Int} deriving (Show, Eq, Ord)

severity :: Layer -> Int
severity (Layer d r) = d*r

main :: IO ()
main = do
    --content <- readFile "test_input.txt"
    content <- readFile "input.txt"
    let input = [Layer (read l) (read r)  | e <- [filter (\x -> isDigit x || x == ' ') l | l <- lines content], let l:r:[] = words e]
    let input' = fillMissing input
    let tot = sverityForVisit input' 0 0
    print tot

    let delay = solveB input'
    print delay
    --print input'

solveB :: [Layer] -> Int
solveB l = findZeroSeverityDelay l 0 42

findZeroSeverityDelay :: [Layer] -> Int -> Int -> Int
findZeroSeverityDelay _ delay 0 = delay - 1
findZeroSeverityDelay layers delay _ = let s = testDelay layers delay 0
                                        in findZeroSeverityDelay layers (delay + 1) s




fillMissing :: [Layer] -> [Layer]
fillMissing layers = let m = depth $ maximum layers
                     in reverse $ fillMissing' layers m


fillMissing' :: [Layer] -> Int -> [Layer]
fillMissing' _ (-1) = []
fillMissing' layers n    = case x of
                            Just a -> a:(fillMissing' layers (n-1))
                            Nothing -> (Layer n 0):(fillMissing' layers (n-1))
                        where x = find (\l -> depth l == n) layers

layerPos :: Layer -> Int -> Int
layerPos (Layer _ r) tick
                        | r == 0 = (-1)
                        | m2 < r = m2 -- 0,1,3
                        | otherwise = (r-1) - (m2 - (r-1))
                        where lr = (2*r) - 2
                              m2 = mod tick lr

sverityForVisit :: [Layer] -> Int -> Int -> Int
sverityForVisit (l:ls) tick pos
                    | pos == lPos = trace (show tick ++ " - " ++ show l) (severity l) + (sverityForVisit ls (tick + 1) pos)
                    | otherwise = trace (show tick ++ " - " ++ show l ++ " - " ++ show lPos) (sverityForVisit ls (tick + 1) pos)
                    where lPos = layerPos l tick
sverityForVisit [] _ _ = 0


testDelay :: [Layer] -> Int -> Int -> Int
testDelay (l:ls) tick pos
                    | pos == lPos = 42
                    | otherwise = (testDelay ls (tick + 1) pos)
                    where lPos = layerPos l tick
testDelay [] _ _ = 0