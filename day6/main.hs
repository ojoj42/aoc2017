import Data.ByteString as BS
import Crypto.Hash.SHA256 as SHA256
import Data.Array
import Data.List as DL
import Debug.Trace

main :: IO ()
main = do
    let input = [14,0,15,12,11,11,3,5,1,6,8,4,9,1,8,4]
    --let input = [0, 2, 7, 0]
    let banks =  array (1, toInteger(DL.length input)) (Prelude.zip [1,2..] input)
    print banks
    let res = run banks []
    let cycle_length = DL.length $ DL.takeWhile (fst res /=) $ snd res
    --let res2 = length (snd res)
    print (cycle_length+1)
    print ((DL.length $ snd res))

--run :: (Array Integer Integer) -> [Integer] -> Integer 
run banks history = case e of
     Just a -> (a, history) 
     Nothing -> (run new_banks (curr_hash:history))
    where e = DL.find (curr_hash==) history
          curr_hash = dmo_hash2(banks)
          new_banks = distribute (max_id) ((banks!max_id)) banks_zero
          banks_zero = (banks//[(max_id, 0)])
          max_id = array_maximum_idx banks
          
distribute pos 0 banks = banks
distribute pos amount banks = distribute newPos (amount-1) (banks//[(newPos, (banks!newPos) + 1)])
    where newPos = if inRange(bounds banks) (pos + 1)
          then pos +1
          else 1 

array_maximum_idx :: (Array Integer Integer) -> Integer
array_maximum_idx a =  DL.head [i | i <- [1..(snd $ bounds a)], (DL.maximum a) == a!i]

dmo_hash :: (Array Integer Integer) -> Integer
dmo_hash a = DL.foldl1 (\acc x -> hash(acc + hash(x))) [a!i | i <- [1..(snd $ bounds a)]]
    where hash x = (x * 2654435761) `mod` (2^32)
          
dmo_hash2 :: (Array Integer Integer) -> ByteString
dmo_hash2 a = SHA256.hash (BS.pack [fromIntegral(a!i) | i <- [1..(snd $ bounds a)]])
    
    
    
    
