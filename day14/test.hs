{-# LANGUAGE TupleSections #-}
import Control.Applicative
import Control.Arrow
import Data.Graph.Inductive


-- Example graph from SO question.
graph :: Gr (Maybe Int) ()
graph = mkGraph (map (id&&&Just) [1,2,3,4,5,6,7,8,9])
                (map (\(x,y) -> (x,y,())) $
                     concatMap gridNeighbors [1..9])
  where gridNeighbors n = map (n,)
                        . filter ((&&) <$> valid <*> not . boundary n)
                        $ [n-3,n-1,n+1,n+3]
        valid x = x > 0 && x < 10
        boundary n x = case n `rem` 3 of
                         0 -> x == n + 1
                         1 -> x == n - 1
                         _ -> False

-- <$> infix fmap
-- <*> infix Applicative
-- rem us like mod but handles negative values differently

-- Swap the labels of nodes 4 and 7
swapTest g = case match 4 g of
               (Just c4, g') -> case match 7 g' of
                                  (Just c7, g'') -> setLabel c4 (lab' c7) &
                                                    (setLabel c7 (lab' c4) &
                                                     g'')
                                  _ -> error "No node 7!"
               _ -> error "No node 4!"
  where setLabel :: Context a b -> a -> Context a b
        setLabel (inEdges, n, _, outEdges) l = (inEdges, n, l, outEdges)

-- :set -XTupleSections enabled use of (1,) 1 == (1,1)
gridNeighbors n = map (n,)
        . filter ((&&) <$> valid <*> not . boundary n)
        $ [n-128,n-1,n+1,n+128]
        where
      valid x = x > 0 && x < (128*128)
      boundary n x = case n `rem` 128 of
         0 -> x == n + 1
         1 -> x == n - 1
         _ -> False