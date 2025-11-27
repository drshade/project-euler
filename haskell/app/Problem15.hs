module Problem15 where

import qualified Data.Map      as Map
import           Data.MemoTrie (memoFix)

type XY = (Int, Int)
type Lattice = Map.Map XY (XY, XY)

-- Build up the lattice structure Map (x,y) -> ((x,y), (x,y))
lattice :: Int -> Lattice
lattice size =
    Map.fromList [((x,y), ((x+1,y), (x,y+1))) | x <- [0..size-1], y <- [0..size-1]]

-- Count the pathways through the lattice, memoizing on the way
paths :: Lattice -> XY -> Int
paths lat = memoFix $ \rec xy ->
    maybe 1 (\(r,d) -> rec r + rec d) (Map.lookup xy lat)

answer :: Int
answer = paths (lattice 20) (0,0)
