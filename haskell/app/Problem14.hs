module Problem14 where

import qualified Data.List     as List
import qualified Data.Map      as Map
import           Data.MemoTrie (memoFix)

-- Can't be tail-recursive if we want to memo simply
next :: Int -> Int
next = memoFix $ \rec val ->
    case val of
        1 -> 0
        x -> 1 + if even x then rec (x `div` 2) else rec (3 * x + 1)

run :: Int -> (Int, Int)
run max =
    let map = Map.fromList $ (\start -> (start, next start)) <$> [1..max]
     in head $ List.sortBy (\(sx, x) (sy, y) -> compare y x) $ Map.toList map
