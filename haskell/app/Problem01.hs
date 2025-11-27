module Problem01 where

problem_1 :: Int -> Int
problem_1 = (map run [0..] !!)
    -- Problem says "all multiples below 1000" and range is inclusive
    where
        run 0 = 0
        run n =
            if (n `is_multiple_of` 3 || n `is_multiple_of` 5)
                then n + problem_1 (n - 1)
                else problem_1 (n - 1)
        is_multiple_of left right = left `mod` right == 0

problem_1' :: Int -> Int
problem_1' = (map stuff [0..] !!)
    where
        stuff n = sum $ filter (\e -> e `is_multiple_of` 3 || e `is_multiple_of` 5) $ [1..n-1]
        is_multiple_of left right = left `mod` right == 0

