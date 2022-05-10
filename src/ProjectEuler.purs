
module ProjectEuler where

import Prelude

import Data.Array (reverse) as Array
import Data.BigInt (BigInt, fromInt, fromNumber, fromString, prime, rem, toNumber)
import Data.Foldable (sum, product, find)
import Data.Int (fromString, pow) as Data.Int
import Data.Lazy (Lazy, defer, force)
import Data.List (List(Nil), concatMap, filter, fromFoldable, head, index, length, range, reverse, sort, (:))
import Data.Maybe (fromMaybe)
import Data.String (joinWith, replaceAll, split)
import Data.String.Pattern (Pattern(Pattern), Replacement(Replacement))
import Effect (Effect)
import Effect.Console (log)
import Math (sqrt)

problem_12 :: Lazy Int
problem_12 = defer \_ ->
    let triangle' :: Int -> Int -> Int
        triangle' acc x
            | x <= 0    = acc
            | otherwise = triangle' (acc + x) (x - 1)
        triangle = triangle' 0

        -- The secret to making this optimal is the resetting of a max as we discover new divisors
        -- (i.e. we know both products, and we can max out at the second product)
        divisors' :: Int -> List Int -> Int -> Int -> List Int
        divisors' x acc cur max 
            | cur >= max        = acc
            | x `mod` cur == 0  = divisors' x (cur : x `div` cur : acc) (cur + 1) (x `div` cur)
            | otherwise         = divisors' x acc (cur + 1) max
        divisors x = divisors' x Nil 1 x

        find' :: Int -> Int -> Int
        find' target depth
            | (length $ divisors $ triangle depth) > target = triangle depth
            | otherwise                                     = find' target (depth + 1)
        find x = find' x 0
    in find 500

problem_11 :: Lazy Int
problem_11 = defer \_ ->
    let grid :: List Int
        grid = """
            08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
            49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
            81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
            52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
            22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
            24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
            32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
            67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
            24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
            21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
            78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
            16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
            86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
            19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
            04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
            88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
            04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
            20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
            20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
            01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48
        """
            # replaceAll (Pattern "\n") (Replacement "")
            # replaceAll (Pattern " ") (Replacement "")
            # split (Pattern "")
            # fromFoldable
            # (\list -> 
                let
                    digitify (x1 : x2 : xs) accum = 
                        digitify xs (digit : accum)
                        where
                            digit = parseInt $ x1 <> x2
                            parseInt = Data.Int.fromString >>> fromMaybe 0
                    digitify _ accum = reverse accum
                in
                digitify list Nil
            )
        -- Look down the grid
        down :: Int -> List Int
        down idx = (0 : 20 : 40 : 60 : Nil) 
            # map (\offset -> index grid (idx + offset) # fromMaybe 0)
        -- Look right across the grid
        right :: Int -> List Int
        right idx = (0 : 1 : 2 : 3 : Nil) 
            # map (\offset -> 
                if idx `mod` 20 <= 16 then index grid (idx + offset) # fromMaybe 0
                else 0
            )
        -- Look diagonally down and left
        diag_left :: Int -> List Int
        diag_left idx = (0 : 19 : 38 : 57 : Nil)
            # map (\offset -> 
                if idx >= 3 then index grid (idx + offset) # fromMaybe 0
                else 0
            )
        -- Look diagonally down and right
        diag_right :: Int -> List Int
        diag_right idx = (0 : 21 : 42 : 63 : Nil)
            # map (\offset -> 
                if idx <= 16 then index grid (idx + offset) # fromMaybe 0
                else 0
            )
        -- Calculate the maximum product from any direction
        max_product idx = 
            let
                down_value = down idx # product
                right_value = right idx # product
                diag_left_value = diag_left idx # product
                diag_right_value = diag_right idx # product
            in
                down_value `max` right_value `max` diag_left_value `max` diag_right_value
        -- Search from each index in the grid
        find_max_product :: Int -> Int -> Int
        find_max_product max_so_far idx
            | idx >= length grid = max_so_far
            | max_product idx > max_so_far = find_max_product (max_product idx) (idx + 1)
            | otherwise = find_max_product max_so_far (idx + 1)
    in find_max_product 0 0

problem_10 :: Lazy BigInt
problem_10 = defer \_ ->
    let sum_primes_to :: Int -> BigInt -> BigInt -> BigInt
        sum_primes_to max next accum 
            | next > (fromInt max) = accum
            | prime next = sum_primes_to max (add next (fromInt 1)) (add accum next)
            | otherwise  = sum_primes_to max (add next (fromInt 1)) accum
    in sum_primes_to 1999999 (fromInt 2) (fromInt 0)

problem_9 :: Lazy Int
problem_9 = defer \_ -> 
    let -- how hacky is this! but i'm pretty proud of it
        search a b 1000 = search a (b + 1) (b + 2)
        search a 1000 c = search (a + 1) (a + 2) (a + 3)
        search 1000 b c = 0 -- exhausted our search... should find the solution before this - bug catcher
        search a b c
            | a + b + c == 1000 && (a `Data.Int.pow` 2) + (b `Data.Int.pow` 2) == (c `Data.Int.pow` 2) = a * b * c
            | otherwise = search a b (c + 1)
    in search 1 2 3

problem_8 :: Lazy BigInt
problem_8 = defer \_ -> 
    find_max_product (fromInt 0) input_pad
    where
        input_pad :: List Int
        input_pad = """
            73167176531330624919225119674426574742355349194934
            96983520312774506326239578318016984801869478851843
            85861560789112949495459501737958331952853208805511
            12540698747158523863050715693290963295227443043557
            66896648950445244523161731856403098711121722383113
            62229893423380308135336276614282806444486645238749
            30358907296290491560440772390713810515859307960866
            70172427121883998797908792274921901699720888093776
            65727333001053367881220235421809751254540594752243
            52584907711670556013604839586446706324415722155397
            53697817977846174064955149290862569321978468622482
            83972241375657056057490261407972968652414535100474
            82166370484403199890008895243450658541227588666881
            16427171479924442928230863465674813919123162824586
            17866458359124566529476545682848912883142607690042
            24219022671055626321111109370544217506941658960408
            07198403850962455444362981230987879927244284909188
            84580156166097919133875499200524063689912560717606
            05886116467109405077541002256983155200055935729725
            71636269561882670428252483600823257530420752963450
        """
            # replaceAll (Pattern "\n") (Replacement "")
            # replaceAll (Pattern " ") (Replacement "")
            # split (Pattern "")
            # map (\i -> Data.Int.fromString i # fromMaybe 0)
            # fromFoldable
        
        product :: Int -> List Int -> BigInt
        product _ (Nil) = fromInt 1
        product max_elements (x : xs)
            | max_elements == 1 = fromInt x
            | otherwise = (fromInt x) * (product (max_elements - 1) xs)

        find_max_product :: BigInt -> List Int -> BigInt
        find_max_product max_so_far list@(_ : xs)
            | product 13 list > max_so_far = find_max_product (product 13 list) xs
            | otherwise = find_max_product (max_so_far) xs
        find_max_product max_so_far _ = max_so_far

problem_7 :: Lazy BigInt
problem_7 = defer \_ ->
    primes_to 10001 (fromInt 2) 0
    where
        -- Using BigInt because it has "prime" function for checking if prime
        primes_to :: Int -> BigInt -> Int -> BigInt
        primes_to max next count 
            | prime next && (count + 1) == max = next
            | prime next = primes_to max (add next (fromInt 1)) (count + 1)
            | otherwise  = primes_to max (add next (fromInt 1)) (count)

problem_6 :: Lazy Int
problem_6 = defer \_ -> 
    square_of_sum - sum_of_squares
    where
        sum_of_squares = range 1 100 # map (\e -> e `Data.Int.pow` 2) # sum
        square_of_sum  = range 1 100 # sum # (\e -> e `Data.Int.pow` 2)

problem_5 :: Lazy Int
problem_5 = defer \_ -> 
    let -- search until we find a value which is divisible by all the values
        search_from value
            -- SLOW! -> | range 20 11 # map (\d -> value `mod` d == 0) # all identity = value
            | value `mod` 20 == 0 && value `mod` 19 == 0 && value `mod` 18 == 0
                && value `mod` 17 == 0 && value `mod` 16 == 0 && value `mod` 15 == 0
                && value `mod` 14 == 0 && value `mod` 13 == 0 && value `mod` 12 == 0
                && value `mod` 11 == 0
                = value
            | otherwise = search_from (value + 1)
    in search_from 1

problem_4 :: Lazy Int
problem_4 = defer \_ -> 
    let -- Is a number a palindrome?
        is_palindrome :: Int -> Boolean
        is_palindrome num =
            str_num == str_num_reversed
            where
                str_num = show num
                str_num_reversed = str_num # split (Pattern "") # Array.reverse # joinWith ""

        -- Calculate all the products of 3 digit numbers
        products :: List Int
        products = 
            three_digit_numbers # concatMap (\x -> three_digit_numbers # map (\y -> x * y))
            where three_digit_numbers = range 100 999
    in products # filter is_palindrome # sort # reverse # head # fromMaybe 0

-- Def some initial issues:
--  we will overflow the int type with target value 600851475143
--  so we need to use BigInt package
--  testing for prime means trying every value upto sqrt of the target
--  as a divisor
problem_3 :: Lazy BigInt
problem_3 = defer \_ -> 
    let -- This is what we need to prime factor
        target = fromString "600851475143" # fromMaybe (fromInt 0)

        -- Calculate the prime numbers between start and end value
        primes_upto :: BigInt -> BigInt -> List BigInt -> List BigInt
        primes_upto next end accum 
            | next >= end   = accum
            | prime next    = primes_upto (add next (fromInt 1)) end (next : accum)
            | otherwise     = primes_upto (add next (fromInt 1)) end (accum)

        -- Calculate the sqrt of a bigint - by cheating and using the Number implementation
        bigint_sqrt :: BigInt -> BigInt
        bigint_sqrt num = toNumber num # sqrt # fromNumber # fromMaybe num
    in primes_upto (fromInt 1) (target `div` (bigint_sqrt target)) Nil
            # find (\prime -> target `rem` prime == (fromInt 0))
            # fromMaybe (fromInt 0)

problem_2 :: Lazy Int
problem_2 = defer \_ -> 
    let is_even e = e `mod` 2 == 0
        fib_terms_upto :: Int -> List Int -> List Int
        fib_terms_upto limit accum@(x1 : x2 : _) 
            | (x1 <= limit) = fib_terms_upto limit ((x1 + x2) : accum)
        fib_terms_upto _ accum = accum
    in fib_terms_upto 4000000 (2 : 1 : Nil)
            # filter is_even
            # sum

problem_1 :: Lazy Int
problem_1 = defer \_ -> 
    -- Problem says "all multiples below 1000" and range is inclusive
    range 1 999
        # filter (\e -> e `is_multiple_of` 3 || e `is_multiple_of` 5)
        # sum
    where
        is_multiple_of left right = left `mod` right == 0

-- String formatter for printing answers
show_correct :: forall a. Show a => Eq a => Boolean -> Int -> Lazy a -> a -> String
show_correct false problem_number _ correct = 
    "problem #" <> show problem_number <> " -> SKIPPING! (correct answer is " <> show correct <> ")"
show_correct true problem_number lazy_guess correct =
    "problem #" <> show problem_number <> " -> " <> show guess <> " - " <> if guess == correct then "CORRECT!" else "INCORRECT"
    where guess = force $ lazy_guess

main :: Effect Unit
main = do
    log "Project Euler Solutions by Tom (problems at https://projecteuler.net/show=all)"
    log (show_correct true 1  problem_1  $ 233168)
    log (show_correct true 2  problem_2  $ 4613732)
    log (show_correct true 3  problem_3  $ fromInt 6857)
    log (show_correct true 4  problem_4  $ 906609)
    log (show_correct true 5  problem_5  $ 232792560)
    log (show_correct true 6  problem_6  $ 25164150)
    log (show_correct true 7  problem_7  $ fromInt 104743)
    log (show_correct true 8  problem_8  $ fromString "23514624000" # fromMaybe (fromInt 0))
    log (show_correct true 9  problem_9  $ 31875000)
    log (show_correct true 10 problem_10 $ fromString "142913828922" # fromMaybe (fromInt 0))
    log (show_correct true 11 problem_11 $ 70600674)
    log (show_correct true 12 problem_12 $ 76576500)