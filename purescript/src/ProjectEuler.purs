
module ProjectEuler where

import Prelude

import Data.Array (catMaybes, foldr)
import Data.Array (reverse) as Array
import Data.BigInt (BigInt, fromInt, fromNumber, fromString, prime, rem, toNumber)
import Data.Foldable (sum, product, find)
import Data.Int (fromString, pow) as Data.Int
import Data.Lazy (Lazy, defer, force)
import Data.List (List(Nil), concatMap, filter, fromFoldable, head, index, length, range, reverse, sort, (:))
import Data.Maybe (fromMaybe)
import Data.Number (fromString) as Number
import Data.Number.Format (toStringWith, precision) as NumberFormat
import Data.String (joinWith, replaceAll, split, trim, take)
import Data.String.Pattern (Pattern(Pattern), Replacement(Replacement))
import Effect (Effect)
import Effect.Console (log)
import Data.Number (sqrt)


problem_13 :: Lazy String
problem_13 = defer \_ ->
    -- Basically just add them all up (as floating points),
    -- render as a string and then chop away the "." from 
    -- scientific notation - and return the first 10 chars
    take 10
        $ replaceAll (Pattern ".") (Replacement "")
        $ NumberFormat.toStringWith (NumberFormat.precision 10)
        $ foldr (+) 0.0
        $ catMaybes
        $ Number.fromString
            <$> trim
            <$> split (Pattern "\n")
            """
            37107287533902102798797998220837590246510135740250
            46376937677490009712648124896970078050417018260538
            74324986199524741059474233309513058123726617309629
            91942213363574161572522430563301811072406154908250
            23067588207539346171171980310421047513778063246676
            89261670696623633820136378418383684178734361726757
            28112879812849979408065481931592621691275889832738
            44274228917432520321923589422876796487670272189318
            47451445736001306439091167216856844588711603153276
            70386486105843025439939619828917593665686757934951
            62176457141856560629502157223196586755079324193331
            64906352462741904929101432445813822663347944758178
            92575867718337217661963751590579239728245598838407
            58203565325359399008402633568948830189458628227828
            80181199384826282014278194139940567587151170094390
            35398664372827112653829987240784473053190104293586
            86515506006295864861532075273371959191420517255829
            71693888707715466499115593487603532921714970056938
            54370070576826684624621495650076471787294438377604
            53282654108756828443191190634694037855217779295145
            36123272525000296071075082563815656710885258350721
            45876576172410976447339110607218265236877223636045
            17423706905851860660448207621209813287860733969412
            81142660418086830619328460811191061556940512689692
            51934325451728388641918047049293215058642563049483
            62467221648435076201727918039944693004732956340691
            15732444386908125794514089057706229429197107928209
            55037687525678773091862540744969844508330393682126
            18336384825330154686196124348767681297534375946515
            80386287592878490201521685554828717201219257766954
            78182833757993103614740356856449095527097864797581
            16726320100436897842553539920931837441497806860984
            48403098129077791799088218795327364475675590848030
            87086987551392711854517078544161852424320693150332
            59959406895756536782107074926966537676326235447210
            69793950679652694742597709739166693763042633987085
            41052684708299085211399427365734116182760315001271
            65378607361501080857009149939512557028198746004375
            35829035317434717326932123578154982629742552737307
            94953759765105305946966067683156574377167401875275
            88902802571733229619176668713819931811048770190271
            25267680276078003013678680992525463401061632866526
            36270218540497705585629946580636237993140746255962
            24074486908231174977792365466257246923322810917141
            91430288197103288597806669760892938638285025333403
            34413065578016127815921815005561868836468420090470
            23053081172816430487623791969842487255036638784583
            11487696932154902810424020138335124462181441773470
            63783299490636259666498587618221225225512486764533
            67720186971698544312419572409913959008952310058822
            95548255300263520781532296796249481641953868218774
            76085327132285723110424803456124867697064507995236
            37774242535411291684276865538926205024910326572967
            23701913275725675285653248258265463092207058596522
            29798860272258331913126375147341994889534765745501
            18495701454879288984856827726077713721403798879715
            38298203783031473527721580348144513491373226651381
            34829543829199918180278916522431027392251122869539
            40957953066405232632538044100059654939159879593635
            29746152185502371307642255121183693803580388584903
            41698116222072977186158236678424689157993532961922
            62467957194401269043877107275048102390895523597457
            23189706772547915061505504953922979530901129967519
            86188088225875314529584099251203829009407770775672
            11306739708304724483816533873502340845647058077308
            82959174767140363198008187129011875491310547126581
            97623331044818386269515456334926366572897563400500
            42846280183517070527831839425882145521227251250327
            55121603546981200581762165212827652751691296897789
            32238195734329339946437501907836945765883352399886
            75506164965184775180738168837861091527357929701337
            62177842752192623401942399639168044983993173312731
            32924185707147349566916674687634660915035914677504
            99518671430235219628894890102423325116913619626622
            73267460800591547471830798392868535206946944540724
            76841822524674417161514036427982273348055556214818
            97142617910342598647204516893989422179826088076852
            87783646182799346313767754307809363333018982642090
            10848802521674670883215120185883543223812876952786
            71329612474782464538636993009049310363619763878039
            62184073572399794223406235393808339651327408011116
            66627891981488087797941876876144230030984490851411
            60661826293682836764744779239180335110989069790714
            85786944089552990653640447425576083659976645795096
            66024396409905389607120198219976047599490197230297
            64913982680032973156037120041377903785566085089252
            16730939319872750275468906903707539413042652315011
            94809377245048795150954100921645863754710598436791
            78639167021187492431995700641917969777599028300699
            15368713711936614952811305876380278410754449733078
            40789923115535562561142322423255033685442488917353
            44889911501440648020369068063960672322193204149535
            41503128880339536053299340368006977710650566631954
            81234880673210146739058568557934581403627822703280
            82616570773948327592232845941706525094512325230608
            22918802058777319719839450180888072429661980811197
            77158542502016545090413245809786882778948721859617
            72107838435069186155435662884062257473692284509516
            20849603980134001723930671666823555245252804609722
            53503534226472524250874054075591789781264330331690
            """

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
        search a 1000 _ = search (a + 1) (a + 2) (a + 3)
        search 1000 _ _ = 0 -- exhausted our search... should find the solution before this - bug catcher
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
    log (show_correct true 13 problem_13 $ "5537376230")
