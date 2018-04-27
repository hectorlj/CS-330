import Data.Array
-- Type Signature
isPrime :: Int -> Bool
isPrime a = null [b | b <- [2..(iSqrt a)], isMod0 a b]

iSqrt :: Int -> Int
iSqrt a = floor(sqrt(fromIntegral a))

isMod0 ::Int -> Int -> Bool
isMod0 a b = if (a `mod` b) == 0
    then True 
    else False

--Type Signature
primes :: [Int]
primes = filter isPrime [2..]

isPrimeFast :: Int -> Bool
isPrimeFast a
    | a == 2 = True
    | otherwise = null [b | b <- (takeWhile(<= (iSqrt a)) primesFast), isMod0 a b]

primesFast :: [Int]
primesFast = filter isPrimeFast [2..]

lcsLength :: String -> String -> Int
lcsLength string1 string2  = a ! (0,0) where
    n = length string1
    m = length string2
    a = array ((0,0), (n,m)) (a1 ++ a2 ++ a3)
    a1 = [((i,m), 0) | i <- [0..n]]
    a2 = [((n,j), 0) | j <- [0..m]]
    a3 = [((i,j), fun char1 char2 i j) | (char1,i) <- zip string1 [0..], (char2,j) <- zip string2 [0..]]
    fun char1 char2 i j
        | char1 == char2 = 1 + (a!(i+1,j+1))
        | otherwise = max (a!(i,j+1)) (a!(i+1,j))