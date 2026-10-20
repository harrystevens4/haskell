module Main where

main :: IO ()
main = putStr . showItemsNewline $ generatePrimes 2 []

isPrime :: Integer -> [Integer] -> Bool
isPrime guess sieve = not $ any id $ sieve >>= \x -> return (guess `mod` x == 0)

generatePrimes :: Integer -> [Integer] -> [Integer] 
generatePrimes guess sieve
    | isPrime guess sieve = guess : generatePrimes (guess + 1) (guess : sieve)
    | otherwise           = generatePrimes (guess + 1) sieve

showItemsNewline :: Show a => [a] -> String
showItemsNewline (x:xs) = (show x) ++ '\n' : showItemsNewline xs

-- >>> isPrime 9 [7,5,3,2]
-- False

-- >>> expandSieve [7,5,3,2]
-- [11,7,5,3,2]
    
-- >>> take 20 primes
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71]

-- >>> reverse $ iterateN 50 [] expandSieve
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229]
