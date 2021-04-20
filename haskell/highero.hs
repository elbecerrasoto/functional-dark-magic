-- Learn you a Haskell for Great Good!
-- Chapter 05
-- Higher Order Functions

-- 01. Find the largest number under 100,000 that's divisible by 3,829
divisibleBy :: Int -> Int -> Bool
divisibleBy n x
  | x `mod` n == 0 = True
  | otherwise      = False

exercise01 = maximum (filter (divisibleBy 3829) [1,2..100000])

-- Book's Answer
largestDivisible :: Integer
largestDivisible = head (filter p [100000, 99999..])
  where p x = x `mod` 3829 == 0

-- 02. Sum of all odd squares that are smaller than 10,000

sumOfOddSquares :: Integer
sumOfOddSquares = sum ( filter p (map (^2) [1,2..10000]) )
  where p n = odd n && n < 10000

exercise02 = sumOfOddSquares

-- Book's Answer
exerciseB02 = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

-- 03. The same but with list comprehensions
exercise03 = sum (takeWhile (<10000) [f x | x <- [1..], odd (f x)])
  where f = (^2)

-- Book's Answer
exerciseB03 = sum (takeWhile (<10000) [m | m <- [n^2 | n <- [1..]], odd m])

-- Collatz Sequence
-- 1. Start with any natural number
-- 2. If the number is 1, stop.
-- 3. If the number is even, divide by 2.
-- 4. If the number is odd, multiply by 3 and add 1.
-- 5. Repeat over.

-- 04. For all starting numbers between 1 and 100, how many Collatz chains have a lenght greater that 15?

collatzChain :: Integer -> [Integer]
collatzChain 1 = [1]
collatzChain n
  | even n = n : collatzChain (n `div` 2)
  | odd  n = n : collatzChain (3*n + 1)


exercise04 = length (filter (>15) (map length (map collatzChain [1..100])))

-- Book's Answer
chain :: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n = n:chain (n `div` 2)
  | odd  n = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15

exerciseB04 = numLongChains


-- Stuff
identity :: a -> a
identity x = x

-- Recursive Maximum, as opposed to two fingers algorithm
maximum' :: Ord a => [a] -> a
maximum' [] = error "Undefined"
maximum' [x] = x
maximum' (x:xs)
  | x > maximum' xs = x
  | otherwise       = maximum' xs

sum' :: Num a => [a] -> a
sum' [] = error "Undefined"
sum' [x] = x
sum' (x:xs) = x + (sum' xs)
