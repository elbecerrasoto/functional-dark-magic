-- Practice with recursion

-- Exercises

-- maximum
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list, you dummy!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- replicate
replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate' (n-1) x

-- take
take' :: Int -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

-- reverse
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

{-
-- repeat
-- Wrong, repeat is infinite!
repeat' :: Int -> a -> [a]
repeat' n _
    | n <= 0 = []
repeat' n x = x : repeat' (n-1) x
-}

-- repeat
repeat' :: a -> [a]
repeat' x = x:repeat' x

-- zip
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = [(x, y)] ++ zip' xs ys

-- elem
elem' :: (Eq a) => a -> [a] -> Bool
elem' x [] = False
elem' x (y:ys) = if x == y then x == y else elem' x ys

-- test pattern matching
testPM :: [a] -> [a]
testPM (x:xs) = xs
testPM [] = []
-- x:xs At least has x, x:[]
