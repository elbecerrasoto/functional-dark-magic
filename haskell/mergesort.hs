-- Mergesort
merge :: (Ord a) => [a] -> [a]
merge [x] = [x]
merge xs = combine (merge right) (merge left)
    where right = fst (half_split xs)
          left  = snd (half_split xs)

-- split (recursion descent)
half_split :: [a] -> ([a], [a])
half_split xs   = (take ihalf xs, drop ihalf xs)
    where ihalf = (length xs) `div` 2

-- combine (recursion ascent)
combine :: (Ord a) => [a] -> [a] -> [a]
combine [] ys = ys
combine xs [] = xs
combine (x:xs) (y:ys)
    | x <= y    = x : combine xs     (y:ys)
    | otherwise = y : combine (x:xs) ys
