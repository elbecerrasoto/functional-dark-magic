-- Mergesort
mergesort :: (Ord a) => [a] -> [a]
mergesort [x] = [x]
mergesort xs = combine (mergesort right) (mergesort left)
  where right = take ihalf xs
        left  = drop ihalf xs
        ihalf = (length xs) `div` 2

combine :: Ord (a) => [a] -> [a] -> [a]
combine xs [] = xs
combine [] ys = ys
combine (x:xs) (y:ys)
  | x <= y = x : combine    xs (y:ys)
  | x >  y = y : combine (x:xs)   ys
