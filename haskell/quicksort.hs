-- Quicksort
quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = (quicksort' low) ++ [x] ++ (quicksort' high)
    where low  = fst (against_pivot xs x)
          high = snd (against_pivot xs x)

against_pivot :: (Ord a) => [a] -> a -> ([a], [a])
against_pivot [] pivot = ([], [])
against_pivot (x:xs) pivot
    | x <= pivot = (x : fst (against_pivot xs pivot),     snd (against_pivot xs pivot))
    | x > pivot =      (fst (against_pivot xs pivot), x : snd (against_pivot xs pivot))

-- Book's Answer
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerOrEqual = filter (<= x) xs
      larger         = filter (>  x) xs
  in quicksort smallerOrEqual ++ [x] ++ quicksort larger
