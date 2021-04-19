-- Negative Indexing
byNegi :: [a] -> Int -> a
byNegi xs idx
    | idx >= 0 = xs !! idx
    | nidx >= 0 = xs !! nidx
    | nidx < 0 = error "index out of bounds"
    where nidx = idx + length xs

-- Slicing a list
slice :: [a] -> Int -> Int -> [a]
slice xs start stop = take diff (drop start xs)
    where diff = stop - start
