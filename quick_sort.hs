-- Quick Sort has a short implementation in Haskell
-- 

quick_sort :: (Ord a) => [a] -> [a]
quick_sort [] = []
quick_sort [x] = [x]
quick_sort (x:xs) = 
    (quick_sort (filter (<=x) xs)) ++
    [x] ++
    (quick_sort (filter (>x) xs))