module Heap
( createHeap
, addHeap
, popMin
, lenHeap
, minValue
, isEmpty
) where

data Heap a = Heap
	{ content :: [a]
	} deriving (Eq, Show)

createHeap :: (Ord a) => [a] -> Heap a
createHeap xs = heapify xs

heapify :: (Ord a) => [a] -> Heap a
heapify xs = foldl (\acc val -> addHeap acc val) (Heap []) xs


addHeap :: (Ord a) => Heap a -> a -> Heap a
addHeap h val =
	let xs = (content h) ++ [val]
	    in

	    upheap (Heap xs) ((length xs)-1)

popMin :: (Ord a) => Heap a -> (a, Heap a)
popMin (Heap []) = error "The heap is empty"
popMin (Heap [x]) = (x, Heap [])
popMin h@(Heap (x:xs)) =
	let lst = lenHeap h
	    in

	    (x, downheap (Heap (init (swap (content h) 0 (lst-1)))) 0)


lenHeap :: Heap a -> Int
lenHeap h = length (content h)

swap :: [a] -> Int -> Int -> [a]
swap h i j
    | (length h) == 0 = error "The heap is empty"
    | (i < 0) || (i >= (length h)) = error "The ith index is not in heap"
    | (j < 0) || (j >= (length h)) = error "The jth index is not in heap"
    | (j < i) = swap h j i
    | otherwise = 
    	(take i h) ++
    	[h !! j] ++
    	(drop (i+1) (take j h)) ++
    	[h !! i] ++
    	(drop (j+1) h)

upheap :: (Ord a) => Heap a -> Int -> Heap a
upheap h i
    | isEmpty h = error "The heap is empty"
    | (i < 0) || (i >= lenHeap h) = error "The index is out of bounds"
    | (i == 0) = h
    | otherwise =
    	let x = (content h) !! i
    	    j = parent i
    	    p = (content h) !! j 
    	    in
    	    if p > x
    	    then upheap (Heap (swap (content h) i j)) j
    	    else h


downheap :: (Ord a) => Heap a -> Int -> Heap a
downheap h i
    | isEmpty h = error "The heap is empty"
    | (i < 0) || (i >= lenHeap h) = error "The index is out of range"
    | ((left i) >= lenHeap h) = h
    | ((right i) >= lenHeap h) =
    	let cnt = content h in
    	    if cnt !! i > cnt !! (left i)
    	    then downheap (Heap (swap (content h) i (left i))) (left i)
    	    else h
    | otherwise =
    	let cnt = content h
    	    j = left i
    	    k = right i
    	    min_ind = if (cnt !! j) > (cnt !! k) then k else j 
    	    in

    	    if (cnt !! i) > (cnt !! min_ind)
    	    then downheap (Heap (swap (content h) i min_ind)) min_ind
    	    else h


parent :: Int -> Int
parent i = (i - 1) `div` 2

left :: Int -> Int
left i = 2 * i + 1

right :: Int -> Int
right i = 2 * i + 2

isEmpty :: Heap a -> Bool
isEmpty h = length (content h) == 0

minValue :: Heap a -> a
minValue h
    | isEmpty h = error "The heap is empty"
    | otherwise = (content h) !! 0