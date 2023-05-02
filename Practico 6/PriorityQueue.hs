module PriorityQueue
(PriorityQueue,emptyPQ,isEmptyPQ,insertPQ,findMinPQ,deleteMinPQ)
where
data PriorityQueue a = PQ [a]

emptyPQ :: PriorityQueue a
isEmptyPQ :: PriorityQueue a -> Bool
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
findMinPQ :: Ord a => PriorityQueue a -> a
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a

emptyPQ = PQ []
isEmptyPQ (PQ xs) = null xs
insertPQ e (PQ xs) = PQ (e:xs)
findMinPQ (PQ xs) = minimum xs
deleteMinPQ (PQ xs) = PQ (borrarMin xs)

borrarMin :: Ord a => [a] -> [a]
--Precondicion: La lista no esta vacia
borrarMin xs = borrar (minimum xs) xs

borrar :: Eq a => a -> [a] -> [a]
borrar x [] = []
borrar x (y:ys) = if x == y then ys else y : borrar x ys
