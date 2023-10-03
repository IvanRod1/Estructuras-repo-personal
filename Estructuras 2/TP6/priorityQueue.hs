module PriorityQueue
(PriorityQueue,emptyPQ,isEmptyPQ,insertPQ,findMinPQ,deleteMinPQ)
where

data PriorityQueue a = PQ [a] deriving Show 

emptyPQ :: PriorityQueue a
isEmptyPQ :: PriorityQueue a -> Bool
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
findMinPQ :: Ord a => PriorityQueue a -> a
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a

pq1 = PQ [1,2,10,4]
emptyPQ = PQ [] -- constante

-------------------------------------------

isEmptyPQ (PQ xs) = null xs --constante

-------------------------------------------
findMinPQ (PQ xs) = minimum xs  -- lineal

--------------------------------------------------------------
insertPQ x (PQ []) = PQ [x]
insertPQ x (PQ (y:ys)) = if x <= y then PQ (x:y:ys) else insertPQ y (insertPQ x (PQ ys)) -- lineal

--------------------------------------------------------------------------------

deleteMinPQ (PQ []) = PQ []
deleteMinPQ (PQ (x:xs)) = if x < (minimum xs) then PQ xs else deleteMinPQ (PQ xs) --lineal