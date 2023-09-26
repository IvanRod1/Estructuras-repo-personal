module Queue2
(Queue,emptyQ,isEmptyQ,enqueue,firstQ,dequeue,)
where

data Queue a = Q [a] deriving Show 

emptyQ :: Queue a
isEmptyQ :: Queue a -> Bool
enqueue :: a -> Queue a -> Queue a
firstQ :: Queue a -> a
dequeue :: Queue a -> Queue a

qInit = enqueue 1 emptyQ

emptyQ = Q []

-----------------------------------

isEmptyQ (Q xs) = null xs

-------------------------------------

enqueue x (Q ys) = Q (x:ys)

--------------------------------------

firstQ (Q (x:xs)) = if null xs then x else firstQ (Q xs)

-------------------------------------------------------

dequeue(Q (x:xs)) = if null xs then emptyQ else enqueue x (dequeue (Q xs)) 