module QueueV1
(Queue, emptyQ,isEmptyQ,enqueue,firstQ,dequeue)
where
data Queue a = Queue [a]

emptyQ :: Queue a
isEmptyQ :: Queue a -> Bool
enqueue :: a -> Queue a -> Queue a
firstQ :: Queue a -> a
dequeue :: Queue a -> Queue a

emptyQ = Queue []

isEmptyQ (Queue a) = null a

enqueue x (Queue a) = Queue (agregarAlFinal a x)

agregarAlFinal :: [a] -> a -> [a]          --
--agregarAlFinal [] b = b : [] 
agregarAlFinal a b = agregar a [b] 
                                                --- practica 2
agregar :: [a] -> [a] -> [a]               --
agregar [] b = b
agregar (x:xs) b = x : agregar xs b 

firstQ (Queue a) = elPrimero a

elPrimero :: [a] -> a
elPrimero (a:_) = a

dequeue (Queue a) = Queue (sinElPrimero a) 

sinElPrimero :: [a] -> [a]
sinElPrimero(_:a) = a