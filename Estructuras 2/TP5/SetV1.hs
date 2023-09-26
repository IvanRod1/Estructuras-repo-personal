module SetV1
(Set,emptyS,addS,belongs,sizeS,removeS, unionS, setToList) where

data Set a = Set [a] deriving Show

emptyS :: Set a
addS :: Eq a => a -> Set a -> Set a
belongs :: Eq a => a -> Set a -> Bool
sizeS :: Eq a => Set a -> Int
removeS :: Eq a => a -> Set a -> Set a
unionS :: Eq a => Set a -> Set a -> Set a
setToList :: Eq a => Set a -> [a]

emptyS = Set []

addS x (Set []) = Set [x]
addS x (Set ys) = Set (x : ys) -- constante?
----------------------------------------------------

belongs x (Set []) = False
belongs x (Set (y:ys)) = x == y || belongs x (Set ys)
-----------------------------------------------------------

sizeS (Set []) = 0
sizeS (Set (x:xs)) = 1 + sizeS (Set xs)
--------------------------------------------------------------------

removeS x (Set []) = (Set [])
removeS x (Set (y:ys)) = if x == y then (Set ys) else removeS x (Set (agregarAlFinal ys y))
------------------------------------------------------------------------------------------------------

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] y = [y]
agregarAlFinal (x:xs) y = if estaVacia xs then x : y : xs else x : agregarAlFinal xs y  


estaVacia :: [a] -> Bool    
estaVacia(x:xs) = False 
estaVacia _ = True

-------------------------------------------------------------------------------------------------------------

unionS (Set xs) (Set ys) = Set (xs++ys)

-----------------------------------------------------

setToList (Set []) = []
setToList (Set (x:xs)) = x : setToList (Set xs)

----------------------------------------------------------
