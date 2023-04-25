module SetV1
(Set,emptyS,addS,belongs,sizeS,removeS, unionS,) where

data Set a = Set [a]
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

emptyS :: Set a 
addS :: Eq a => a -> Set a -> Set a 
belongs :: Eq a => a -> Set a -> Bool 
sizeS :: Eq a => Set a -> Int
removeS :: Eq a => a -> Set a -> Set a
unionS :: Eq a => Set a -> Set a -> Set a
--setToList :: Eq a => Set a -> [a]

emptyS = Set []

addS x (Set []) = Set [x]
addS x (Set xs) = Set (x : xs)

belongs x (Set []) = False
belongs x (Set (y:ys)) = x == y || belongs x (Set ys)

sizeS (Set xs) = longitud  xs

removeS x (Set (y:ys)) = if x == y
                         then Set ys
                         else removeS x (Set (agregarAlFinal ys y))

unionS (Set xs) (Set ys) = Set (xs ++ ys) 

--setTolist (Set xs) = xs

agregar :: [a] -> [a] -> [a]
agregar [] b = b
agregar (x:xs) b = x : agregar xs b

longitud :: [a] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

agregarAlFinal :: [a] -> a -> [a]
--agregarAlFinal [] b = b : []
agregarAlFinal a b = agregar a [b] 
