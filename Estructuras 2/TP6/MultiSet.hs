import Map
module MultiSet
(MultiSet,emptyMS,addMS,ocurrencesMS,unionMS,intersectionMS,multiSetToList)
where

data MultiSet a = MS (Map a Int)

emptyMS :: MultiSet a

addMS :: Ord a => a -> MultiSet a -> MultiSet a

ocurrencesMS :: Ord a => a -> MultiSet a -> Int

unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a 

intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a 

multiSetToList :: MultiSet a -> [(a, Int)]


----------------------------------------------------------------------

emptyMS = MS (emptyM) -- O(1)

-------------------------------------------------------------------

{-addMS x (MS []) = MS [x]
addMS x (MS (y:ys)) = if x <= y then MS (x:y:ys) else addMS y (addMS x (MS ys))-}
addMS x (MS mp) = case lookup x mp of                           --O(lookup...) + O(assoc)
                  Just y -> MS (assocM x (y + 1) mp)
                  Nothing -> MS (assocM x 1 mp)

---------------------------------------------------------------------

{-ocurrencesMS x (MS []) = 0
ocurrencesMS x (MS (y:ys)) = if x == y then 1 + ocurrencesMS x (MS ys) else ocurrencesMS x (MS ys)-}
ocurrencesMS x (MS mp) = case lookup x mp of                           --O(lookup...)
                         Just y -> y
                         Nothing -> 0

---------------------------------------------------------------------

unionMS (MS []) (MS ys) = (MS ys)
unionMS (MS (x:xs)) (MS ys) = addMS x (unionMS (MS xs) (MS ys))

--------------------------------------------------------------------------

intersectionMS (MS []) _ = (MS []) 
intersectionMS (MS (x:xs)) (MS ys) = if pertenece x ys then addMS x (intersectionMS (MS (sacar x xs)) (MS (sacar x ys))) else intersectionMS (MS xs) (MS ys)

sacar :: Eq a => a -> [a] -> [a]
sacar _ [] = []                             --O(n)
sacar x (y:ys) = if x == y then sacar x ys else y : sacar x ys

pertenece :: Eq a => a -> [a] -> Bool
pertenece x [] = False                  --O(n)
pertenece x (y:ys) = x == y || pertenece x ys

-----------------------------------------------------------------------------------------------------------------
{-multiSetToList (MS []) = []
multiSetToList (MS (x:xs)) = (x,(apariciones x xs) + 1) : multiSetToList (MS (sacar x xs))-}

multiSetToList(MS mp) = mapToList mp



