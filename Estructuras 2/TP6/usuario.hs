import Map


map1 = assocM "Rodolfo" 2 (assocM "Ivan" 1 emptyM)
map2 = assocM 100 2 (assocM 200 1 emptyM)

valuesM :: Eq k => Map k v -> [Maybe v]
valuesM m = valoresDe m (keys m)    --cuadratico

valoresDe :: Eq k =>  Map k v -> [k] -> [Maybe v]
valoresDe _ [] = []                                 --cuadratico
valoresDe m (k:ks) = lookupM k m : valoresDe m ks   

--------------------------------------------------------------------------------

todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas [] _ = True
todasAsociadas (k:ks) m = pertenece k (keys m) && todasAsociadas ks m -- cuadratica

pertenece :: Eq a => a -> [a] -> Bool
pertenece x [] = False
pertenece x (y:ys) = x == y || pertenece x ys  -- lineal

--------------------------------------------------------------------------------

listToMap :: Eq k => [(k, v)] -> Map k v
listToMap [] = emptyM
listToMap (kv:kvs) = assocM (fst kv) (snd kv) (listToMap kvs)       --O(n)

------------------------------------------------------------------------------------

mapToList :: Eq k => Map k v -> [(k, v)]
mapToList m = claveValorALista (keys m) m -- eficiencia O(n)

claveValorALista :: Eq k => [k] -> Map k v -> [(k,v)]
claveValorALista [] _ = []
claveValorALista (k:ks) m = (k,soloValor(lookupM k m)) : claveValorALista ks m -- constante * lineal

soloValor :: Maybe a -> a 
soloValor Nothing = error "No deberia estar aca" --constante
soloValor (Just a) = a

---------------------------------------------------------------------------------------

{-agruparEq :: Eq k => [(k, v)] -> Map k [v]
agruparEq [] = 
agruparEq (kv:kvs) = if laClaveEstaRepetida (fst kv) kvs then 

laClaveEstaRepetida :: Eq k => k -> [(k,v)] -> Bool
laClaveEstaRepetida k [] = False 
laClaveEstaRepetida k (kv:kvs) = k == (fst kv) || laClaveEstaRepetida k kvs 

agregarValor :: (k,v) -> [(k,v)] -> Map k [v]
agregarValor kv [] = M [(fst kv , [snd kv])]
agregarValor kv1 (kv:kvs) = if fst (kv1) == fst (kv) then  else -}

--------------------------------------------------------------------------------------------

incrementar :: Eq k => [k] -> Map k Int -> Map k Int
incrementar [] m = m
incrementar (k:ks) m = if existeClave k (lookupM k m) then assocM k (soloValor(lookupM k m) + 1) (incrementar ks m) else incrementar ks m

existeClave :: Eq k => k -> Maybe v -> Bool  --O(1)
existeClave k Nothing = False
existeClave k (Just _) = True 

----------------------------------------------------------------------------------------------

mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
{-Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
una clave del primero existe en el segundo, es reemplazada por la del primero.
-}
mergeMaps m1 m2 = listToMap(sinClavesRepetidas (mapToList m1) (mapToList m2)) -- O(n)(O(n)(O(n) + O(n)))

sinClavesRepetidas :: Eq k => [(k,v)] ->  [(k,v)] ->  [(k,v)]       --O(n)
sinClavesRepetidas [] kvs = kvs
sinClavesRepetidas (kv:kvs) (kv2:kvs2) = if (fst kv) == (fst kv2) then kv : sinClavesRepetidas kvs kvs2 else kv : kv2 : sinClavesRepetidas kvs kvs2

-------------------------------------------------------------------------------------------------
indexar :: [a] -> Map Int a
indexar [] = emptyM                                    --O(n) * O(n) 
indexar (x:xs) = assocM (length xs) x (indexar xs)

-----------------------------------------------------

ocurrencias :: String -> Map Char Int
ocurrencias [] = emptyM                                             --O(n) * O(n)
ocurrencias (x:xs) = assocM x ((cantOcurrencias x xs) + 1) (ocurrencias xs)


cantOcurrencias :: Eq a => a -> [a] -> Int
cantOcurrencias _ [] = 0                                                            -- O(n)
cantOcurrencias x (y:ys) = if x == y then 1 + cantOcurrencias x ys else cantOcurrencias x ys

---------------------------------------------------------------------------------------------------