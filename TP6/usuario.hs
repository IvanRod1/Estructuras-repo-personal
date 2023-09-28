import Map


map1 = assocM "Rodolfo" 2 (assocM "Ivan" 1 emptyM)


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
listToMap (kv:kvs) = assocM (fst kv) (snd kv) (listToMap kvs)

------------------------------------------------------------------------------------

mapToList :: Eq k => Map k v -> [(k, v)]
mapToList m = claveValorALista (keys m) m -- eficiencia ???

claveValorALista :: Eq k => [k] -> Map k v -> [(k,v)]
claveValorALista [] _ = []
claveValorALista (k:ks) m = (k,soloValor(lookupM k m)) : claveValorALista ks m -- constante * lineal

soloValor :: Maybe a -> a 
soloValor Nothing = error "No deberia estar aca" --constante
soloValor (Just a) = a

---------------------------------------------------------------------------------------

agruparEq :: Eq k => [(k, v)] -> Map k [v]
agruparEq [] = emptyM
agruparEq (kv:kvs) = if laClaveEstaRepetida (fst kv) kvs then agregarValor kv kvs else agruparEq kvs

laClaveEstaRepetida :: Eq k => k -> [(k,v)] -> Bool
laClaveEstaRepetida k [] = False 
laClaveEstaRepetida k (kv:kvs) = k == (fst kv) || laClaveEstaRepetida k kvs 

agregarValor :: (k,v) -> [(k,v)] -> Map k [v]
agregarValor kv [] = M [(fst kv , [snd kv])]
agregarValor kv1 (kv:kvs) = if fst (kv1) == fst (kv) then M (kv, (snd kv1) : (snd kv)) else 

