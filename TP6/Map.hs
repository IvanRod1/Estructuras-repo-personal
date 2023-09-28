module Map
(Map,emptyM,assocM,lookupM,deleteM,keys)
where
data Map k v = M [(k,v)] deriving Show


emptyM :: Map k v
assocM :: Eq k => k -> v -> Map k v -> Map k v
lookupM :: Eq k => k -> Map k v -> Maybe v
deleteM :: Eq k => k -> Map k v -> Map k v
keys :: Map k v -> [k]

map1 = M [("Ivan",1),("Rodolfo",2),("Rodriguez",3)]
emptyM = (M [])

-------------------------------------------

keys (M []) = []
keys (M (kv:kvs)) = fst kv : keys (M kvs) -- lineal

--------------------------------------------
--(k,v) : kvs
assocM k v (M []) = M [(k,v)]
assocM k v (M (kv:kvs)) = if k == (fst kv) then M ((k,v) : kvs) else agregarClaveValor kv (assocM k v (M kvs)) -- lineal

agregarClaveValor :: (k,v) -> Map k v -> Map k v 
agregarClaveValor x (M kvs) = M (x : kvs)
----------------------------------------------

lookupM k (M []) = Nothing
lookupM k (M (kv:kvs)) = if k == (fst kv) then Just (snd kv) else lookupM k (M kvs) --lineal

---------------------------------------------------------------------------

deleteM k (M (kv:kvs)) = if k == (fst kv) then (M kvs) else deleteM k (M (agregarAlFinal kvs kv)) -- lineal

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] y = [y]
agregarAlFinal (x:xs) y = if null xs then x : y : xs else x : agregarAlFinal xs y

----------------------------------------------------------------------------------------