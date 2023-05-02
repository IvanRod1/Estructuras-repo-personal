module Map
(Map,emptyM,assocM,lookupM,deleteM,keys)
where
data Map k v = M [(k,v)]

emptyM :: Map k v
assocM :: Eq k => k -> v -> Map k v -> Map k v
lookupM :: Eq k => k -> Map k v -> Maybe v
deleteM :: Eq k => k -> Map k v -> Map k v
keys :: Map k v -> [k]

emptyM = M []

assocM k v kvs = (k,v) : kvs

lookupM k [] = Nothing
lookupM k (kv:kvs) = if k == left(kv)
                     then Just (right kv)
                     else lookupM k kvs

deleteM k (kv:kvs) = if k == left(kv)
                     then kvs
                     else deleteM k kvs
keys [] = []
keys (kv:kvs) = left kv : keys kvs

{-data Map kv = M [k] [v]
  
  assocM :: Eq k -> k -> v -> Map k v -> Map k v
  
  assocM x y (M xs ys) = 
    let (xs1,ys1) = g x y xs ys
    in .....
    
g :: Eq k -> k -> v -> [k] -> [v] -> ([k],[v])
g x y [] _ =
g x y (x':xs)(y':ys) = if x == x'
                       then ((x':xs),(y : ys))         
                       else g x y xs ys -}


