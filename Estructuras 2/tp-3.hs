data Color = Azul | Rojo  deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show

nroBolitas :: Color -> Celda -> Int
nroBolitas _ CeldaVacia = 0
nroBolitas c (Bolita x cel) = if sonIguales c x then 1 + nroBolitas c cel else nroBolitas c cel 


sonIguales :: Color -> Color -> Bool
sonIguales Azul Azul = True
sonIguales Rojo Rojo = True
sonIguales _ _ = False 

var = Bolita Azul (Bolita Azul (Bolita Rojo CeldaVacia))

------------------------------------------------------------------------------------------------------------------------------------
poner :: Color -> Celda -> Celda
poner c CeldaVacia = Bolita c CeldaVacia
poner c celda = Bolita c celda

-----------------------------------------

sacar :: Color -> Celda -> Celda
sacar c CeldaVacia = CeldaVacia
sacar c (Bolita x cel) = if sonIguales c x then cel else Bolita x (sacar c cel)

------------------------------------------------------------------------------------------------
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ celda = celda 
ponerN x c celda = Bolita c (ponerN (x - 1) c celda)

-----------------------------------------------------------------------------------------

data Objeto = Cacharro | Tesoro deriving Show 
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino deriving Show

camino = Nada (Cofre [Cacharro,Cacharro] (Nada (Cofre [Tesoro] Fin)))
camino2 = Fin 

-- (Nada (Cofre [Tesoro] Fin)
hayTesoro :: Camino -> Bool
hayTesoro Fin = False 
hayTesoro (Cofre obj c )= hayObjetoValioso obj || hayTesoro c
hayTesoro (Nada c) = hayTesoro c 

hayObjetoValioso :: [Objeto] -> Bool
hayObjetoValioso  [] = False 
hayObjetoValioso (x:xs) = esTesoro x || hayObjetoValioso xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False 

-----------------------------------------------------------------------------------------
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin = error "debe haber algun tesoro"
pasosHastaTesoro (Nada c) = 1 + pasosHastaTesoro c 
pasosHastaTesoro (Cofre obj c) = if hayObjetoValioso obj then 0 else 1 + pasosHastaTesoro c 

---------------------------------------------------------------------------------------
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn _ Fin = False 
hayTesoroEn x (Nada c) = if x > 0 then hayTesoroEn (x-1) c else False 
hayTesoroEn x (Cofre obj c) = if x > 0 then hayTesoroEn (x-1) c else hayObjetoValioso obj 

-----------------------------------------------------------------------------------------------
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros x c = cantidadDeTesorosEnCamino c >= x 

cantidadDeTesorosEnCamino :: Camino -> Int
cantidadDeTesorosEnCamino Fin = 0
cantidadDeTesorosEnCamino (Nada c) = cantidadDeTesorosEnCamino c 
cantidadDeTesorosEnCamino (Cofre obj c) = if hayObjetoValioso obj then 1 + cantidadDeTesorosEnCamino c else cantidadDeTesorosEnCamino c

----------------------------------------------------------------------------------------------------
camino3 = Nada (Cofre [Tesoro](Nada (Cofre [Tesoro] (Nada (Cofre [Tesoro] Fin)))))
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre x y c = cantidadDeTesorosHaciendo_Pasos (y - x) (avanzar_Pasos x c)

-- (avanzar_Pasos x c)
avanzar_Pasos :: Int -> Camino -> Camino
avanzar_Pasos x c = if x > 0 && not (esFin c) then avanzar_Pasos (x - 1) (detectorCamino c) else c  

detectorCamino :: Camino -> Camino
detectorCamino Fin = Fin
detectorCamino (Cofre _ c) = c
detectorCamino (Nada c) = c

esFin :: Camino -> Bool
esFin Fin = True
esFin _ = False 

cantidadDeTesorosHaciendo_Pasos :: Int -> Camino -> Int 
cantidadDeTesorosHaciendo_Pasos x Fin = 0 
cantidadDeTesorosHaciendo_Pasos x (Cofre obj c) = if hayObjetoValioso obj && x >= 0 then 1 + cantidadDeTesorosHaciendo_Pasos (x - 1) c else cantidadDeTesorosHaciendo_Pasos (x - 1) c
cantidadDeTesorosHaciendo_Pasos x (Nada c) = cantidadDeTesorosHaciendo_Pasos (x - 1) c



-------------------------------------------------------------------------------------------------------------------------------

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

arbol1 :: Tree Int 
arbol1 = NodeT 1 (NodeT 2 EmptyT EmptyT) (NodeT 5 (NodeT 20 EmptyT EmptyT) EmptyT)

sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT x t1 t2) = x + sumarT t1 + sumarT t2

--------------------------------------------------------

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT _ t1 t2) =  1 + sizeT t1 + sizeT t2

--------------------------------------------------------
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT x t1 t2) = NodeT (x * 2) (mapDobleT t1) (mapDobleT t2)

--------------------------------------------------------

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT x EmptyT = False
perteneceT x (NodeT y t1 t2) = x == y || perteneceT x t1 || perteneceT x t2

----------------------------------------------------------------------------------

leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT x t1 t2) = if esArbolVacio t1 && esArbolVacio t2 then [x] else leaves t1 ++ leaves t2

esArbolVacio :: Tree a -> Bool
esArbolVacio EmptyT = True
esArbolVacio _ = False

-----------------------------------------------------------------------------------

heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT x t1 t2) = if esArbolVacio t1 && esArbolVacio t2 then 1 + heightT t1 + heightT t2 else heightT t1 + heightT t2

------------------------------------------------------------------------------------
mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT x t1 t2) = NodeT x (mirrorT t2) (mirrorT t1)

----------------------------------------------------------------------------------

toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT x t1 t2) = toList t1 ++ [x] ++ toList t2

----------------------------------------------------------------------------------

levelN :: Int -> Tree a -> [a]
levelN _ EmptyT = []
--levelN 0 (NodeT y _ _ ) = [y]
--levelN x (NodeT y t1 t2) = if x > 0 then levelN (x - 1) t1 ++ levelN (x - 1) t2 else y : levelN x t1 ++ levelN x t2
levelN  x (NodeT y t1 t2) = if x == 0 then [y] else levelN (x - 1) t1 ++ levelN (x - 1) t2



todosloscaminos(Nodet x t1 t2) = agregarA x (todosLosCaminos t1 ++ todosLosCaminos t2)
