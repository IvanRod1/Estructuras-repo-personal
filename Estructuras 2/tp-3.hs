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

data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

camino = Nada (Cofre [Cacharro,Cacharro] (Nada (Cofre [Tesoro] Fin)))

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
cantTesorosEntre :: Int -> Int -> Camino -> Int

