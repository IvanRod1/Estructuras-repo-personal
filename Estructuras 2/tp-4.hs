data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show


data Ingrediente = Salsa| Queso | Jamon | Aceitunas Int deriving Show

pizza1 = Capa Salsa (Capa Queso (Capa (Aceitunas 3) Prepizza))

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p  

-------------------------------------------------------------------------

armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (x:xs) = Capa x (armarPizza xs)

--------------------------------------------------------------------------

sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa i p) = if esJamon i then sacarJamon p else Capa i (sacarJamon p)


esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _ = False 

---------------------------------------------------------------------------

tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = True 
tieneSoloSalsaYQueso (Capa i p) = (esSalsa i || esQueso i) &&  tieneSoloSalsaYQueso p 

esSalsa :: Ingrediente -> Bool
esSalsa Salsa = True
esSalsa _ = False

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _ = False 

------------------------------------------------------------------------------

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa i p) = if esAceituna i then Capa (multiplicarAceitunasPor i 2) (duplicarAceitunas p) else Capa i (duplicarAceitunas p)

esAceituna :: Ingrediente -> Bool
esAceituna (Aceitunas _) = True
esAceituna _ = False

multiplicarAceitunasPor :: Ingrediente -> Int -> Ingrediente
multiplicarAceitunasPor (Aceitunas x) y = Aceitunas (x * y)
multiplicarAceitunasPor _ _ = error "no es aceituna" 

----------------------------------------------------------------------------------------

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (x:xs) = (cantidadDeCapas x, x) : cantCapasPorPizza xs 

---------------------------------------------------------------------------------------------

data Dir = Izq | Der deriving Show
 
data Objeto = Tesoro | Chatarra  deriving Show

data Cofre = Cofre [Objeto] deriving Show

data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa deriving Show

mapa1 :: Mapa
mapa1 = Bifurcacion (Cofre [Chatarra,Chatarra]) (Fin (Cofre [Tesoro,Tesoro,Tesoro])) (Bifurcacion (Cofre [Chatarra,Chatarra]) (Fin (Cofre [Tesoro])) (Fin (Cofre [Chatarra])))

hayTesoro :: Mapa -> Bool
hayTesoro (Fin c) = esCofreConTesoro c
hayTesoro (Bifurcacion c m1 m2) =  esCofreConTesoro c  || hayTesoro m1  ||  hayTesoro m2

esCofreConTesoro :: Cofre -> Bool
esCofreConTesoro (Cofre obj) = hayAlgunTesoro obj
esCofreConTesoro _ = False

hayAlgunTesoro :: [Objeto] -> Bool
hayAlgunTesoro [] = False 
hayAlgunTesoro (x:xs) = esTesoro x || hayAlgunTesoro xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False 

-------------------------------------------------------------------------------------------------

hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] m = hayTesoro m
hayTesoroEn (x:xs) m = if esDerecha x then hayTesoroEn xs (caminoDerecho m) else hayTesoroEn xs (caminoIzquierdo m)

esDerecha :: Dir -> Bool
esDerecha Der = True
esDerecha _ = False

caminoDerecho :: Mapa -> Mapa
caminoDerecho (Fin c) = (Fin c) 
caminoDerecho (Bifurcacion  _ _ m2) = m2

caminoIzquierdo :: Mapa -> Mapa
caminoIzquierdo (Fin c) = (Fin c) 
caminoIzquierdo (Bifurcacion _ m1 _) = m1 


---------------------------------------------------------------------------------------------------

caminoAlTesoro :: Mapa -> [Dir]
--Precondicion: Debe existir solamente  un tesoro en el mapa
caminoAlTesoro (Fin c) = if esCofreConTesoro c then [] else error "Debe existir algun tesoro en el mapa"
caminoAlTesoro (Bifurcacion c m1 m2) = if hayTesoro m1 then Izq : caminoAlTesoro m1 else Der : caminoAlTesoro m2

------------------------------------------------------------------------------------------------------------------

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin _) = []
caminoDeLaRamaMasLarga (Bifurcacion c m1 m2) = if longitudDelCamino m1 > longitudDelCamino m2 then Izq : caminoDeLaRamaMasLarga m1 else Der : caminoDeLaRamaMasLarga m2


longitudDelCamino :: Mapa -> Int
longitudDelCamino (Fin _) = 0
longitudDelCamino (Bifurcacion c m1 m2) = 1 + longitudDelCamino m1 + longitudDelCamino m2

----------------------------------------------------------------------------------------------------------------------
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c) = if esCofreConTesoro c then [tesorosDelCofre c] else []
tesorosPorNivel (Bifurcacion c m1 m2) = if esCofreConTesoro c then  tesorosDelCofre c : tesorosPorNivel m1 ++ tesorosPorNivel m2 else tesorosPorNivel m1 ++ tesorosPorNivel m2

tesorosDelCofre :: Cofre -> [Objeto]
tesorosDelCofre (Cofre obj) = tesorosDeLaLista obj

tesorosDeLaLista :: [Objeto] -> [Objeto]
tesorosDeLaLista [] = []
tesorosDeLaLista (x:xs) = if esTesoro x then x : tesorosDeLaLista xs else tesorosDeLaLista xs 

---------------------------------------------------------------------------------------------------------------------------

todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin _) = []
todosLosCaminos (Bifurcacion c m1 m2) =  agregarALasListas Izq Der (todosLosCaminos m1 ++ todosLosCaminos m2)

