sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs
----------------------------------------

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

----------------------------------------

sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x:xs) = (x + 1) : sucesores xs

-------------------------------------------

conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (x:xs) = x && conjuncion xs

--------------------------------------------

disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (x:xs) = x || disyuncion xs

---------------------------------------------
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs

---------------------------------------------

pertenece :: Eq a => a -> [a] -> Bool
pertenece x [] = False
pertenece x (y:ys) = x == y || pertenece x ys

---------------------------------------------

apariciones :: Eq a => a -> [a] -> Int
apariciones x [] = 0
apariciones x (y:ys) = if x == y then 1 + apariciones x ys else apariciones x ys

---------------------------------------------

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA x [] = []
losMenoresA x (y:ys) = if x > y then y : losMenoresA x ys else losMenoresA x ys

----------------------------------------------

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA x [] = []
lasDeLongitudMayorA x (y:ys) = if longitud y > x then y : lasDeLongitudMayorA x ys else lasDeLongitudMayorA x ys

-----------------------------------------------

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] y = [y]
agregarAlFinal x y = x ++ [y]

--------------------------------------------------

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = agregarAlFinal (reversa xs) x 

---------------------------------------------------

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] [] = []
zipMaximos x  [] = x
zipMaximos []  y = y
zipMaximos (x:xs)(y:ys) = if x > y then x : zipMaximos xs ys else y : zipMaximos xs ys


------------------------------------------------------------------------------------------

factorial :: Int -> Int
factorial 0 = 1
factorial x =  x  * factorial (x - 1)

----------------------------------------------------------

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva x = x : cuentaRegresiva (x - 1)

-----------------------------------------------------------

repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir x y = y : repetir (x - 1) y

----------------------------------------------------------

losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _ = []
losPrimeros x (y:ys) = y : losPrimeros (x - 1) ys

------------------------------------------------------------

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 ls = ls
sinLosPrimeros x (y:ys) = sinLosPrimeros (x - 1) ys

---------------------------------------------------------------

data Persona = P String Int deriving Show
carlo = P "Carlo" 45
masi = P "Masi" 1000
edad :: Persona -> Int
edad (P _ e) = e


mayoresA :: Int -> [Persona] -> [Persona]
mayoresA x [] = []
mayoresA x (y:ys) = if (edad y)> x then y : mayoresA x ys else mayoresA x ys

------------------------------------------------------------------

promedioEdad :: [Persona] -> Int
promedioEdad [] = error "Debe haber almenos una persona"
promedioEdad xs = div (sumaDeEdades xs) (longitud xs)


sumaDeEdades :: [Persona] -> Int
sumaDeEdades [] = 0
sumaDeEdades (x:xs) = edad x + sumaDeEdades xs


-------------------------------------------------------------------

elMasViejo :: [Persona] -> Persona
elMasViejo [] = error "Debe haber almenos una persona"
elMasViejo [x] = x
elMasViejo (x:xs) = maximaEdadEntre x (elMasViejo xs)


maximaEdadEntre :: Persona -> Persona -> Persona
maximaEdadEntre p1 p2 = if edad p1 > edad p2 
                        then p1 
                        else p2
----------------------------------------------------------------------

data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]

gyarados = ConsPokemon Agua 10000
flareon = ConsPokemon Fuego 7800
venasaur = ConsPokemon Planta 7600

entrenador = ConsEntrenador "Kal" [flareon,venasaur]


cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador _ []) = 0 
cantPokemon (ConsEntrenador _ xs) = longitud xs


-------------------------------------------------------------------

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t (ConsEntrenador _ []) = 0
cantPokemonDe t (ConsEntrenador _ xs) =  cantidadPokemonDeTipo_ t xs 



igualdadDeTipos :: TipoDePokemon -> TipoDePokemon -> Bool
igualdadDeTipos Agua Agua = True
igualdadDeTipos Fuego Fuego = True
igualdadDeTipos Planta Planta = True
igualdadDeTipos _ _ = False

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino _ = 0

tipo :: Pokemon -> TipoDePokemon
tipo (ConsPokemon t _) = t 

cantidadPokemonDeTipo_ :: TipoDePokemon -> [Pokemon] -> Int
cantidadPokemonDeTipo_ t [] = 0
cantidadPokemonDeTipo_ t (x:xs) = unoSiCeroSino (igualdadDeTipos t (tipo x)) + cantidadPokemonDeTipo_ t xs

---------------------------------------------------------------------------------------------------------------

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador _ []) (ConsEntrenador _ ys) = 
cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador _ xs) (ConsEntrenador _ []) = 
cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador _ xs) (ConsEntrenador _ ys) = 


pokemonesDeTipo_De_ :: TipoDePokemon -> Entrenador -> [Pokemon]
pokemonesDeTipo_De_ t (ConsEntrenador _ []) = 0
pokemonesDeTipo_De_ t (ConsEntrenador _ xs) = pokemonesDe_ t xs

pokemonesDe_ :: TipoDePokemon -> [Pokemon] -> [Pokemon]
pokemonesDe_ t [] = []
pokemonesDe_ t (x:xs) = if igualdadDeTipos t (tipo x) then x : pokemonesDe_ t xs else pokemonesDe_ t xs

-- (pokemons filtrados) (pokemons de e2)
-- if esTipoSuperior (tipo x) (tipo y)
-- then  