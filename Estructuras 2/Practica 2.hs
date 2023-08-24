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

entrenador = ConsEntrenador "Kal" [flareon,venasaur,gyarados]
entrenador2 = ConsEntrenador "Kalo" [venasaur]

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
cuantosDeTipo_De_LeGananATodosLosDe_ t e1 e2 = cantidadDePokemonQueLesGananATodos (pokemonesDeTipo_De_ t e1) (listaPokemon e2)


cantidadDePokemonQueLesGananATodos ::  [Pokemon] -> [Pokemon] -> Int
--Indica la cantidad de pokemones de la primer lista que le ganan a todos los pokemon de la segunda lista
cantidadDePokemonQueLesGananATodos [] [] = 0
cantidadDePokemonQueLesGananATodos [] ys = 0
cantidadDePokemonQueLesGananATodos (x:xs) ys = unoSiCeroSino(leGanaATodos x ys) + cantidadDePokemonQueLesGananATodos xs ys


pokemonesDeTipo_De_ :: TipoDePokemon -> Entrenador -> [Pokemon]
--Describe una lista de pokemon del tipo dado
pokemonesDeTipo_De_ t (ConsEntrenador _ []) = []
pokemonesDeTipo_De_ t (ConsEntrenador _ xs) = pokemonesDe_ t xs

pokemonesDe_ :: TipoDePokemon -> [Pokemon] -> [Pokemon]
--Describe la lista de pokemon del tipo dado
pokemonesDe_ t [] = []
pokemonesDe_ t (x:xs) = if igualdadDeTipos t (tipo x) then x : pokemonesDe_ t xs else pokemonesDe_ t xs


leGanaATodos :: Pokemon -> [Pokemon] -> Bool
-- indica si el pokemon le gana a todos los pokemon de la lista
leGanaATodos pk [] = True 
leGanaATodos pk (x:xs) = superaA pk x && leGanaATodos pk xs

superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = esTipoSuperior (tipo p1) (tipo p2)

esTipoSuperior :: TipoDePokemon -> TipoDePokemon -> Bool
esTipoSuperior Agua Fuego = True
esTipoSuperior Fuego Planta = True
esTipoSuperior Planta Agua = True
esTipoSuperior _ _ = False

listaDePokemon_LeGanaATodos :: [Pokemon] -> [Pokemon] -> Bool 
--Indica si todos los pokemon de la primer lista le ganan a todos los pokemon de la segunda lista de pokemon
listaDePokemon_LeGanaATodos [] [] = False
listaDePokemon_LeGanaATodos [] ys = True 
listaDePokemon_LeGanaATodos (x:xs) ys = leGanaATodos x ys && listaDePokemon_LeGanaATodos xs ys

listaPokemon :: Entrenador -> [Pokemon]
--Denota la lista de pokemon de un Entrenador
listaPokemon (ConsEntrenador _ xs) = xs

------------------------------------------------------------------------------------------------------------------
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon e = tieneTodosLosTipos (listaPokemon e)

tieneTodosLosTipos :: [Pokemon] -> Bool
tieneTodosLosTipos [] = False
tieneTodosLosTipos xs = tieneTipoFuego xs && tieneTipoAgua xs && tieneTipoPlanta xs 

tieneTipoFuego :: [Pokemon] -> Bool
tieneTipoFuego [] = False 
tieneTipoFuego (x:xs) = esTipoFuego (tipo x) || tieneTipoFuego xs

esTipoFuego :: TipoDePokemon -> Bool
esTipoFuego Fuego = True
esTipoFuego _ = False 

tieneTipoAgua :: [Pokemon] -> Bool 
tieneTipoAgua [] = False 
tieneTipoAgua (x:xs) = esTipoAgua (tipo x) || tieneTipoAgua xs

esTipoAgua :: TipoDePokemon -> Bool 
esTipoAgua Agua = True
esTipoAgua _ = False 

tieneTipoPlanta :: [Pokemon] -> Bool
tieneTipoPlanta [] = False 
tieneTipoPlanta (x:xs) = esTipoPlanta (tipo x) || tieneTipoPlanta xs

esTipoPlanta :: TipoDePokemon -> Bool 
esTipoPlanta Planta = True 
esTipoPlanta _ = False 

----------------------------------------------------------------------------------------------------------------------
data Seniority = Junior | SemiSenior | Senior deriving Show 
data Proyecto = ConsProyecto String deriving Show 
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto deriving Show 
data Empresa = ConsEmpresa [Rol] deriving Show 

developer = Developer Junior (ConsProyecto "x")
management = Management Senior (ConsProyecto "yt")
empresa = ConsEmpresa [developer,developer,developer,management,management]


proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa xs) = proyectosSinRepetir (proyectosListaRol xs)

proyectoDeRol :: Rol -> Proyecto
proyectoDeRol (Developer _ prj) = prj
proyectoDeRol (Management _ prj) = prj 

proyectosListaRol :: [Rol] -> [Proyecto]
--Dado una lista de roles, devuelve una lista de proyectos
proyectosListaRol [] = []
proyectosListaRol (x:xs) = proyectoDeRol x : proyectosListaRol xs


proyectosSinRepetir :: [Proyecto] -> [Proyecto]
--Dado una lista de proyectos, devuelve una lista de proyectos que no esten repetidos
proyectosSinRepetir [] = []
proyectosSinRepetir (x:xs) = if elProyectoEstaRepetido x xs then proyectosSinRepetir xs else x : proyectosSinRepetir xs 

elProyectoEstaRepetido :: Proyecto -> [Proyecto] -> Bool
--Indica si el proyecto dado esta repetido en la lista de proyectos
elProyectoEstaRepetido x [] = False 
elProyectoEstaRepetido x (y:ys) = nombreProyecto x == nombreProyecto y || elProyectoEstaRepetido x ys

nombreProyecto :: Proyecto -> String
nombreProyecto (ConsProyecto s) = s

----------------------------------------------------------------------------------------------------------------------------

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior e [] = 0
losDevSenior e (x:xs) = cantDevSeniorEn (rolesEmpresa e) x + losDevSenior e xs 

cantDevSeniorEn :: [Rol] -> Proyecto -> Int
cantDevSeniorEn [] _ = 0
cantDevSeniorEn (x:xs) y = if esSenior x && nombreProyecto(proyectoDeRol x) == nombreProyecto   y then 1 + cantDevSeniorEn xs y else cantDevSeniorEn xs y 


esSenior :: Rol -> Bool
esSenior (Developer Senior _ ) = True
esSenior (Management Senior _ ) = True
esSenior _ = False 

rolesEmpresa :: Empresa -> [Rol]
rolesEmpresa (ConsEmpresa xs) = xs 

------------------------------------------------------------------------------------------------------------------------------------------
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn [] _ = 0
cantQueTrabajanEn (x:xs) e = cantidadPersonasDe_TrabajandoEn_ (rolesEmpresa e) x + cantQueTrabajanEn xs e 

cantidadPersonasDe_TrabajandoEn_ :: [Rol] -> Proyecto -> Int
--Indica la cantidad de personas trabajando en el proyecto dado que foman parte de la lista de rol dada
cantidadPersonasDe_TrabajandoEn_ [] _ = 0 
cantidadPersonasDe_TrabajandoEn_ (x:xs) y = if nombreProyecto (proyectoDeRol x) == nombreProyecto y then 1 + cantidadPersonasDe_TrabajandoEn_ xs y  else  cantidadPersonasDe_TrabajandoEn_ xs y

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
--asignadosPorProyecto e = proyectosConSuCantidadDeGente (proyectos e) (rolesEmpresa e)


{-proyectosConSuCantidadDeGente :: [Proyecto] -> [Rol] -> [(Proyecto,Int)]
proyectosConSuCantidadDeGente _  [] = []
proyectosConSuCantidadDeGente [] _ =  []
proyectosConSuCantidadDeGente (x:xs) ys = (x, cantidadPersonasDe_TrabajandoEn_ ys x) : proyectosConSuCantidadDeGente xs ys-}

proyectosConSuCantidadDeGente :: [Rol] -> [(Proyecto,Int)]
proyectosConSuCantidadDeGente [] = []
proyectosConSuCantidadDeGente (x:xs) = if 
        [(proyectoDeRol x, (cantidadPersonasDe_TrabajandoEn_ xs (proyectoDeRol x)) + 1)] ++ proyectosConSuCantidadDeGente xs 


