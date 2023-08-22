sucesor :: Int -> Int
sucesor x = x + 1

-----------

sumar :: Int -> Int -> Int
sumar x y = x + y

-----------

divisionYResto :: Int -> Int -> (Int,Int)
divisionYResto x y = (div x y, mod x y)

--------------

maxDelPar :: (Int,Int) -> Int
maxDelPar (x,y) = if x > y
                  then x
                  else y

-------------

--sumar ((sucesor (maxDelPar (divisionYResto 16 2)))) 1
--sucesor (sumar (maxDelPar (divisionYResto 4 2)) (sumar 5 2))
--maxDelPar (divisionYResto (sumar 45 5) (sucesor 4))
--sumar (maxDelPar(divisionYResto 36 6)) (sucesor 3)

--------------

data Dir = Norte | Este | Sur | Oeste deriving Show

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Este = Oeste
opuesto Sur = Norte
opuesto Oeste = Este

iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este = True
iguales Oeste Oeste = True
iguales _ _ = False


siguiente :: Dir -> Dir
--precondicion = La direccion dada debe ser distinta de Oeste
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
------------------------
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving Show

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes,Domingo)

-----------------------
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Miercoles = True
empiezaConM Martes = True
empiezaConM _ = False

------------------------
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues d1 d2 = numeroDeDiaDeLaSemana d1 > numeroDeDiaDeLaSemana d2


numeroDeDiaDeLaSemana :: DiaDeSemana -> Int
numeroDeDiaDeLaSemana Lunes = 1
numeroDeDiaDeLaSemana Martes = 2
numeroDeDiaDeLaSemana Miercoles = 3
numeroDeDiaDeLaSemana Jueves = 4
numeroDeDiaDeLaSemana Viernes = 5
numeroDeDiaDeLaSemana Sabado = 6
numeroDeDiaDeLaSemana Domingo = 7
numeroDeDiaDeLaSemana _ = error "No es un dia"
-------------------------
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio d = (numeroDeDiaDeLaSemana Lunes) < (numeroDeDiaDeLaSemana d)  && (numeroDeDiaDeLaSemana d) < (numeroDeDiaDeLaSemana Domingo)

----------------

negar :: Bool -> Bool
negar True = False
negar _ = True

----------------------------
implica :: Bool -> Bool -> Bool
implica True p = p
implica _ _ = True 

-----------------------
yTambien :: Bool -> Bool -> Bool
yTambien True p = p
yTambien _ _ = False 

-------------------------
oBien :: Bool -> Bool -> Bool
oBien False p = p
oBien _ _ = True 

----------------------------

data Persona = P String Int  deriving Show

sujeto = P "Tom" 21

nombre :: Persona -> String
nombre (P n _) = n

-----------------------------
edad :: Persona -> Int
edad (P _ e) = e

------------------------------

crecer :: Persona -> Persona
crecer (P n e) = P n (sumar e 1)

---------------------------------

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nombreNuevo (P n e) = P nombreNuevo e

-----------------------------------
esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad p1 > edad p2 

-----------------------------------
laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 =  if esMayorQueLaOtra p1 p2
                      then p1
                      else p2

-----------------------------------------
data Pokemon = PKM TipoDePokemon Int deriving Show
data Entrenador = E String Pokemon Pokemon deriving Show
data TipoDePokemon = Agua | Fuego | Planta deriving Show

gyarados = PKM Agua 10000
flareon = PKM Fuego 7800
venasaur = PKM Planta 7600

entrenador = E "polpot" gyarados venasaur

esTipoSuperior :: TipoDePokemon -> TipoDePokemon -> Bool
esTipoSuperior Agua Fuego = True
esTipoSuperior Fuego Planta = True
esTipoSuperior Planta Agua = True
esTipoSuperior _ _ = False

tipo :: Pokemon -> TipoDePokemon
tipo (PKM t _) = t 

superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = esTipoSuperior (tipo p1) (tipo p2)

------------------------------------------
unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino _ = 0

igualdadDeTipos :: TipoDePokemon -> TipoDePokemon -> Bool
igualdadDeTipos Agua Agua = True
igualdadDeTipos Fuego Fuego = True
igualdadDeTipos Planta Planta = True
igualdadDeTipos _ _ = False

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe t (E _ p1 p2) = unoSiCeroSino(igualdadDeTipos t (tipo p1)) + unoSiCeroSino(igualdadDeTipos t (tipo p2))

--------------------------------
juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon ((E _ p1 p2),(E _ p3 p4)) = [p1,p2,p3,p4]

----------------------------------

loMismo :: a -> a
loMismo a = a

-----------------------------

siempreSiete :: a -> Int
siempreSiete a = 7

-------------------------
swap :: (a,b) -> (b, a)
swap (x,y) = (y,x)

------------------------
estaVacia :: [a] -> Bool
estaVacia(x:xs) = False
estaVacia _ = True

-----------------------
elPrimero :: [a] -> a
elPrimero(x:_) = x

-------------------------
sinElPrimero :: [a] -> [a]
sinElPrimero (_:xs) = xs

--------------------------
splitHead :: [a] -> (a, [a])
splitHead ls = (elPrimero ls, sinElPrimero ls)


