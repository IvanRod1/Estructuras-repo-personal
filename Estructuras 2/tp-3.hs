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