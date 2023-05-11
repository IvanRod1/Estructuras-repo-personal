module Nave
(naveVacia,tripulantesDe,sectores,conMayorRango,conMasTripulantes,conRango,sectorDe,agregarTripulante)
where
    data Nave = MkN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector, Int)

    {-Inv.Rep = Los Tripulantes que esten en el Set, tambien lo estan en el heap
                Uno sector no se puede repetir
                No puede haber tripulantes iguales
                Un tripulante no puede estar en mas de un sector-}

naveVacia :: [Sector] -> Nave
{-Propósito: Crea una nave con todos esos sectores sin tripulantes.
Precondición: la lista de sectores no está vacía
Costo: O(S log S) siendo S la cantidad de sectores de la lista.-}
tripulantesDe :: Sector -> Nave -> Set Tripulante
sectores :: Nave -> [Sector]
conMayorRango :: Nave -> Tripulante
conMasTripulantes :: Nave -> Sector
conRango :: Rango -> Nave -> Set Tripulante
sectorDe :: Tripulante -> Nave -> Sector
agregarTripulante :: Tripulante -> Sector -> Nave -> Nave


naveVacia ls = MkN (sinLosSectores ls mm) HT maxSector mm --O(S log S) siendo S la cantidad de sectores de la lista

sinLosSectores :: Eq k => [Sector] -> Map k v -> Map k v --(log N)
sinLosSectores [] m = m
sinLosSectores (x:xs) ((y,ys):yss) = if x == y
                                     then sinLosSectores xs (removeM y yss)
                                     else sinLosSectores xs yss

maxSector :: Ord v => Map k v -> (k,Int) --(log N)
{-Precondicion: El map no puede ser vacio-}
maxSector ((k,v):kvs) = if length v > snd(maxSector kvs)
                        then (k,lenght v)
                        else maxSector kvs


tripulantesDe :: Sector -> Nave -> Set Tripulante --(log N)
tripulantesDe s (MkN mm _ _) = lookupM s mm

conMayorRango (MkN _ (H a t1 t2) _) = a --O(1)

conMasTripulantes (MkN _ _ (x,y)) = x --O(1)

conRango r (MkN mm ht (x,y)) = tripulantesDeRango r ht --O(P log P) siendo P la cantidad de tripulantes.

tripulantesDeRango :: Rango -> Heap Tripulante -> Set Tripulante
{-Precondicion : El Heap no puede ser vacio -}
tripulantesDeRango x (H (T _ r) ti td) = if x == r
                                         then addS r (tripulantesDeRango ti ++ tripulantesDeRango td)                          --r : tripulantesDeRango ti ++ tripulantesDeRango td
                                         else tripulantesDeRango ti ++ tripulantesDeRango td


sectorDe (T n r) (MkN mm ht (x,y)) =  busquedaPorSectores
