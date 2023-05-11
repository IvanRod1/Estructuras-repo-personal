import Nave
import Map


tripulantes :: Nave -> Set Tripulante
tripulantes n = let ls = sectores n
                in listToSet(tripulantesDeSectores ls n)

tripulantesDeSectores :: [Sector] -> Nave -> [Tripulante]
tripulantesDeSectores [] _ = []
tripulantesDeSectores (x:xs) n = (tripulantesDe x n) ++ (tripulantesDeSectores xs n)
    
listToSet :: [a] -> Set a 
listToSet [] = emptyS
listToSet (x:xs) = addS x listToSet xs
    
    --addS(tripulantesDe x n) (tripulantesDeSectores xs n)

