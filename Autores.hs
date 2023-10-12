data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))
{-INV. REP.
    -Toda persona p, perteneciente al valor de mp1, p es clave de mp2
    -Todo checksum c, perteneciente al valor de mp2,es clave de mp1
    --}

agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
{-Propósito: Agrega al organizador un programa con el Checksum indicado; el conjunto es el conjunto de personas autores
de dicho programa.
Precondición: el identificador del programa que se agrega no fue usado previamente en el organizador, y el Set de personas
no está vacío.
Eficiencia: no hay ninguna garantía de eficiencia.-}

agregarPrograma (MkO m1 m2) chksm ps = MkO (assocM cksm ps m1) (agregarListaConValorAMap (setToList ps) chksm m2) --O (log C + p *(log P + log c))

agregarListaConValorAMap :: [Persona] -> Checksum -> Map Persona (Set Checksum) -> Map Persona (Set Checksum)
agregarListaConValorAMap [] _ mp = mp
agregarListaConValorAMap (p:ps) c mp = case lookupm p mp of 
                                       Just _ -> assocM p (addS c cs) (agregarListaConValorAMap ps c mp)
                                       Nothing -> assocM p (addS c emptyS) (agregarListaConValorAMap ps c mp)
                                        --O(n) * O(log P) + O(log c) 
{-
agregarPrograma (MkO m1 m2) chksm ps = MkO (assocM cksm ps m1) (agregarListaConValorAMap ps chksm m2) 
-}

---------------------------------------------------------------------------------------------------------------------------

programaronJuntas :: Organizador -> Persona -> Persona -> Bool
{-Propósito: dado un organizador y dos personas, denota verdadero si ambas son autores de algún software en común.
Precondición: las personas deben ser distintas.
Eficiencia: O(log P + C log C) en peor caso, donde P es la cantidad de personas distintas que aparecen en todos los
programas del organizador, y C la cantidad total de programas.-}

programaronJuntas (MkO m1 m2) p1 p2 = if p1 == p2 then error " " else let 

{-log k , siendo k -}
lookupSet :: (Eq k , Eq a) => k -> Map k (Set a) -> Set a
lookupSet x m = case lookupm x m of Just s -> s Nothing -> emptyS
{-log P-}
programasDe :: Organizador -> Persona -> Set Checksum



---------------------------------------------------------------------------------------------------------------
{- log P(programasdE) + c log c (interseccion sobre sets de checksum)-}
programasEnComun p1 p2 o = intersection (programasDe o p1) (programasDe o p2)


