#include "UFSet.h"

/*
 * UFSet.cpp contiene la implementación de la interfaz para UFSet declarada en UFSet.h. 
 * Deben implementarse las operaciones de acuerdo a la representación elegida para el tipo UFSet.
 */

/* El tipo UFNode* representa:
 *  1. un elemento de un UFSet (o sea, un nodo del árbol que contiene a todos los elementos del conjunto)
 *  2. al conjunto en su totalidad, si el nodo es la raíz del arbol
 *
 *  El nodo tiene un puntero a su elemento asociado en el campo element. 
 *  Deberán agregarse los campos necesarios para completar la representación.
 */
struct UFNode {
   ELEM_TYPE element;
   UFNode* elementPrevio;
   int rank;
};

/*INV.REP

   - Todo UFSet que no apunte a otro UFSet que sea él mismo, se considera como el elemento distinguido.
   - Todo UFSet valido debe reconocer su elemento, su UFSet previo y el rango que tiene, el mismo se representa con un entero mayor o igual a cero.
   - Todo UFSet tiene un representante, ese representante es otro UFSet que tiene como UFSet previo a él mismo.
   - Todo UFSet tiene un rango, el cual representa el alcance máximo conocido por ese UFSet. 
*/


/* 
 * Inicializa el UFSet ufset, cuyo valor asociado será value 
 */
UFSet createUFS(ELEM_TYPE value) {
   UFNode* ufsetN = new UFNode;
   ufsetN->element = value;
   ufsetN->elementPrevio = ufsetN;
   ufsetN->rank= 0;

   return ufsetN;
}

ELEM_TYPE elemUFS(UFSet ufset) {
   // COMPLETAR
   return ufset->element;
}


UFSet ufsetMaximo (UFSet elem)
{
   /*Propósito: Dado un UFSet, devuelve su UFSet máximo. El UFSet maximo es aquel que se tiene a él mismo como "Padre" o UFSet previo*/
   while(elem->elementPrevio != elem)
   {
      elem = elem->elementPrevio;
   }

   return elem;
}

/*
 * Encuentra el elemento distinguido para el UFSet dado. 
 * Esta operación puede ser optimizada con la técnica de compresión de camino.
 */

// VERSION ITERATIVA FIND

UFSet findUFS(UFSet elem)
{
    UFNode* aux = createUFS(elem->element);

   while(elem != elem->elementPrevio)
   {

      aux -> elementPrevio = elem->elementPrevio;

      elem->elementPrevio = ufsetMaximo(elem);

      elem = aux->elementPrevio;
   }


   delete aux;

   return elem;
}


/*UFSet findUFS(UFSet elem)
{
    UFNode* aux = createUFS(elem->element);

   while(elem->elementPrevio != ufsetMaximo(elem))
   {

      aux -> elementPrevio = elem->elementPrevio;

      elem->elementPrevio = ufsetMaximo(elem);

      elem = aux->elementPrevio;
   }


   delete aux;

   return elem;

}*/





// VERSION RECURSIVA FIND
/*
UFSet findUFS(UFSet elem) {


   UFNode* aux = createUFS(elem->element);

   if(elem != elem->elementPrevio)
   {
      aux->elementPrevio = elem->elementPrevio;
      elem->elementPrevio = ufsetMaximo(elem);

      return findUFS(aux->elementPrevio);
   }
   else
   {
      delete aux;

      return elem;
   }


}
*/




/*
 * Calcula la unión entre los conjuntos ufset1 y ufset2. 
 * Esta operación puede ser optimizada con la técnica de unión por rango.
 */

void unionUFS(UFSet ufset1, UFSet ufset2) {

   if(findUFS(ufset1)->rank > findUFS(ufset2)->rank)
   {
      findUFS(ufset2)->elementPrevio = findUFS(ufset1);
      findUFS(ufset1)->rank = findUFS(ufset2)->rank + 1;
   }
   else
   {
      findUFS(ufset1) -> elementPrevio = findUFS(ufset2);
      findUFS(ufset2)->rank = findUFS(ufset1)->rank + 1;
   }

}



/*PRE-OPTIMIZACIÓN 1 // PRE-OPTIMIZACION 2*/

/*

UFSet findUFS(UFSet elem) {

   while(elem->elementPrevio != elem)
   {
      return findUFS(elem->elementPrevio);
   }

   return elem;
}

void unionUFS(UFSet ufset1, UFSet ufset2) {

   findUFS(ufset2) -> elementPrevio = findUFS(ufset1);
}


*/





/////////////////////////////////////////////SIN USAR////////////////////////////////////////////////////////////////////////

/*
int rango(UFSet ufset) // modificar. Aplicar el rango en la estructura UFSet
{
   /*Describe la cantidad de UFSet reccoridos desde el UFSet dado hasta su UFSet distinguido
   
   if((ufset->elementPrevio->element) == ufset->element)
   {
      return 0;
   }
   else
   {
      return 1 + rango(ufset->elementPrevio);
   }
   
}
*/

/*void unionUFS(UFSet ufset1, UFSet ufset2) {

   if(rango(ufset1) > rango(ufset2))
   {
      findUFS(ufset2) -> elementPrevio = findUFS(ufset1);
   }
   else
   {
      findUFS(ufset1) -> elementPrevio = findUFS(ufset2);
   }


}*/
