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
   UFNode* elementPrevio; //Todo ufSET sabe cual es su ufset previo y si no tiene un ufSET previo, el mismo será su previo
};

/* 
 * Inicializa el UFSet ufset, cuyo valor asociado será value 
 */
UFSet createUFS(ELEM_TYPE value) {
   // COMPLETAR
   UFNode* ufsetN = new UFNode;
   ufsetN->element = value;
   ufsetN->elementPrevio = ufsetN;

   return ufsetN;
}

ELEM_TYPE elemUFS(UFSet ufset) {
   // COMPLETAR
   return ufset->element;
}

/*
 * Encuentra el elemento distinguido para el UFSet dado. 
 * Esta operación puede ser optimizada con la técnica de compresión de camino.
 */

/*UFSet findUFS(UFSet elem) {

   while(elem->elementPrevio != elem)
   {
      return findUFS(elem->elementPrevio);
   }

   return elem;
}*/

UFSet ufsetMaximo (UFSet elem)
{
   while(elem->elementPrevio != elem)
   {
      return ufsetMaximo(elem->elementPrevio);
   }

   return elem;
}

UFSet findUFS(UFSet elem) {
/*Comprension de camino*/

   UFNode* aux = new UFNode;

   if(elem != elem->elementPrevio)
   {
      aux = elem->elementPrevio;
      elem->elementPrevio = ufsetMaximo(elem);

      return findUFS(aux);
   }
   else
   {
      delete aux;

      return elem;
   }


}




/*UFSet findUFSi(UFSet elem)
{
   
}*/




int rango(UFSet ufset)
{
   
   if((ufset->elementPrevio->element) == ufset->element)
   {
      return 0;
   }
   else
   {
      return 1 + rango(ufset->elementPrevio);
   }
   
}


/*
 * Calcula la unión entre los conjuntos ufset1 y ufset2. 
 * Esta operación puede ser optimizada con la técnica de unión por rango.
 */

void unionUFS(UFSet ufset1, UFSet ufset2) {

   if(rango(ufset1) > rango(ufset2))
   {
      findUFS(ufset2) -> elementPrevio = findUFS(ufset1);
   }
   else
   {
      findUFS(ufset1) -> elementPrevio = findUFS(ufset2);
   }


}


/*PRE-OPTIMIZACIÓN 1*/
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

/*int main()
{
    UFSet nashi = createUFS(crearEquipo(ARGENTINA,"C"));
    UFSet nashe = createUFS(crearEquipo(BRASIL,"A"));
    unionUFS(nashi,nashe);

    cout << rango(nashe);
}*/




