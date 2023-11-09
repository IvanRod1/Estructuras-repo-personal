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
   // COMPLETAR
   UFNode* elementPrevio; //Todo ufSET sabe cual es su ufset previo y si no tiene un ufSET previo, el mismo será NULL
   //UFNode* elementRepresentante; // Todo ufSET sabe cual es su ufSET representante y ese ufSET nunca podrá ser NULL
};

/* 
 * Inicializa el UFSet ufset, cuyo valor asociado será value 
 */
UFSet createUFS(ELEM_TYPE value) {
   // COMPLETAR
   UFNode* ufsetN = new UFNode;
   ufsetN->element = value;
   ufsetN->elementPrevio = ufsetN;
   //ufsetN->elementRepresentante = ufsetN;

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
UFSet findUFS(UFSet elem) {
  
   // COMPLETAR
   //return elem->elementRepresentante;
   if(elem->elementPrevio->element == elem->element)
   {
      return elem;
   }
   else
   {
      return findUFS(elem->elementPrevio);
   }

}

/*UFSet findUFSi(UFSet elem)
{
   
}*/




int rango(UFSet ufset)
{
   int contador = 0;
   UFNode* ufsetAux = ufset;
   
   while(ufsetAux->elementPrevio->element != ufsetAux->element)
   {
      contador++;
      ufsetAux = ufsetAux->elementPrevio;
   }

   delete ufsetAux;

   return contador;
}

/*
 * Calcula la unión entre los conjuntos ufset1 y ufset2. 
 * Esta operación puede ser optimizada con la técnica de unión por rango.
 */

void unionUFS(UFSet ufset1, UFSet ufset2) {
   // COMPLETAR
   //findUFS(ufset1) -> elementPrevio = findUFS(ufset2);
   //findUFS(ufset1) -> elementPrevio = ufset2;
   //cout << "hola" << endl;
   if(rango(findUFS(ufset1)) > rango(findUFS(ufset2)))
   {
      delete findUFS(ufset2)->elementPrevio;
      findUFS(ufset2)->elementPrevio = findUFS(ufset1);
   }
   else
   {
      delete findUFS(ufset1)->elementPrevio;
      findUFS(ufset1)->elementPrevio = findUFS(ufset2);
   }

}


