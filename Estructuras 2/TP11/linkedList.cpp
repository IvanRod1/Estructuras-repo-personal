#include <iostream>
using namespace std;
#include "linkedList.h"

struct NodoL {
int elem; // valor del nodo
NodoL* siguiente; // puntero al siguiente nodo
};

struct LinkedListSt {
// INV.REP.: cantidad indica la cantidad de nodos que se pueden recorrer
// desde primero por siguiente hasta alcanzar a NULL
int cantidad; // cantidad de elementos
NodoL* primero; // puntero al primer nodo
};

struct IteratorSt {
    NodoL* current;
};



LinkedList nil()
//Crea una lista vacía.
{
    LinkedListSt * llist = new LinkedListSt;
    llist->cantidad = 0;
    llist ->primero = NULL;

    return llist;
}

bool isEmpty(LinkedList xs)
//Indica si la lista está vacía.
{
    return xs->primero == NULL;
}

int head(LinkedList xs)
//Devuelve el primer elemento.
{
    return xs->primero->elem;
}

void Cons(int x, LinkedList xs)
//Agrega un elemento al principio de la lista
{
    NodoL* elemN = new NodoL;
    elemN->elem = x;
    elemN -> siguiente = xs->primero;
    xs->primero = elemN;
    xs->cantidad ++;
}

void Tail(LinkedList xs)
//Quita el primer elemento.
{
    xs->primero = xs->primero->siguiente;
    delete xs->primero;
}

int length(LinkedList xs)
//Devuelve la cantidad de elementos
{
    return xs->cantidad;
}

void Snoc(int x, LinkedList xs)
//Agrega un elemento al final de la lista.
{
    LinkedListSt* auxlist = xs;
    NodoL* elemN = new NodoL;
    
    while(xs->primero->siguiente != NULL)
    {
        xs->primero = xs->primero->siguiente;
    }

    elemN->elem = x;
    elemN->siguiente = NULL;

    xs->primero->siguiente = elemN;

    xs->primero = auxlist->primero;
    xs->cantidad++;

    delete auxlist; 

}

ListIterator getIterator(LinkedList xs)
//Apunta el recorrido al primer elemento.
{
    IteratorSt* i = new IteratorSt;
    i->current = xs->primero;

    return i;
}

int current(ListIterator ixs)
//Devuelve el elemento actual en el recorrido.
{
    return ixs->current->elem;
}

void SetCurrent(int x, ListIterator ixs)
//Reemplaza el elemento actual por otro elemento.
{
    NodoL* elemN = new NodoL;
    elemN->elem = x;
    elemN->siguiente = NULL;

    ixs->current = elemN;

}

void Next(ListIterator ixs)
//Pasa al siguiente elemento.
{
    ixs->current = ixs->current->siguiente;
}

bool atEnd(ListIterator ixs)
//Indica si el recorrido ha terminado.
{
    return ixs->current == NULL;
}

void DisposeIterator(ListIterator ixs)
//Libera la memoria ocupada por el iterador
{
    delete ixs;
}

void DestroyL(LinkedList xs)
//Libera la memoria ocupada por la lista.
{
    delete xs;
}
