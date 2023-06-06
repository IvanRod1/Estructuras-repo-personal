#include <iostream>
using namespace std;
#include "linkedLists.h"

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


void Cons(int x, LinkedList xs)
{
    NodoL* elemN = new NodoL;
    elemN ->elem = x;
    elemN -> siguiente = xs -> primero;
    xs ->primero = elemN;
    xs->cantidad++;
}

int Head (LinkedList xs)
{
    return(xs->primero->elem);
}

void Tail (LinkedList xs)
{
    NodoL* temp = xs ->primero;
    xs -> primero = xs -> primero ->siguiente;
    xs ->cantidad--;
    delete temp;
}
void Snoc(int x, LinkedList xs)
{
    NodoL* elemN = new NodoL;
    elemN -> elem = x;
    elemN -> siguiente = NULL;

    NodoL* temp = xs->primero;

    while(xs->primero->siguiente != NULL)
    {   
        xs->primero = xs->primero->siguiente;
    }
    xs->primero->siguiente = elemN;
    xs->primero = temp;
    delete temp;
    
}

ListIterator getIterator(LinkedList xs)
{
    IteratorSt* iterator = new IteratorSt;
    iterator->current = xs ->primero;
    return iterator;
}

int current(ListIterator ixs)
{
    return ixs ->current->elem;
}

void SetCurrent(int x, ListIterator ixs)
{
    ixs->current->elem = x;
}

void Next(ListIterator ixs)
{
    ixs->current= ixs ->current->siguiente;
}

bool atEnd(ListIterator ixs)
{
    return (ixs->current->siguiente == NULL);
}

void DisposeIterator(ListIterator ixs)
{
    delete (ixs->current);
}

void DestroyL(LinkedList xs)
{
    NodoL* temp = xs->primero;

    while(xs->primero != NULL)
    {
        xs->primero = xs->primero->siguiente;
        delete temp;
        temp = xs->primero;
    }
    delete xs;

}

int sumatoria(LinkedList xs)
{
    int suma = 0;
    ListIterator i = getIterator(xs);
    while(i->current != NULL)//(xs->primero != NULL)
    {
        suma += i->current->elem;
        Next(i);
    }
    return suma;
}

void Sucesores(LinkedList xs)
{
    ListIterator i = getIterator(xs);
    while(xs->primero != NULL)
    {
        i->current->elem++;
        Next(i);

    }
}
