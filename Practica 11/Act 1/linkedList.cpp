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


LinkedList nil()
{
  LinkedListSt* listEmpty = new LinkedListSt;
  listEmpty -> cantidad = 0;
  listEmpty -> primero = NULL;
  
  return listEmpty;
}

bool isEmpty(LinkedList xs)
{
   return(xs->cantidad == 0);
}

void Cons(int x, LinkedList xs)
{
  NodoL* elemN = new NodoL;
  elemN -> elem = x;
  elemN -> siguiente = xs -> primero;
  xs -> primero = elemN;
  xs -> cantidad++;
  
}

int head (LinkedList xs)
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
    NodoL* current = xs->primero;

    NodoL* temp = xs->primero;

    /*while(xs->primero->siguiente != NULL)
    {   
        xs->primero = xs->primero->siguiente;
    }*/

    /*for (int i = 0; i < xs->cantidad; i++)
    {
        xs->primero = xs->primero->siguiente;
    }*/

    while(current != NULL)
    {   
        xs->primero = xs->primero->siguiente;
        current= current ->siguiente;
    }

    
    xs->primero->siguiente = elemN;
    xs->primero = temp;
    xs->cantidad++;
    delete temp;
    
}

int length(LinkedList xs)
{
  return (xs-> cantidad);
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
    ixs->current= ixs->current->siguiente;
}

bool atEnd(ListIterator ixs)
{
    return (ixs->current == NULL);
}

void DisposeIterator(ListIterator ixs)
{
    delete ixs;
}

void DestroyL(LinkedList xs)
{
    NodoL* temp = xs->primero;

    while(xs->primero->siguiente != NULL)
    {
        xs->primero = xs->primero->siguiente;
        delete temp;
        temp = xs->primero;
    }
    delete xs;

}
/*--------------------------------------------------*/
int sumatoria(LinkedList xs)
{
    int suma = 0;
    ListIterator i = getIterator(xs);
    while(i->current->siguiente != NULL)//(xs->primero != NULL)
    {
        suma += i->current->elem;
        Next(i);
    }
    return suma;
}

void Sucesores(LinkedList xs)
{
    ListIterator i = getIterator(xs);
    while(xs->primero->siguiente != NULL)
    {
        i->current->elem++;
        Next(i);

    }
}

bool pertenece(int x, LinkedList xs)
{
    ListIterator i = getIterator(xs);
    while(i->current->elem != x || i->current->siguiente != NULL)
    {
        Next(i);
    }
    return(i->current->elem == x);
}

int apariciones(int x, LinkedList xs)
{
    int contador = 0;
    ListIterator i = getIterator(xs);
    while (!(atEnd(i)))
    {
        if(i->current->elem == x)
        {
            contador++;
        }
        Next(i);
    }
    return contador;
}

int minimo(LinkedList xs)
{
    int min = xs->primero->elem;
    ListIterator it = getIterator(xs);
    Next(it);
    for (int i = 0; i <= xs->cantidad; i++)
    {
        if(it->current->elem < min)
        {
            min = it->current->elem;
        }
        Next(it);
    }
    
    return(min);
}

int main()
{
    LinkedListSt* list = new LinkedListSt;
    
    Cons(5,list);
    Cons(20,list);
    Snoc(1,list);

    ListIterator i = getIterator(list);
    Next(i);
    cout << minimo(list);
    return 0;
}


