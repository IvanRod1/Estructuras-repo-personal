#include <iostream>
#include "linkedList.h"
using namespace std;

int sumatoria(LinkedList xs)
//Devuelve la suma de todos los elementos.
{
    int acumulador = 0;
    ListIterator i = getIterator(xs);
    while(!(atEnd(i)))
    {
        acumulador += current(i);
        Next(i);
    }
    DisposeIterator(i);
    return acumulador;
}

void Sucesores(LinkedList xs)
//Incrementa en uno todos los elementos
{
    ListIterator i = getIterator(xs);
    while(!(atEnd(i)))
    {
        Snoc(current(i) + 1,xs);
        Tail(xs);
        Next(i);
    
    }
    DisposeIterator(i);
}

bool pertenece(int x, LinkedList xs)
//Indica si el elemento pertenece a la lista.
{
    
}




int main()
{
    LinkedList lista = nil();
    Cons(1,lista);
    Cons(2,lista);
    cout << head(lista) << endl;
}