#include <iostream>
using namespace std;
struct LinkedListSt;
struct NodoL;
struct IteratorSt;

typedef LinkedListSt* LinkedList;
typedef IteratorSt* ListIterator;

void Snoc(int x, LinkedList xs);
void Tail (LinkedList xs);
int Head (LinkedList xs);
ListIterator getIterator(LinkedList xs);
int current(ListIterator ixs);
void SetCurrent(int x, ListIterator ixs);
void Next(ListIterator ixs);
bool atEnd(ListIterator ixs);
void DisposeIterator(ListIterator ixs);
void DestroyL(LinkedList xs);
//-----------------
 int sumatoria(LinkedList xs);
 void Sucesores(LinkedList xs);
